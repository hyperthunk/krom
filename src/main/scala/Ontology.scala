package org.nebularis.krom

import com.typesafe.scalalogging.LazyLogging
import org.nebularis.krom.DataType
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.{DefaultPrefixManager, SimpleIRIMapper}

import java.io.OutputStream
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.language.postfixOps;

// TODO: we can make huge type safety improvements here with SYB

class Ontology private (val config: KromConfig, val ontology: OWLOntology,
                        val manager: OWLOntologyManager) extends LazyLogging {

    private val counter = new AtomicLong(Integer.MAX_VALUE)
    private val morkIRI = IRI.create(config.morkURL)
    private val morkBaseIRI = IRI.create(config.morkBaseIRI)
    private val skosBaseIRI = IRI.create(config.skosBaseIRI)
    private val anonymousEntities: mutable.Set[String] = mutable.HashSet.empty

    private lazy val factory: OWLDataFactory = manager.getOWLDataFactory
    private lazy val morkPrefixManager = new DefaultPrefixManager(null, null, config.morkBaseIRI + "#")
    private lazy val mappingPrefixManager = new DefaultPrefixManager(null, null, config.kromBaseIRI + "#")
    private lazy val morkRepScheme: OWLClass = getMorkClass(":RepresentationScheme")
    private lazy val morkRepSchemeDef: OWLClassExpression = morkRepScheme.getNNF
    private lazy val morkDCScheme: OWLClass = getMorkClass(":TaxonomyScheme")
    private lazy val morkDCSchemeDef: OWLClassExpression = morkDCScheme.getNNF
    private lazy val morkAttribute: OWLClass = getMorkClass(":Attribute")
    private lazy val morkAttributeDef: OWLClassExpression = morkAttribute.getNNF
    private lazy val morkAnonEntity: OWLClass = getMorkClass(":AnonymousEntity")
    private lazy val morkAnonEntityDef: OWLClassExpression = morkAnonEntity.getNNF
    private lazy val morkEntity: OWLClass = getMorkClass(":Entity")
    private lazy val morkEntityDef: OWLClassExpression = morkEntity.getNNF
    private lazy val morkArray: OWLClass = getMorkClass(":Array")
    private lazy val morkArrayDef: OWLClassExpression = morkArray.getNNF
    private lazy val morkConcept: OWLClass = getMorkClass(":DataConcept")
    private lazy val morkConceptDef: OWLClassExpression = morkConcept.getNNF
    private lazy val morkObjectification: OWLClass = getMorkClass(":Objectification")
    private lazy val morkObjectificationDef: OWLClassExpression = morkObjectification.getNNF

    private lazy val representationSchemeID: String = scheme("Representation")
    private lazy val taxaSchemeID: String = scheme("Taxa")
    private lazy val repScheme: OWLNamedIndividual = getPrefixedNamedIndividual(representationSchemeID)
    private lazy val conceptScheme: OWLNamedIndividual = getPrefixedNamedIndividual(taxaSchemeID)
    private lazy val bottom: OWLNamedIndividual = factory.getOWLNamedIndividual(":Bottom", morkPrefixManager)

    private lazy val morkAssociativeBroadConceptRoleIRI =
        factory.getOWLObjectProperty(morkAssociativeBroadConceptRole, morkPrefixManager).getIRI
    private lazy val morkAssociativeNarrowConceptRoleIRI =
        factory.getOWLObjectProperty(morkAssociativeNarrowConceptRole, morkPrefixManager).getIRI

    private val morkMemberPropertyLabel = ":memberProperty"
    private val morkAssociativeBroadConceptRole = ":broadConceptRole"
    private val morkAssociativeNarrowConceptRole = ":narrowConceptRole"

    private def scheme(suffix: String): String = config.kromBaseIRI.split("/").last + "_" + suffix

    private enum ConceptScheme(val node: OWLNamedIndividual, val prop: String):

        def assertInScheme(individual: OWLNamedIndividual): Unit = {
            this match {
                case NoScheme => ()
                case _ => assertObjectProperty(individual, prop, node)
            }
        }

        def prefixed(ident: String): String = {
            val id = ident.capitalize
            this match {
                case RepresentationScheme =>
                    identifier(config.kromRepPrefix, config.kromRepSuffix, id).getOrElse("Rep_" + id)
                case TaxonomyScheme =>
                    identifier(config.kromTaxaPrefix, config.kromTaxaSuffix, id).getOrElse("Taxa_" + id)
                case NoScheme => ident
            }
        }

        private def identifier(pfx: Option[String],
                               sfx: Option[String], it: String): Option[String] = (pfx, sfx) match {
            case (None, None) => Some(it)
            case (Some(prefix), None) => if it.startsWith(prefix) then Some(it) else Some(prefix + it)
            case (None, Some(suffix)) => if it.endsWith(suffix) then Some(it) else Some(it + suffix)
            case (Some(prefix), Some(suffix)) =>
                if it.startsWith(prefix) && it.endsWith(suffix) then Some(it)
                else Some(prefix + it + suffix)
        }

        case RepresentationScheme extends ConceptScheme(repScheme, ":representationScheme")
        case TaxonomyScheme extends ConceptScheme(conceptScheme, ":conceptScheme")
        case NoScheme extends ConceptScheme(bottom, ":intransitiveExactMatch")

    private def getMorkClass(shortName: String) = {
        factory.getOWLClass(shortName, morkPrefixManager)
    }

    private def init(): Unit = {
        val iriMapper = new SimpleIRIMapper(morkBaseIRI, morkIRI)
        manager.getIRIMappers.add(iriMapper)
        manager.loadOntology(morkBaseIRI)

        val skosImport = factory.getOWLImportsDeclaration(skosBaseIRI)
        manager.applyChange(new AddImport(ontology, skosImport))

        val morkImport = factory.getOWLImportsDeclaration(morkBaseIRI)
        manager.applyChange(new AddImport(ontology, morkImport))

        val noScheme = ConceptScheme.NoScheme
        val rep = assertIndividual(representationSchemeID, morkRepSchemeDef,
                                   skipIdent = true, noPrefix = true, scheme = noScheme)
        assertSkosNote("Representation Concept Scheme", rep)

        val taxa = assertIndividual(taxaSchemeID, morkDCSchemeDef,
                                    skipIdent = true, noPrefix = true, scheme = noScheme)
        assertSkosNote("Taxonomy Concept Scheme", taxa)

        logger.whenDebugEnabled {
            logger.debug("Imports Closure")
            ontology.getAxioms(Imports.INCLUDED).forEach { (ax: OWLAxiom) =>
                println(ax)
            }
            logger.debug("T-Box (Imports Closure)")
            ontology.getTBoxAxioms(Imports.INCLUDED).forEach { (ax: OWLAxiom) =>
                println(ax)
            }
            logger.debug("CLASS ASSERTION AXIOMS")
            ontology.getAxioms(AxiomType.DECLARATION).forEach { (ax: OWLAxiom) =>
                ax.getNestedClassExpressions.forEach { (cls: OWLClassExpression) =>
                    val oCls = cls.asOWLClass()
                    logger.debug("toStringID: " + oCls.toStringID +
                                 ", toString: " + oCls.toString +
                                 ", IRI: " + oCls.getIRI)
                }
            }
        }
    }

    def print(): Unit = {
        ontology.getABoxAxioms(Imports.EXCLUDED).forEach { (ax: OWLAxiom) =>
            println("non-imports ABOX closure axiom: " + ax)
        }
        ontology.getRBoxAxioms(Imports.EXCLUDED).forEach { (ax: OWLAxiom) =>
            println("non-imports RBOX closure axiom: " + ax)
        }
    }

    def save(): Unit = {
        config.withOutputStream { (ioS: OutputStream) =>
            ontology.saveOntology(config.outputFormat, ioS)
            this
        }
    }

    def addMemberProperty(broader: String, narrower: String): Unit = {
        val child = getNamedIndividual(narrower)
        val (subj, obj) = assertObjectProperty(broader, morkMemberPropertyLabel, child)

        // properties are inherent data concepts that must be reified
        objectifyAssociation(broader.capitalize + "_" + narrower.capitalize, subj, obj)
    }

    def addRepEntityAnon(to: String): String = {
        if anonymousEntities.contains(to) then to
        else
            val id = anonID
            anonymousEntities += id
            val individual = assertIndividual(id, morkAnonEntityDef, noIdent = true)
            assertObjectProperty(to, ":elementType", individual)
            objectifyAssociation(to.capitalize + "_" + id.capitalize, getNamedIndividual(to), individual)
            id
        // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
    }

    def objectifyAssociation(id: String, subj: OWLIndividual, obj: OWLIndividual): Unit = {
        val objFact = assertIndividual(id, morkObjectificationDef,
            skipIdent = true, scheme = ConceptScheme.TaxonomyScheme)
        assertObjectProperty(objFact, morkAssociativeBroadConceptRole, subj)
        assertObjectProperty(objFact, morkAssociativeNarrowConceptRole, obj)

        assertSkosNote("Models an association between DataConcepts, created-by krom", objFact)
        assertRdfsSeeAlso(morkAssociativeBroadConceptRoleIRI, objFact)
        assertRdfsSeeAlso(morkAssociativeNarrowConceptRoleIRI, objFact)
    }

    def addRepresentationEntity(id: String): Unit = {
        val entity = assertIndividual(id, morkEntityDef)
        assertClass(entity, morkConceptDef)
        ConceptScheme.TaxonomyScheme.assertInScheme(entity)
    }

    def addRepresentationArray(id: String): Unit = assertIndividual(id, morkArrayDef)

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit = {
        val individual = assertIndividual(what, morkAttributeDef)
        assertObjectProperty(where, morkMemberPropertyLabel, individual)
        assertClass(individual, morkConceptDef)
        ConceptScheme.TaxonomyScheme.assertInScheme(individual)

        val propIRI = assertDataPropertyRange(what, underlying.xsdType)
        assertRdfsSeeAlso(propIRI, individual)
    }

    private def assertDataPropertyRange(id: String, dataType: DataType): IRI = {
        val identProp = factory.getOWLDataProperty(":externalPropertyValue", morkPrefixManager)
        val dpEx = factory.getOWLDataProperty(":ex_prop_" + id, mappingPrefixManager)
        val subPropAxiom = factory.getOWLSubDataPropertyOfAxiom(dpEx, identProp)
        val rangeAxiom = factory.getOWLDataPropertyRangeAxiom(dpEx, dataType.toOwlDataType(factory))

        manager.applyChange(AddAxiom(ontology, subPropAxiom))
        manager.applyChange(AddAxiom(ontology, rangeAxiom))
        dpEx.getIRI
    }

    private def assertRdfsSeeAlso(otherIRI: IRI, individual: OWLNamedIndividual): Unit = {
        val annotation = factory.getOWLAnnotation(factory.getRDFSSeeAlso, otherIRI)
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)

        manager.applyChange(AddAxiom(ontology, annotationAxiom))
    }

    private def identifyIndividual(id: String, individual: OWLNamedIndividual): OWLNamedIndividual = {
        val identProp = factory.getOWLDataProperty(":identifier", morkPrefixManager)
        val propAssertion = factory.getOWLDataPropertyAssertionAxiom(identProp, individual, id)

        manager.applyChange(AddAxiom(ontology, propAssertion))
        individual
    }

    private def assertObjectProperty(subject: String, propertyName: String,
                                     obj: OWLIndividual): (sub: OWLIndividual, obj: OWLIndividual) = {
        assertObjectProperty(getNamedIndividual(subject), propertyName, obj)
    }

    private def assertObjectProperty(subject: OWLIndividual, propertyName: String,
                                     obj: OWLIndividual): (sub: OWLIndividual, obj: OWLIndividual) = {
        val expr = factory.getOWLObjectProperty(propertyName, morkPrefixManager)
        val propAssertion = factory.getOWLObjectPropertyAssertionAxiom(expr, subject, obj)

        manager.applyChange(AddAxiom(ontology, propAssertion))
        (subject, obj)
    }

    private def assertIndividual(id: String,
                                 ofClass: OWLClassExpression,
                                 skipIdent: Boolean = false,
                                 noIdent: Boolean = false,
                                 noPrefix: Boolean = false,
                                 scheme: ConceptScheme = ConceptScheme.RepresentationScheme): OWLNamedIndividual = {
        val individual = if noPrefix then getPrefixedNamedIndividual(id) else getNamedIndividual(id, scheme)
        assertClass(individual, ofClass)

        val indiv = (skipIdent, noIdent) match {
            case (true, _) => individual
            case (false, true) => identifyIndividual("", assertSkosRepNote(id, individual, scheme))
            case (_, false) => identifyIndividual(id, assertSkosRepNote(id, individual, scheme))
        }
        scheme.assertInScheme(indiv)
        indiv
    }

    private def assertClass(individual: OWLNamedIndividual, ofClass: OWLClassExpression): Unit = {
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)
        manager.applyChange(AddAxiom(ontology, clsAssertion))
    }

    private def getPrefixedNamedIndividual(id: String): OWLNamedIndividual = {
        factory.getOWLNamedIndividual(":" + id, mappingPrefixManager)
    }

    private def getNamedIndividual(id: String,
                                   scheme: ConceptScheme = ConceptScheme.RepresentationScheme): OWLNamedIndividual =
        getPrefixedNamedIndividual(scheme.prefixed(id))

    private def assertSkosRepNote(id: String, individual: OWLNamedIndividual,
                                  scheme: ConceptScheme): OWLNamedIndividual =
        val note = "Individual Representation " + scheme.prefixed(id) + ", created-by krom"
        assertSkosNote(note, individual)

    private def assertSkosNote(note: String, individual: OWLNamedIndividual): OWLNamedIndividual = {
        val skosNote = factory.getOWLAnnotationProperty(config.skosBaseIRI + "#note")
        val annotation = factory.getOWLAnnotation(skosNote, factory.getOWLLiteral(note))
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)
        manager.applyChange(AddAxiom(ontology, annotationAxiom))
        individual
    }

    // TODO: SWRL (or SHACL) axioms to correlate anonymous members that share the same type def
    private def anonID: String =
        ConceptScheme.RepresentationScheme.prefixed("Anon_" + counter.incrementAndGet().toString)

}

object Ontology {
    def openOntology(config: KromConfig): Ontology = {
        val mgr = OWLManager.createOWLOntologyManager()
        val ontology = mgr.createOntology(IRI.create(config.kromBaseIRI))
        val ont = new Ontology(config, ontology, mgr)
        ont.init()
        ont
    }
}

package org.nebularis.krom

import com.typesafe.scalalogging.LazyLogging
import org.nebularis.krom.DataType
import org.nebularis.krom.Ontology.*
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
    private lazy val morkAnonEntity: OWLClass = getMorkClass(":AnonymousElement")
    private lazy val morkAnonEntityDef: OWLClassExpression = morkAnonEntity.getNNF
    private lazy val morkCollectionElem: OWLClass = getMorkClass(":ScalarValueElement")
    private lazy val morkCollectionElemDef: OWLClassExpression = morkCollectionElem.getNNF
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
        factory.getOWLObjectProperty(morkBroadConceptRoleKey, morkPrefixManager).getIRI
    private lazy val morkAssociativeNarrowConceptRoleIRI =
        factory.getOWLObjectProperty(morkNarrowConceptRoleKey, morkPrefixManager).getIRI

    private def scheme(suffix: String): String = shortName(config.kromBaseIRI) + "_" + suffix

    private def shortName(iri: String) = iri.split("/").last

    private enum ConceptScheme(val node: OWLNamedIndividual, val prop: String):

        def assertInScheme(individual: OWLNamedIndividual): Unit = this match {
            case NoScheme => ()
            case _ => assertObjectProperty(individual, prop, node)
        }

        def prefixed(ident: String): String =
            val id = ident.capitalize
            this match {
                case RepresentationScheme =>
                    identifier(config.kromRepPrefix, config.kromRepSuffix, id).getOrElse("Rep_" + id)
                case TaxonomyScheme =>
                    identifier(config.kromTaxaPrefix, config.kromTaxaSuffix, id).getOrElse("Taxa_" + id)
                case NoScheme => ident
            }

        private def identifier(pfx: Option[String],
                               sfx: Option[String], it: String): Option[String] = (pfx, sfx) match {
            case (None, None) => Some(it)
            case (Some(prefix), None) =>
                nonEmptyAffix(_.startsWith(prefix), prefix.concat.apply andThen Some.apply)(it)
            case (None, Some(suffix)) =>
                nonEmptyAffix(_.endsWith(suffix), s => Some(s.concat(suffix)))(it)
            case (Some(prefix), Some(suffix)) =>
                if it.startsWith(prefix) && it.endsWith(suffix) then Some(it)
                else Some(prefix.concat(it).concat(suffix))
        }

        private def nonEmptyAffix(pred: String => Boolean, mk: String => Option[String])
                                 (s: String): Option[String] =
            if pred(s) then Some(s) else mk(s)

        case RepresentationScheme extends ConceptScheme(repScheme, ":representationScheme")
        case TaxonomyScheme extends ConceptScheme(conceptScheme, ":conceptScheme")
        case NoScheme extends ConceptScheme(bottom, ":intransitiveExactMatch")

    private def getMorkClass(shortName: String) = factory.getOWLClass(shortName, morkPrefixManager)

    private def init(): Unit =
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

    def print(): Unit =
        ontology.getABoxAxioms(Imports.EXCLUDED).forEach { (ax: OWLAxiom) =>
            println("non-imports ABOX closure axiom: " + ax)
        }
        ontology.getRBoxAxioms(Imports.EXCLUDED).forEach { (ax: OWLAxiom) =>
            println("non-imports RBOX closure axiom: " + ax)
        }

    def save(): Unit =
        config.withOutputStream { (ioS: OutputStream) =>
            ontology.saveOntology(config.outputFormat, ioS)
            this
        }

    def addMemberProperty(broader: String, narrower: String): Unit =
        val child = getNamedIndividual(narrower)
        val (subj, obj) = assertObjectProperty(broader, morkMemberPropertyKey, child)
        objectifyAssociation(broader, subj, narrower, obj)

    def addRepEntityAnon(to: String): String =
        if anonymousEntities.contains(to) then to
        else
            // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
            val id = anonID
            anonymousEntities += id
            val individual = assertIndividual(id, morkAnonEntityDef, noIdent = true)
            assertObjectProperty(to, morkElementTypeKey, individual)
            objectifyAssociation(to, getNamedIndividual(to), id, individual)
            id

    def addCollectionElement(to: String, dataType: DataType): Unit =
        val id = to.concat("_Element").concat(counter.incrementAndGet().toString)
        val individual = assertIndividual(id, morkCollectionElemDef)
        assertObjectProperty(to, morkElementTypeKey, individual)
        val propIRI = assertDataPropertyRange(id, morkCollectionElementScalarValueKey, dataType)
        assertObjectProperty(propIRI.getIRIString, morkRepresentedAsKey, individual)
        assertRdfsSeeAlso(propIRI, individual)

    def addRepresentationEntity(id: String): Unit =
        val individual = assertIndividual(id, morkEntityDef)
        assertClass(individual, morkConceptDef)
        ConceptScheme.TaxonomyScheme.assertInScheme(individual)
        assertObjectProperty(individual, morkRepresentationOfKey, individual)

    def addRepresentationArray(id: String): Unit =
        val individual = assertIndividual(id, morkArrayDef)
        assertClass(individual, morkConceptDef)
        assertObjectProperty(individual, morkRepresentationOfKey, individual)
        assertDatatypeProperty(individual, morkOrderedItemsKey, Scalar(false))

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit =
        val individual = assertIndividual(what, morkAttributeDef)
        assertObjectProperty(where, morkMemberPropertyKey, individual)
        assertClass(individual, morkConceptDef)
        ConceptScheme.TaxonomyScheme.assertInScheme(individual)

        val propIRI = assertDataPropertyRange(what, underlying.xsdType)
        assertObjectProperty(propIRI.getIRIString, morkRepresentedAsKey, individual)
        assertRdfsSeeAlso(propIRI, individual)

    // properties are inherent data concepts that must be reified
    private def objectifyAssociation(subjName: String,
                                     subj: OWLNamedIndividual,
                                     objName: String,
                                     obj: OWLNamedIndividual): Unit =
        val id = subjName.capitalize + "_" + objName.capitalize
        val objFact: OWLNamedIndividual =
            assertIndividual(id, morkObjectificationDef,
                             skipIdent = true, scheme = ConceptScheme.TaxonomyScheme)
        assertObjectProperty(objFact, morkBroadConceptRoleKey, subj)
        assertObjectProperty(objFact, morkNarrowConceptRoleKey, obj)

        val subjectID = ConceptScheme.RepresentationScheme.prefixed(subjName)
        val txt = s"Models an association between $subjectID and $objName, created-by krom"
        assertSkosNote(txt, objFact)
        assertRdfsSeeAlso(morkAssociativeBroadConceptRoleIRI, objFact)
        assertRdfsSeeAlso(morkAssociativeNarrowConceptRoleIRI, objFact)

    private def assertDataPropertyRange(id: String, dataType: DataType): IRI =
        assertDataPropertyRange(":ex_prop_" + id, morkExternalPropertyValueKey, dataType)

    private def assertDataPropertyRange(id: String, propKey: String, dataType: DataType): IRI =
        val identProp = factory.getOWLDataProperty(propKey, morkPrefixManager)
        val dpEx = factory.getOWLDataProperty(id, mappingPrefixManager)
        val subPropAxiom = factory.getOWLSubDataPropertyOfAxiom(dpEx, identProp)
        val rangeAxiom = factory.getOWLDataPropertyRangeAxiom(dpEx, dataType.toOwlDataType(factory))

        manager.applyChange(AddAxiom(ontology, subPropAxiom))
        manager.applyChange(AddAxiom(ontology, rangeAxiom))
        dpEx.getIRI

    private def identifyIndividual(id: String, individual: OWLNamedIndividual): OWLNamedIndividual =
        assertDatatypeProperty(individual, morkIdentifierPropertyKey, Scalar(id))

    private def assertObjectProperty(subject: String, propertyName: String,
                                     obj: OWLNamedIndividual): (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        assertObjectProperty(getNamedIndividual(subject), propertyName, obj)

    private def assertObjectProperty(subject: OWLNamedIndividual, propertyName: String,
                                     obj: OWLNamedIndividual): (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        val expr = factory.getOWLObjectProperty(propertyName, morkPrefixManager)
        val propAssertion = factory.getOWLObjectPropertyAssertionAxiom(expr, subject, obj)

        manager.applyChange(AddAxiom(ontology, propAssertion))
        (subject, obj)

    private def assertDatatypeProperty(individual: OWLNamedIndividual,
                                       propertyName: String,
                                       scalar: Scalar): OWLNamedIndividual =
        val propAssertion = scalar.assertDatatypeProperty(propertyName, individual, factory, morkPrefixManager)
        propAssertion match {
            case None => individual
            case Some(prop) => manager.applyChange(AddAxiom(ontology, prop)); individual
        }

    private def assertIndividual(id: String,
                                 ofClass: OWLClassExpression,
                                 skipIdent: Boolean = false,
                                 noIdent: Boolean = false,
                                 noPrefix: Boolean = false,
                                 scheme: ConceptScheme = ConceptScheme.RepresentationScheme): OWLNamedIndividual =

        val individual = if noPrefix then getPrefixedNamedIndividual(id) else getNamedIndividual(id, scheme)
        assertClass(individual, ofClass)

        val identifiedIndividual = (skipIdent, noIdent) match {
            case (true, _) => individual
            case (false, true) => identifyIndividual("", assertSkosRepNote(id, individual, scheme))
            case (_, false) => identifyIndividual(id, assertSkosRepNote(id, individual, scheme))
        }
        scheme.assertInScheme(identifiedIndividual)
        identifiedIndividual

    private def assertClass(individual: OWLNamedIndividual, ofClass: OWLClassExpression): Unit =
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)
        manager.applyChange(AddAxiom(ontology, clsAssertion))

    private def getPrefixedNamedIndividual(id: String): OWLNamedIndividual =
        factory.getOWLNamedIndividual(":" + id, mappingPrefixManager)

    private def getNamedIndividual(id: String,
                                   scheme: ConceptScheme = ConceptScheme.RepresentationScheme): OWLNamedIndividual =
        getPrefixedNamedIndividual(scheme.prefixed(id))

    // management of notes and comments

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

    private def assertRdfsSeeAlso(otherIRI: IRI, individual: OWLNamedIndividual): Unit = {
        val annotation = factory.getOWLAnnotation(factory.getRDFSSeeAlso, otherIRI)
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)

        manager.applyChange(AddAxiom(ontology, annotationAxiom))
    }

    // TODO: SWRL (or SHACL) axioms to correlate anonymous members that share the same type def
    private def anonID: String =
        ConceptScheme.RepresentationScheme.prefixed("Anon_" + counter.incrementAndGet().toString)

}

object Ontology {
    private val morkIdentifierPropertyKey = ":identifier"
    private val morkMemberPropertyKey = ":memberProperty"
    private val morkElementTypeKey = ":elementType"
    private val morkOrderedItemsKey = ":orderedItems"
    private val morkBroadConceptRoleKey = ":broadConceptRole"
    private val morkNarrowConceptRoleKey = ":narrowConceptRole"
    private val morkRepresentedAsKey = ":representedAs"
    private val morkRepresentationOfKey = ":representationOf"
    private val morkExternalPropertyValueKey = ":externalPropertyValue"
    private val morkCollectionElementScalarValueKey = ":collectionElementScalarValue"

    def openOntology(config: KromConfig): Ontology = {
        val mgr = OWLManager.createOWLOntologyManager()
        val ontology = mgr.createOntology(IRI.create(config.kromBaseIRI))
        val ont = new Ontology(config, ontology, mgr)
        ont.init()
        ont
    }
}

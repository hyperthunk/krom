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

trait Initialized(val dataFactory: OWLDataFactory) extends LazyLogging {

    def getOwlClass(shortName: String, prefixManager: PrefixManager): OWLClass =
        dataFactory.getOWLClass(shortName, prefixManager)

    def getClassExpression(shortName: String, prefixManager: PrefixManager): OWLClassExpression =
        getOwlClass(shortName, prefixManager).getNNF
}

trait IndividualSearch(final val factory: OWLDataFactory, final val prefixManager: PrefixManager) {
    def getPrefixedNamedIndividual(id: String): OWLNamedIndividual =
        factory.getOWLNamedIndividual(":" + id, prefixManager)
}

class PropertyFactory(final val ontology: OWLOntology,
                      final val manager: OWLOntologyManager,
                      prefixManager: PrefixManager)
    extends IndividualSearch(manager.getOWLDataFactory, prefixManager) {

    def assertObjectProperty(subject: OWLNamedIndividual, propertyName: String,
                             obj: OWLNamedIndividual): (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        val factory = manager.getOWLDataFactory
        val expr = factory.getOWLObjectProperty(propertyName, prefixManager)
        val propAssertion = factory.getOWLObjectPropertyAssertionAxiom(expr, subject, obj)

        manager.applyChange(AddAxiom(ontology, propAssertion))
        (subject, obj)
}

trait Scheme {
    def assertInScheme(individual: OWLNamedIndividual, propertyFactory: PropertyFactory): Unit
    def prefixed(ident: String): String
}

abstract class ConceptScheme(final val schemeID: String,
                             final val schemeProperty: String,
                             final val prefix: Option[String],
                             final val suffix: Option[String],
                             final val defaultPrefix: String,
                             final val ontology: OWLOntology,
                             final val manager: OWLOntologyManager,
                             pm: PrefixManager)
    extends IndividualSearch(manager.getOWLDataFactory, pm), Scheme:

    def assertInScheme(individual: OWLNamedIndividual, propertyFactory: PropertyFactory): Unit =
        propertyFactory.assertObjectProperty(individual, schemeProperty,
            super.getPrefixedNamedIndividual(schemeID))

    def getNamedIndividual(id: String): OWLNamedIndividual =
        super.getPrefixedNamedIndividual(prefixed(id))

    def prefixed(ident: String): String =
        val id = ident.capitalize
        identifier(prefix, suffix, id).getOrElse(defaultPrefix + id)

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

case class RepresentationScheme(sid: String,
                                sp: String,
                                config: KromConfig,
                                ont: OWLOntology,
                                mgr: OWLOntologyManager,
                                pm: PrefixManager)
            extends ConceptScheme(sid, sp, config.kromRepPrefix, config.kromRepSuffix,
                                  "Rep_", ont, mgr, pm)

case class TaxonomyScheme(sid: String,
                          sp: String,
                          config: KromConfig,
                          ont: OWLOntology,
                          mgr: OWLOntologyManager,
                          pm: PrefixManager)
            extends ConceptScheme(sid, sp, config.kromRepPrefix, config.kromRepSuffix,
                                  "Taxa_", ont, mgr, pm)

case class NoScheme() extends Scheme {
    override def assertInScheme(individual: OWLNamedIndividual,
                                propertyFactory: PropertyFactory): Unit = ()
    override def prefixed(ident: String): String = ident
}

class IndividualManager(final val config: KromConfig,
                        final val ontology: OWLOntology,
                        final val manager: OWLOntologyManager,
                        prefixManager: PrefixManager)
    extends IndividualSearch(manager.getOWLDataFactory, prefixManager) {

    private val skosBaseIRI = config.skosBaseIRI
    private val morkIdentifierPropertyKey = ":identifier"
    private val propertyFactory = PropertyFactory(ontology, manager, prefixManager)

    def getNamedIndividual(subject: String, scheme: Scheme): OWLNamedIndividual =
        super.getPrefixedNamedIndividual(scheme.prefixed(subject))

    def assertIndividual(id: String,
                         ofClass: OWLClassExpression,
                         skipIdent: Boolean = false,
                         noIdent: Boolean = false,
                         noPrefix: Boolean = false,
                         scheme: Scheme): OWLNamedIndividual =

        val individual = if noPrefix then super.getPrefixedNamedIndividual(id)
        else this.getNamedIndividual(id, scheme)
        assertClass(individual, ofClass)

        val identifiedIndividual = (skipIdent, noIdent) match {
            case (true, _) => individual
            case (false, true) => identifyIndividual("", assertSkosRepNote(id, individual, scheme))
            case (_, false) => identifyIndividual(id, assertSkosRepNote(id, individual, scheme))
        }
        scheme.assertInScheme(identifiedIndividual, propertyFactory)
        identifiedIndividual

    private def assertDatatypeProperty(individual: OWLNamedIndividual,
                                       propertyName: String,
                                       scalar: Scalar): OWLNamedIndividual =
        val propAssertion = scalar.assertDatatypeProperty(propertyName, individual, factory, prefixManager)
        propAssertion match {
            case None => individual
            case Some(prop) => manager.applyChange(AddAxiom(ontology, prop)); individual
        }

    private def identifyIndividual(id: String, individual: OWLNamedIndividual): OWLNamedIndividual =
        assertDatatypeProperty(individual, morkIdentifierPropertyKey, Scalar(id))

    private def assertClass(individual: OWLNamedIndividual, ofClass: OWLClassExpression): Unit =
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)
        manager.applyChange(AddAxiom(ontology, clsAssertion))

    private def assertSkosRepNote(id: String, individual: OWLNamedIndividual,
                                  scheme: Scheme): OWLNamedIndividual =
        val note = "Individual Representation " + scheme.prefixed(id) + ", created-by krom"
        assertSkosNote(note, individual)

    private def assertSkosNote(note: String, individual: OWLNamedIndividual): OWLNamedIndividual = {
        val skosNote = factory.getOWLAnnotationProperty(skosBaseIRI + "#note")
        val annotation = factory.getOWLAnnotation(skosNote, factory.getOWLLiteral(note))
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)
        manager.applyChange(AddAxiom(ontology, annotationAxiom))
        individual
    }
}

class Ontology private (config: KromConfig, ontology: OWLOntology,
                        manager: OWLOntologyManager) extends Initialized(manager.getOWLDataFactory) {

    private val counter = new AtomicLong(Integer.MAX_VALUE)
    private val morkIRI = IRI.create(config.morkURL)
    private val morkBaseIRI = IRI.create(config.morkBaseIRI)
    private val skosBaseIRI = IRI.create(config.skosBaseIRI)
    private val anonymousEntities: mutable.Set[String] = mutable.HashSet.empty

    // private lazy val factory: OWLDataFactory = manager.getOWLDataFactory
    private lazy val morkPrefixManager = new DefaultPrefixManager(null, null, config.morkBaseIRI + "#")
    private lazy val mappingPrefixManager = new DefaultPrefixManager(null, null, config.kromBaseIRI + "#")

    private def schemeName(suffix: String): String =
        shortName(config.kromBaseIRI) + "_" + suffix
    private def shortName(iri: String) = iri.split("/").last

    private lazy val taxaSchemeID = schemeName("Taxa")
    private lazy val taxaSchemeProperty = ":conceptScheme"
    private lazy val taxonomyScheme =
        TaxonomyScheme(taxaSchemeID, taxaSchemeProperty, config, ontology, manager, mappingPrefixManager)

    private lazy val repSchemeID = schemeName("Representation")
    private lazy val repSchemeProperty = ":representationScheme"
    private lazy val representationScheme =
        RepresentationScheme(repSchemeID, repSchemeProperty, config, ontology, manager, mappingPrefixManager)

    private val noScheme = NoScheme()

    private val individualManager =
        IndividualManager(config, ontology, manager, mappingPrefixManager)

    private val morkIndividuals = IndividualManager(config, ontology, manager, morkPrefixManager)

    private val propertyManager = PropertyFactory(ontology, manager, morkPrefixManager)

    private lazy val morkRepSchemeDef: OWLClassExpression =
        getClassExpression(":RepresentationScheme", morkPrefixManager).getNNF
    private lazy val morkDCSchemeDef: OWLClassExpression =
        getClassExpression(":TaxonomyScheme", morkPrefixManager).getNNF
    private lazy val morkAttributeDef: OWLClassExpression =
        getClassExpression(":Attribute", morkPrefixManager)
    private lazy val morkAnonEntityDef: OWLClassExpression =
        getClassExpression(":AnonymousElement", morkPrefixManager).getNNF
    private lazy val morkCollectionElemDef: OWLClassExpression =
        getClassExpression(":ScalarValueElement", morkPrefixManager)
    private lazy val morkEntityDef: OWLClassExpression =
        getClassExpression(":Entity", morkPrefixManager)
    private lazy val morkArrayDef: OWLClassExpression =
        getClassExpression(":Array", morkPrefixManager).getNNF
    private lazy val morkConceptDef: OWLClassExpression =
        getClassExpression(":DataConcept", morkPrefixManager)
    private lazy val morkObjectificationDef: OWLClassExpression =
        getClassExpression(":Objectification", morkPrefixManager)

    private lazy val bottom: OWLNamedIndividual = factory.getOWLNamedIndividual(":Bottom", morkPrefixManager)

    private lazy val morkAssociativeBroadConceptRoleIRI =
        factory.getOWLObjectProperty(morkBroadConceptRoleKey, morkPrefixManager).getIRI
    private lazy val morkAssociativeNarrowConceptRoleIRI =
        factory.getOWLObjectProperty(morkNarrowConceptRoleKey, morkPrefixManager).getIRI

    def factory: OWLDataFactory = manager.getOWLDataFactory

    private def init(): Unit =
        val iriMapper = new SimpleIRIMapper(morkBaseIRI, morkIRI)
        manager.getIRIMappers.add(iriMapper)
        manager.loadOntology(morkBaseIRI)

        val skosImport = factory.getOWLImportsDeclaration(skosBaseIRI)
        manager.applyChange(new AddImport(ontology, skosImport))

        val morkImport = factory.getOWLImportsDeclaration(morkBaseIRI)
        manager.applyChange(new AddImport(ontology, morkImport))

        def assertScheme(id: String, ofClass: OWLClassExpression) =
            individualManager.assertIndividual(id, ofClass, skipIdent = true,
                                               noPrefix = true, scheme = noScheme)

        val rep = assertScheme(repSchemeID, morkRepSchemeDef)
        assertSkosNote("Representation Concept Scheme", rep)

        val taxa = assertScheme(taxaSchemeID, morkDCSchemeDef)
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
        val child = individualManager.getNamedIndividual(narrower, representationScheme)
        val parent = individualManager.getNamedIndividual(broader, representationScheme)
        val (subj, obj) = propertyManager.assertObjectProperty(parent, morkMemberPropertyKey, child)
        objectifyAssociation(broader, subj, narrower, obj)

    def addRepEntityAnon(to: String): String =
        if anonymousEntities.contains(to) then to
        else
            // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
            val id = anonID
            anonymousEntities += id
            val individual = individualManager.assertIndividual(id, morkAnonEntityDef,
                                                                noIdent = true, scheme = representationScheme)
            val propIndividual = representationScheme.getNamedIndividual(to)
            propertyManager.assertObjectProperty(propIndividual, morkElementTypeKey, individual)
            objectifyAssociation(to, individualManager.getNamedIndividual(to, representationScheme), id, individual)
            id

    def addCollectionElement(to: String, dataType: DataType): Unit =
        val id = to.concat("_Element").concat(counter.incrementAndGet().toString)
        val individual = individualManager.assertIndividual(id, morkCollectionElemDef,
                                                            scheme = representationScheme)
        propertyManager.assertObjectProperty(representationScheme.getNamedIndividual(to),
                                             morkElementTypeKey, individual)
        val propIRI = assertDataPropertyRange(id, morkCollectionElementScalarValueKey, dataType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        propertyManager.assertObjectProperty(propIndividual, morkRepresentedAsKey, individual)
        assertRdfsSeeAlso(propIRI, individual)

    def addRepresentationEntity(id: String): Unit =
        val individual = individualManager.assertIndividual(id, morkEntityDef, scheme = representationScheme)
        assertClass(individual, morkConceptDef)
        taxonomyScheme.assertInScheme(individual, propertyManager)
        propertyManager.assertObjectProperty(individual, morkRepresentationOfKey, individual)

    def addRepresentationArray(id: String): Unit =
        val individual = individualManager.assertIndividual(id, morkArrayDef, scheme = representationScheme)
        assertClass(individual, morkConceptDef)
        propertyManager.assertObjectProperty(individual, morkRepresentationOfKey, individual)
        assertDatatypeProperty(individual, morkOrderedItemsKey, Scalar(false))

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit =
        val individual = individualManager.assertIndividual(what, morkAttributeDef, scheme = representationScheme)
        propertyManager.assertObjectProperty(representationScheme.getNamedIndividual(where),
                                             morkMemberPropertyKey, individual)
        assertClass(individual, morkConceptDef)
        taxonomyScheme.assertInScheme(individual, propertyManager)

        val propIRI = assertDataPropertyRange(what, underlying.xsdType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        propertyManager.assertObjectProperty(propIndividual, morkRepresentedAsKey, individual)
        assertRdfsSeeAlso(propIRI, individual)

    // properties are inherent data concepts that must be reified
    private def objectifyAssociation(subjName: String,
                                     subj: OWLNamedIndividual,
                                     objName: String,
                                     obj: OWLNamedIndividual): Unit =
        val id = subjName.capitalize + "_" + objName.capitalize
        val objFact: OWLNamedIndividual =
            individualManager.assertIndividual(id, morkObjectificationDef,
                                               skipIdent = true, scheme = taxonomyScheme)
        propertyManager.assertObjectProperty(objFact, morkBroadConceptRoleKey, subj)
        propertyManager.assertObjectProperty(objFact, morkNarrowConceptRoleKey, obj)

        val subjectID = representationScheme.prefixed(subjName)
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

    private def assertDatatypeProperty(individual: OWLNamedIndividual,
                                       propertyName: String,
                                       scalar: Scalar): OWLNamedIndividual =
        val propAssertion = scalar.assertDatatypeProperty(propertyName, individual, factory, morkPrefixManager)
        propAssertion match {
            case None => individual
            case Some(prop) => manager.applyChange(AddAxiom(ontology, prop)); individual
        }

    private def assertClass(individual: OWLNamedIndividual, ofClass: OWLClassExpression): Unit =
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)
        manager.applyChange(AddAxiom(ontology, clsAssertion))

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
        representationScheme.prefixed("Anon_" + counter.incrementAndGet().toString)

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
        val manager = OWLManager.createOWLOntologyManager()
        val ontology = manager.createOntology(IRI.create(config.kromBaseIRI))
        val ont = new Ontology(config, ontology, manager)
        ont.init()
        ont
    }
}

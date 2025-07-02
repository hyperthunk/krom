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

private trait Initialized(val dataFactory: OWLDataFactory) extends LazyLogging {
    def getOwlClass(shortName: String, prefixManager: PrefixManager): OWLClass =
        dataFactory.getOWLClass(shortName, prefixManager)

    def getClassExpression(shortName: String, prefixManager: PrefixManager): OWLClassExpression =
        getOwlClass(shortName, prefixManager).getNNF
}

private trait IndividualSearch(final val factory: OWLDataFactory,
                               final val prefixManager: PrefixManager) extends Initialized {
    def getPrefixedNamedIndividual(id: String): OWLNamedIndividual =
        factory.getOWLNamedIndividual(":" + id, prefixManager)
}

private trait Scheme {
    def assertInScheme(individual: OWLNamedIndividual, propertyFactory: PropertyAxioms): Unit

    def prefixed(ident: String): String
}

private abstract class ConceptScheme(final val schemeID: String,
                                     final val schemeProperty: String,
                                     final val prefix: Option[String],
                                     final val suffix: Option[String],
                                     final val defaultPrefix: String,
                                     final val ontology: OWLOntology,
                                     final val manager: OWLOntologyManager,
                                     pm: PrefixManager)
    extends IndividualSearch(manager.getOWLDataFactory, pm),
            Initialized(manager.getOWLDataFactory), Scheme:

    def assertInScheme(individual: OWLNamedIndividual, propertyFactory: PropertyAxioms): Unit =
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
                                propertyFactory: PropertyAxioms): Unit = ()

    override def prefixed(ident: String): String = ident
}

type Schemes = (repScheme: RepresentationScheme, taxaScheme: TaxonomyScheme, none: NoScheme)
type Prefixes = (mork: PrefixManager, mapping: PrefixManager)

private class IndividualAxioms(final val config: KromConfig,
                               final val ontology: OWLOntology,
                               final val manager: OWLOntologyManager,
                               final val schemes: Schemes,
                               prefixes: Prefixes)
    extends IndividualSearch(manager.getOWLDataFactory, prefixes.mapping),
            Initialized(manager.getOWLDataFactory) {

    private val annotations = Annotations(config, ontology, factory, manager)

    private lazy val morkAnonEntityDef: OWLClassExpression =
        getClassExpression(":AnonymousElement", prefixes.mork)
    private lazy val morkCollectionElemDef: OWLClassExpression =
        getClassExpression(":ScalarValueElement", prefixes.mork)
    private lazy val morkAttributeDef: OWLClassExpression =
            getClassExpression(":Attribute", prefixes.mork)
    private lazy val morkEntityDef: OWLClassExpression =
        getClassExpression(":Entity", prefixes.mork)
    private lazy val morkArrayDef: OWLClassExpression =
        getClassExpression(":Array", prefixes.mork)
    private lazy val morkConceptDef: OWLClassExpression =
        getClassExpression(":DataConcept", prefixes.mork)
    private lazy val morkObjectificationDef: OWLClassExpression =
        getClassExpression(":Objectification", prefixes.mork)

    private val skosBaseIRI = config.skosBaseIRI
    private val morkIdentifierPropertyKey = ":identifier"
    private val propertyFactory = PropertyAxioms(ontology, manager, prefixes)

    def getNamedIndividual(subject: String, scheme: Scheme): OWLNamedIndividual =
        super.getPrefixedNamedIndividual(scheme.prefixed(subject))

    def assertAnonymousEntity(id: String): OWLNamedIndividual = {
        assertIndividual(id, morkAnonEntityDef, noIdent = true, scheme = schemes.repScheme)
    }

    def assertArrayEntity(id: String): OWLNamedIndividual = {
        assertIndividual(id, morkCollectionElemDef, scheme = schemes.repScheme)
    }

    def assertAttribute(id: String): OWLNamedIndividual = {
        assertConcept(assertIndividual(id, morkAttributeDef, scheme = schemes.repScheme))
    }

    def assertEntity(id: String): OWLNamedIndividual = {
        assertConcept(assertIndividual(id, morkEntityDef, scheme = schemes.repScheme))
    }

    def assertArray(id: String): OWLNamedIndividual = {
        assertConcept(assertIndividual(id, morkArrayDef, scheme = schemes.repScheme))
    }

    def assertObjectification(id: String): OWLNamedIndividual = {
        assertConcept(assertIndividual(id, morkObjectificationDef, skipIdent = true, scheme = schemes.taxaScheme))
    }

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

    private def identifyIndividual(id: String, individual: OWLNamedIndividual): OWLNamedIndividual =
        PropertyAxioms.assertDatatypeProperty(individual, morkIdentifierPropertyKey,
                                              Scalar(id), ontology, factory, manager, prefixes.mork)

    def assertConcept(individual: OWLNamedIndividual): OWLNamedIndividual =
        assertClass(individual, morkConceptDef)
        individual

    def assertClass(individual: OWLNamedIndividual, ofClass: OWLClassExpression): Unit =
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)
        manager.applyChange(AddAxiom(ontology, clsAssertion))

    private def assertSkosRepNote(id: String, individual: OWLNamedIndividual,
                                  scheme: Scheme): OWLNamedIndividual =
        val note = "Individual Representation " + scheme.prefixed(id) + ", created-by krom"
        annotations.assertSkosNote(note, individual)
}

private class Annotations(config: KromConfig,
                          ontology: OWLOntology,
                          factory: OWLDataFactory,
                          manager: OWLOntologyManager) {

    def assertSkosNote(note: String, individual: OWLNamedIndividual): OWLNamedIndividual = {
        val skosNote = factory.getOWLAnnotationProperty(config.skosBaseIRI + "#note")
        val annotation = factory.getOWLAnnotation(skosNote, factory.getOWLLiteral(note))
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)
        manager.applyChange(AddAxiom(ontology, annotationAxiom))
        individual
    }

    def assertRdfsSeeAlso(otherIRI: IRI, individual: OWLNamedIndividual): Unit = {
        val annotation = factory.getOWLAnnotation(factory.getRDFSSeeAlso, otherIRI)
        val annotationAxiom = factory.getOWLAnnotationAssertionAxiom(individual.getIRI, annotation)

        manager.applyChange(AddAxiom(ontology, annotationAxiom))
    }
}

private class PropertyAxioms(final val ontology: OWLOntology,
                             final val manager: OWLOntologyManager,
                             prefixes: Prefixes)
    extends IndividualSearch(manager.getOWLDataFactory, prefixes.mapping),
            Initialized(manager.getOWLDataFactory) {

    private val morkMemberPropertyKey = ":memberProperty"
    private val morkBroadConceptRoleKey = ":broadConceptRole"
    private val morkNarrowConceptRoleKey = ":narrowConceptRole"
    private val morkElementTypeKey = ":elementType"

    def assertElementType(subj: OWLNamedIndividual, obj: OWLNamedIndividual):
            (sub: OWLNamedIndividual, obj: OWLNamedIndividual) = {
        assertObjectProperty(subj, morkElementTypeKey, obj)
    }

    def assertObjectProperty(subject: OWLNamedIndividual, propertyName: String,
                             obj: OWLNamedIndividual): (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        val factory = manager.getOWLDataFactory
        val expr = factory.getOWLObjectProperty(propertyName, prefixManager)
        val propAssertion = factory.getOWLObjectPropertyAssertionAxiom(expr, subject, obj)

        manager.applyChange(AddAxiom(ontology, propAssertion))
        (subject, obj)

    def objectifyAssociation(subj: OWLNamedIndividual,
                             objFact: OWLNamedIndividual,
                             obj: OWLNamedIndividual): Unit =
        assertObjectProperty(objFact, morkBroadConceptRoleKey, subj)
        assertObjectProperty(objFact, morkNarrowConceptRoleKey, obj)

    def memberProperty(subject: OWLNamedIndividual, obj: OWLNamedIndividual):
            (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        assertObjectProperty(subject, morkMemberPropertyKey, obj)

    def assertDataPropertyRange(id: String, propKey: String, dataType: DataType): IRI =
        val identProp = factory.getOWLDataProperty(propKey, prefixes.mork)
        val dpEx = factory.getOWLDataProperty(id, prefixes.mork)
        val subPropAxiom = factory.getOWLSubDataPropertyOfAxiom(dpEx, identProp)
        val rangeAxiom = factory.getOWLDataPropertyRangeAxiom(dpEx, dataType.toOwlDataType(factory))

        manager.applyChange(AddAxiom(ontology, subPropAxiom))
        manager.applyChange(AddAxiom(ontology, rangeAxiom))
        dpEx.getIRI

    def assertDatatypeProperty(individual: OWLNamedIndividual,
                               propertyName: String,
                               scalar: Scalar): OWLNamedIndividual =
        PropertyAxioms.assertDatatypeProperty(individual, propertyName, scalar,
                                              ontology, factory, manager, prefixManager)
}

object PropertyAxioms {
    def assertDatatypeProperty(individual: OWLNamedIndividual,
                               propertyName: String,
                               scalar: Scalar,
                               ontology: OWLOntology,
                               factory: OWLDataFactory,
                               manager: OWLOntologyManager,
                               prefixManager: PrefixManager): OWLNamedIndividual =
        val propAssertion = scalar.assertDatatypeProperty(propertyName, individual, factory, prefixManager)
        propAssertion match {
            case None => individual
            case Some(prop) => manager.applyChange(AddAxiom(ontology, prop)); individual
        }
}

class Ontology private (config: KromConfig,
                        ontology: OWLOntology,
                        factory: OWLDataFactory,
                        manager: OWLOntologyManager) extends Initialized(manager.getOWLDataFactory) {

    private val counter = new AtomicLong(Integer.MAX_VALUE)
    private val morkIRI = IRI.create(config.morkURL)
    private val morkBaseIRI = IRI.create(config.morkBaseIRI)
    private val skosBaseIRI = IRI.create(config.skosBaseIRI)
    private val anonymousEntities: mutable.Set[String] = mutable.HashSet.empty

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

    private val schemes = (representationScheme, taxonomyScheme, noScheme)
    private val prefixes = (morkPrefixManager, mappingPrefixManager)

    private val individuals = IndividualAxioms(config, ontology, manager, schemes, prefixes)
    private val properties = PropertyAxioms(ontology, manager, prefixes)
    private val annotations = Annotations(config, ontology, factory, manager)

    private lazy val morkRepSchemeDef: OWLClassExpression =
        getClassExpression(":RepresentationScheme", morkPrefixManager).getNNF
    private lazy val morkDCSchemeDef: OWLClassExpression =
        getClassExpression(":TaxonomyScheme", morkPrefixManager).getNNF

    private lazy val morkAssociativeBroadConceptRoleIRI =
        factory.getOWLObjectProperty(morkBroadConceptRoleKey, morkPrefixManager).getIRI
    private lazy val morkAssociativeNarrowConceptRoleIRI =
        factory.getOWLObjectProperty(morkNarrowConceptRoleKey, morkPrefixManager).getIRI

    private def init(): Unit =
        val iriMapper = new SimpleIRIMapper(morkBaseIRI, morkIRI)
        manager.getIRIMappers.add(iriMapper)
        manager.loadOntology(morkBaseIRI)

        val skosImport = factory.getOWLImportsDeclaration(skosBaseIRI)
        manager.applyChange(new AddImport(ontology, skosImport))

        val morkImport = factory.getOWLImportsDeclaration(morkBaseIRI)
        manager.applyChange(new AddImport(ontology, morkImport))

        def assertScheme(id: String, ofClass: OWLClassExpression) =
            individuals.assertIndividual(id, ofClass, skipIdent = true,
                                               noPrefix = true, scheme = noScheme)

        val rep = assertScheme(repSchemeID, morkRepSchemeDef)
        annotations.assertSkosNote("Representation Concept Scheme", rep)

        val taxa = assertScheme(taxaSchemeID, morkDCSchemeDef)
        annotations.assertSkosNote("Taxonomy Concept Scheme", taxa)

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
        val child = individuals.getNamedIndividual(narrower, representationScheme)
        val parent = individuals.getNamedIndividual(broader, representationScheme)
        val (subj, obj) = properties.memberProperty(parent, child)
        objectifyAssociation(broader, subj, narrower, obj)

    // TODO: SWRL (or SHACL) axioms to correlate anonymous members that share the same type def
    private def anonID: String =
        representationScheme.prefixed("Anon_" + counter.incrementAndGet().toString)

    def addRepEntityAnon(to: String): String =
        if anonymousEntities.contains(to) then to
        else
            // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
            val id = anonID
            anonymousEntities += id
            val individual = individuals.assertAnonymousEntity(id)

            val propIndividual = representationScheme.getNamedIndividual(to)
            properties.assertElementType(propIndividual, individual)
            objectifyAssociation(to, individuals.getNamedIndividual(to, representationScheme), id, individual)
            id

    def addCollectionElement(to: String, dataType: DataType): Unit =
        val id = to.concat("_Element").concat(counter.incrementAndGet().toString)
        val individual = individuals.assertArrayEntity(id)
        properties.assertElementType(representationScheme.getNamedIndividual(to), individual)
        val propIRI = properties.assertDataPropertyRange(id, morkCollectionElementScalarValueKey, dataType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        properties.assertObjectProperty(propIndividual, morkRepresentedAsKey, individual)
        annotations.assertRdfsSeeAlso(propIRI, individual)

    def addRepresentationEntity(id: String): Unit =
        val individual = individuals.assertEntity(id)
        taxonomyScheme.assertInScheme(individual, properties)
        properties.assertObjectProperty(individual, morkRepresentationOfKey, individual)

    def addRepresentationArray(id: String): Unit =
        val individual = individuals.assertArray(id)
        properties.assertObjectProperty(individual, morkRepresentationOfKey, individual)
        properties.assertDatatypeProperty(individual, morkOrderedItemsKey, Scalar(false))

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit =
        val individual = individuals.assertAttribute(what)
        properties.memberProperty(representationScheme.getNamedIndividual(where), individual)
        taxonomyScheme.assertInScheme(individual, properties)

        val propIRI = assertDataPropertyRange(what, underlying.xsdType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        properties.assertObjectProperty(propIndividual, morkRepresentedAsKey, individual)
        annotations.assertRdfsSeeAlso(propIRI, individual)

    // properties are inherent data concepts that must be reified
    private def objectifyAssociation(subjName: String,
                                     subj: OWLNamedIndividual,
                                     objName: String,
                                     obj: OWLNamedIndividual): Unit =
        val id = subjName.capitalize + "_" + objName.capitalize
        val objFact: OWLNamedIndividual = individuals.assertObjectification(id)

        properties.objectifyAssociation(subj, objFact, obj)

        val subjectID = representationScheme.prefixed(subjName)
        val txt = s"Models an association between $subjectID and $objName, created-by krom"
        annotations.assertSkosNote(txt, objFact)
        annotations.assertRdfsSeeAlso(morkAssociativeBroadConceptRoleIRI, objFact)
        annotations.assertRdfsSeeAlso(morkAssociativeNarrowConceptRoleIRI, objFact)

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

}

object Ontology {
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
        val ont = new Ontology(config, ontology, manager.getOWLDataFactory, manager)
        ont.init()
        ont
    }
}

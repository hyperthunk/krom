package org.nebularis.krom

import com.typesafe.scalalogging.LazyLogging
import org.nebularis.krom.DataType
import org.nebularis.krom.LookupResult.{Asserted, Cached}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.parameters.Imports.EXCLUDED
import org.semanticweb.owlapi.model.parameters.Navigation.IN_SUB_POSITION
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util.{DefaultPrefixManager, SimpleIRIMapper}

import java.io.OutputStream
import java.util.concurrent.atomic.AtomicLong
import java.util.function.Predicate
import scala.collection.mutable
import scala.language.postfixOps
import scala.runtime.Nothing$;

private enum LookupResult[T](thing: T):
    case Cached(it: T) extends LookupResult[T](it)
    case Asserted(it: T) extends LookupResult[T](it)

    def result: T = thing

type Schemes = (repScheme: RepresentationScheme, taxaScheme: TaxonomyScheme, none: NoScheme)
type Prefixes = (mork: PrefixManager, mapping: PrefixManager)
type EntityMapping = (base: String, concept: IRI)
type AssertedIndividuals =
    (newId: String, representation: OWLNamedIndividual, concept: OWLNamedIndividual)

class OntologyException(msg: String, context: OWLEntity) extends RuntimeException(msg)

private trait Initialized(val dataFactory: OWLDataFactory) extends LazyLogging {
    def getOwlClass(shortName: String, prefixManager: PrefixManager): OWLClass =
        dataFactory.getOWLClass(shortName, prefixManager)

    def getClassExpression(shortName: String, prefixManager: PrefixManager): OWLClassExpression =
        getOwlClass(shortName, prefixManager).getNNF
}

private trait IndividualSearch(final val factory: OWLDataFactory,
                               final val prefixManager: PrefixManager) extends Initialized {

    private lazy val cachedIndividuals: mutable.Map[IRI, OWLNamedIndividual] = mutable.Map.empty

    def getCachedIndividual(iri: IRI): Option[OWLNamedIndividual] =
        cachedIndividuals.get(iri)

    def getPrefixedNamedIndividual(id: String): OWLNamedIndividual =
        getOrAssertPrefixedNamedIndividual(id).result

    def getOrAssertPrefixedNamedIndividual(id: String): LookupResult[OWLNamedIndividual] =
        val abbreviatedIRI = ":" + id
        val iri = prefixManager.getIRI(abbreviatedIRI)
        cachedIndividuals.get(iri) match {
            case None =>
                val individual = factory.getOWLNamedIndividual(abbreviatedIRI, prefixManager)
                cachedIndividuals.put(individual.getIRI, individual)
                Asserted(individual)
            case Some(indiv) => Cached(indiv)
        }
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

    // TODO: pass config in and see if we're skipping or reducing prefixes

    private def identifier(pfx: Option[String],
                           sfx: Option[String], it: String): Option[String] = (pfx, sfx) match {
        case (None, None) => Some(it)  // TODO: this should be None
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
    extends ConceptScheme(sid, sp, config.kromTaxaPrefix, config.kromTaxaSuffix,
        "Taxa_", ont, mgr, pm)

case class NoScheme() extends Scheme {
    override def assertInScheme(individual: OWLNamedIndividual,
                                propertyFactory: PropertyAxioms): Unit = ()

    override def prefixed(ident: String): String = ident
}

private class IndividualAxioms(final val config: KromConfig,
                               final val ontology: OWLOntology,
                               final val manager: OWLOntologyManager,
                               final val schemes: Schemes,
                               prefixes: Prefixes)
    extends IndividualSearch(manager.getOWLDataFactory, prefixes.mapping),
            Initialized(manager.getOWLDataFactory) {

    private val annotations = Annotations(config, ontology, factory, manager)

    private val morkIdentifierPropertyKey = ":identifier"
    private val morkPathPropertyKey = ":path"
    private val morkConceptIdPropertyKey = ":conceptId"
    private val morkPathPropertyIri = IRI.create(config.morkBaseIRI + "#path")
    private val morkIdentPropertyIri = IRI.create(config.morkBaseIRI + "#identifier")
    private val properties = PropertyAxioms(ontology, manager, prefixes)

    private lazy val cachedEntities: mutable.Map[String, EntityMapping] = mutable.Map.empty

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

    private lazy val morkPathPropertyDef: OWLDataPropertyExpression =
        dataFactory.getOWLDataProperty(morkPathPropertyIri)
    private lazy val morkIdentPropertyDef: OWLDataPropertyExpression =
        dataFactory.getOWLDataProperty(morkIdentPropertyIri)

    def getConceptFor(id: String): Option[OWLNamedIndividual] =
        cachedEntities.get(id) match {
            case None => None
            case Some(mapping) =>
                val concept = factory.getOWLNamedIndividual(mapping.concept)
                Some(concept)
        }

    def getBaseIdFor(id: String): Option[String] =
        cachedEntities.get(id) match {
            case None => None
            case Some(mapping) => Some(mapping.base)
        }

    def getNamedIndividual(subject: String, scheme: Scheme): OWLNamedIndividual =
        super.getPrefixedNamedIndividual(scheme.prefixed(subject))

    def assertAnonymousEntity(id: String): OWLNamedIndividual = {
        assertRepIndividual(id, morkAnonEntityDef, altIdent = Some(""), scheme = schemes.repScheme)
    }

    def assertArrayEntity(id: String): OWLNamedIndividual = {
        assertRepIndividual(id, morkCollectionElemDef, scheme = schemes.repScheme)
    }

    def assertAttribute(where: String, what: String): OWLNamedIndividual = {
        val id = s"${where}_${what}"
        val individual = assertRepIndividual(id, morkAttributeDef,
                                          altIdent = Some(what), scheme = schemes.repScheme)
        // set path attribute if possible via the broader concept
        val broader = getNamedIndividual(where, schemes.repScheme)
        assertConcept(bindPath(broader, individual, what))
    }

    def bindPath(from: OWLNamedIndividual, to: OWLNamedIndividual,
                 ident: String, delim: String = "."): OWLNamedIndividual = {
        // NB: when the same DataConcept appears in multiple paths (in a representation),
        // we need to generate unique individuals (with unique OWL names/ids) to avoid
        // messing up the output graph - yet here, we must look up any potential cached
        // id for such individuals, and ensure the paths we generate are correct...
        val pathIdent = getBaseIdFor(ident) match {
            case None => ident
            case Some(base) => base
        }
        if !concatPath(from, to, pathIdent, morkPathPropertyKey, delim) then {
            concatPath(from, to, pathIdent, morkIdentifierPropertyKey, delim)
        }
        to
    }

    private def concatPath(from: OWLNamedIndividual, to: OWLNamedIndividual,
                           ident: String, pathKey: String, delimiter: String): Boolean = {

        val prop = factory.getOWLDataProperty(pathKey, prefixes.mork).asOWLDataProperty
        def bind(dp: OWLDataPropertyAssertionAxiom): Predicate[OWLDataProperty] = {
            (odp: OWLDataProperty) => {
                if odp.equals(prop) then {
                    val whereIdent = dp.getObject.getLiteral
                    val path = s"$whereIdent$delimiter$ident"
                    PropertyAxioms.assertDatatypeProperty(to, morkPathPropertyKey,
                        Scalar(path), ontology, factory, manager, prefixes.mork)
                    true
                } else {
                    false
                }
            }
        }

        val assertions: Array[OWLDataPropertyAssertionAxiom] = Array.empty
        val axioms = ontology.getDataPropertyAssertionAxioms(from).toArray(assertions)
        val bound = axioms.find { (dp: OWLDataPropertyAssertionAxiom) =>
            dp.dataPropertiesInSignature.anyMatch(bind(dp))
        }
        bound.isDefined
    }

    private def lookupPropertyValue(prop: OWLDataPropertyExpression,
                                    scope: OWLNamedIndividual): Option[OWLLiteral] =
        val literals = EntitySearcher.getDataPropertyValues(scope, prop, ontology).toList
        literals.size match {
            case 0 => None
            case 1 => Some(literals.get(0))
            case n if n > 1 =>
                throw new OntologyException(
                    s"Multiple values for property '${prop.toString}' on individual '${scope.getIRI}'",
                    scope)
                None  // unreachable, but the compiler thinks more of java's type system than it should...
        }

    def assertRootEntity: OWLNamedIndividual =
        // we need to assert the root entity, which is the root of the representation scheme
        val root = assertRepIndividual("$", morkAnonEntityDef, altIdent = None, scheme = schemes.repScheme)
        val note = "Represents the 'root' of a Json document, created-by krom"
        annotations.assertSkosNote(note, root)
        root

    def assertEntity(where: String, id: String): AssertedIndividuals = {
        val repScheme = schemes.repScheme
        val prefixedId = repScheme.prefixed(id)

        def handleCached(individual: OWLNamedIndividual): AssertedIndividuals = {
            def checkAnonScope(scopeIri: String, scope: OWLNamedIndividual): String = {
                val scopeClasses = ontology.classAssertionAxioms(scope)
                val anonMatch =
                    scopeClasses.anyMatch { (cls: OWLClassAssertionAxiom) =>
                        cls.getClassExpression.equals(morkAnonEntityDef)
                    }
                if anonMatch then
                    val path =
                        lookupPropertyValue(morkPathPropertyDef, scope) match {
                            case Some(p) => p.getLiteral
                            case None =>
                                throw new OntologyException(
                                    s"Missing 'path' property for ${scopeIri}", scope)
                        }
                    val pathParts = path.split('.')
                    pathParts.last.replace("[n]", "_Element")
                else
                    // I really dislike that it is idiomatic jvm behaviour to throw, however...
                    // chaining error handling is a big rewrite so for now we will simply crash
                    val msg = s"Duplicate or missing 'path' properties for ${scopeIri}"
                    throw new OntologyException(msg, scope)
                    msg // unreachable, but the compiler...
            }

            cachedEntities.get(id) match {
                case None => //is this actually a runtime error!?
                    throw new OntologyException("Invalid Cache for " + prefixedId, individual)
                case Some(em: EntityMapping) =>
                    // (base = original id, conceptIri = concept entry)
                    val scopeIri = repScheme.prefixed(where)
                    val scope = super.getOrAssertPrefixedNamedIndividual(scopeIri).result
                    val objIdent = lookupPropertyValue(morkIdentPropertyDef, scope) match {
                        case Some(ident) =>
                            ident.getLiteral match {
                                case "" => checkAnonScope(scopeIri, scope)
                                case id => id
                            }
                        case None => checkAnonScope(scopeIri, scope)
                    }
                    // we blend the scope and the id to form a new id, like 'Objectification'
                    val newId = s"${objIdent}_${id.capitalize}"  // owl-id of the OWLNamedIndividual
                    val individual = assertRepIndividual(newId, morkEntityDef,
                                                         altIdent=Some(id),
                                                         scheme = schemes.repScheme)
                    // we need to bind the new individual to the same concept as the cached one
                    val conceptId = schemes.taxaScheme.prefixed(id)
                    val concept = super.getOrAssertPrefixedNamedIndividual(conceptId).result
                    cachedEntities.put(newId, (em.base, concept.getIRI))
                    (newId, individual, concept)
            }
        }

        def handleAsserted(individual: OWLNamedIndividual): AssertedIndividuals =
            assertRepIndividual(individual, prefixedId, morkEntityDef,
                false, Some(id), false, schemes.repScheme)

            // we have created a new individual - we will need to create a DataConcept to bind it to
            val conceptId = schemes.taxaScheme.prefixed(id)
            super.getOrAssertPrefixedNamedIndividual(conceptId) match {
                case Cached(concept) =>
                    throw new OntologyException(
                        s"Duplicate DataConcept for '${conceptId}' on individual '${individual.getIRI}'",
                        individual)
                    (null, null, null) // unreachable, but the compiler...
                case Asserted(concept) =>
                    // we need to assert the concept class on the individual
                    val identifiedConcept =
                        assertConceptIndividual(concept, id.capitalize)
                    cachedEntities.put(id, (id, identifiedConcept.getIRI))
                    (id, individual, identifiedConcept)
            }

        super.getOrAssertPrefixedNamedIndividual(prefixedId) match {
            case Cached(individual) => handleCached(individual)
            case Asserted(individual) => handleAsserted(individual)
        }
    }

    def assertArray(id: String): OWLNamedIndividual = {
        assertConcept(assertRepIndividual(id, morkArrayDef, scheme = schemes.repScheme))
    }

    def assertObjectification(id: String): OWLNamedIndividual = {
        val concept = super.getOrAssertPrefixedNamedIndividual(id).result
        val individual = assertConceptIndividual(concept, id.capitalize, morkObjectificationDef)
        individual
    }

    // TODO: de-duplicate assert{Concept,RepIndividual}

    def assertRepIndividual(id: String,
                            ofClass: OWLClassExpression,
                            skipIdent: Boolean = false,
                            altIdent: Option[String] = None,
                            noPrefix: Boolean = false,
                            scheme: Scheme): OWLNamedIndividual =
        val individual = if noPrefix then super.getPrefixedNamedIndividual(id)
                         else this.getNamedIndividual(id, scheme)
        assertRepIndividual(individual, id, ofClass, skipIdent, altIdent, noPrefix, scheme)

    def assertRepIndividual(individual: OWLNamedIndividual,
                            id: String,
                            ofClass: OWLClassExpression,
                            skipIdent: Boolean,
                            altIdent: Option[String],
                            noPrefix: Boolean,
                            scheme: Scheme): OWLNamedIndividual =
        assertClass(individual, ofClass)

        val identifiedIndividual = (skipIdent, altIdent) match {
            case (true, _) => individual
            case (false, Some(alt)) => identifyIndividual(alt, assertSkosRepNote(id, individual, scheme))
            case (_, None) => identifyIndividual(id, assertSkosRepNote(id, individual, scheme))
        }
        scheme.assertInScheme(identifiedIndividual, properties)
        identifiedIndividual

    def assertConceptFor(concept: OWLNamedIndividual, id: String, what: String): OWLNamedIndividual =
        val configured = assertConceptIndividual(concept, id)
        cachedEntities.get(what) match {
            case Some(mapping) =>
                cachedEntities.put(what, (mapping.base, configured.getIRI))
                configured
            case None =>
                val base = getOrAssertPrefixedNamedIndividual(what).result
                cachedEntities.put(what, (id, configured.getIRI))
                configured
        }

    def assertConceptIndividual(individual: OWLNamedIndividual, id: String): OWLNamedIndividual =
        assertConceptIndividual(individual, id, morkConceptDef)

    def assertConceptIndividual(individual: OWLNamedIndividual,
                                id: String, cls: OWLClassExpression): OWLNamedIndividual =
        val scheme = schemes.taxaScheme
        assertClass(individual, cls)
        val identifiedIndividual = identifyConcept(id, assertSkosDCNote(id, individual, scheme))
        scheme.assertInScheme(identifiedIndividual, properties)
        identifiedIndividual

    private def identifyIndividual(id: String, individual: OWLNamedIndividual): OWLNamedIndividual =
        PropertyAxioms.assertDatatypeProperty(individual, morkIdentifierPropertyKey,
                                              Scalar(id), ontology, factory, manager, prefixes.mork)

    private def identifyConcept(id: String, individual: OWLNamedIndividual): OWLNamedIndividual =
        PropertyAxioms.assertDatatypeProperty(individual, morkConceptIdPropertyKey,
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

    private def assertSkosDCNote(id: String, individual: OWLNamedIndividual,
                                 scheme: Scheme): OWLNamedIndividual =
        val note = "Data Concept " + scheme.prefixed(id) + ", created-by krom"
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

    private final val morkMemberPropertyKey = ":memberProperty"
    private final val morkBroadConceptRoleKey = ":broadConceptRole"
    private final val morkNarrowConceptRoleKey = ":narrowConceptRole"
    private final val morkElementTypeKey = ":elementType"
    private final val morkRepresentedAsKey = ":representedAs"

    def assertElementType(subj: OWLNamedIndividual, obj: OWLNamedIndividual):
            (sub: OWLNamedIndividual, obj: OWLNamedIndividual) = {
        assertObjectProperty(subj, morkElementTypeKey, obj)
    }

    def assertRepresentedAs(subject: OWLNamedIndividual, obj: OWLNamedIndividual):
            (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        assertObjectProperty(subject, morkRepresentedAsKey, obj)

    def assertObjectProperty(subject: OWLNamedIndividual, propertyName: String,
                             obj: OWLNamedIndividual): (sub: OWLNamedIndividual, obj: OWLNamedIndividual) =
        val factory = manager.getOWLDataFactory
        val expr = factory.getOWLObjectProperty(propertyName, prefixes.mork)
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

// NB: NOT THREAD SAFE
class Ontology private (config: KromConfig,
                        ontology: OWLOntology,
                        factory: OWLDataFactory,
                        manager: OWLOntologyManager) extends Initialized(manager.getOWLDataFactory) {

    private final val morkOrderedItemsKey = ":orderedItems"
    private final val morkBroadConceptRoleKey = ":broadConceptRole"
    private final val morkNarrowConceptRoleKey = ":narrowConceptRole"
    private final val morkRepresentationOfKey = ":representationOf"
    private final val morkExternalPropertyValueKey = ":externalPropertyValue"
    private final val morkCollectionElementScalarValueKey = ":collectionElementScalarValue"

    private val counter = new AtomicLong(Integer.MAX_VALUE)
    private val morkIRI = IRI.create(config.morkURL)
    private val morkBaseIRI = IRI.create(config.morkBaseIRI)
    private val skosBaseIRI = IRI.create(config.skosBaseIRI)
    private val anonymousEntities: mutable.Map[String, OWLNamedIndividual] = mutable.HashMap.empty
    private val scalarValues: mutable.Map[String, Int] = mutable.HashMap.empty

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

    private def init(): Ontology =
        val iriMapper = new SimpleIRIMapper(morkBaseIRI, morkIRI)
        manager.getIRIMappers.add(iriMapper)
        manager.loadOntology(morkBaseIRI)

        val skosImport = factory.getOWLImportsDeclaration(skosBaseIRI)
        manager.applyChange(new AddImport(ontology, skosImport))

        val morkImport = factory.getOWLImportsDeclaration(morkBaseIRI)
        manager.applyChange(new AddImport(ontology, morkImport))

        def assertScheme(id: String, ofClass: OWLClassExpression) =
            individuals.assertRepIndividual(id, ofClass, skipIdent = true,
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
        this

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

        individuals.bindPath(subj, obj, narrower)
        objectifyAssociation(broader, subj, narrower, obj)

    // TODO: SWRL (or SHACL) axioms to correlate anonymous members that share the same type def
    private def anonID: String =
        representationScheme.prefixed("Anon_" + counter.incrementAndGet().toString)

    def addRepEntityAnon(to: String): String =
        //NOTE: we eschew dealing with discriminated type definitions here
        // since that is rather complex and better resolved with a json-schema processing approach
        anonymousEntities.get(to) match {
            case None =>
                // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
                val elemId = to.concat("_Element")
                val id = representationScheme.prefixed(elemId)
                val individual = individuals.assertAnonymousEntity(id)
                anonymousEntities.put(to, individual)

                val propIndividual = representationScheme.getNamedIndividual(to)
                properties.assertElementType(propIndividual, individual)

                individuals.bindPath(propIndividual, individual, "[n]", delim = "")

                val conceptId = taxonomyScheme.prefixed(elemId)
                val concept = individuals.getNamedIndividual(conceptId, taxonomyScheme)
                val ent = individuals.assertConceptFor(concept, conceptId, id)
                properties.assertRepresentedAs(ent, individual)

                // TODO: is the Objectification the concept of the rep?
                objectifyAssociation(to, individuals.getNamedIndividual(to, representationScheme), id, individual)
                id
            case Some(ent) =>
                ent.getIRI.toString.split("#").last
        }

    def addCollectionElement(to: String, dataType: DataType): Unit =
        val id = to.concat("_Element").concat(counter.incrementAndGet().toString)
        val individual = individuals.assertArrayEntity(id)
        val broader = representationScheme.getNamedIndividual(to)

        val index = scalarValues.get(to) match {
            case None => 0
            case Some(count) => count + 1
        }
        individuals.bindPath(broader, individual, s"[$index]", delim = "")

        properties.assertElementType(broader, individual)
        val propIRI = properties.assertDataPropertyRange(id, morkCollectionElementScalarValueKey, dataType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        properties.assertRepresentedAs(propIndividual, individual) // TODO: verify this
        annotations.assertRdfsSeeAlso(propIRI, individual)

    def addRootEntity(): Unit = individuals.assertRootEntity

    def addRepresentationEntity(where: String, id: String): String =
        val added = individuals.assertEntity(where, id)
        properties.assertObjectProperty(added.representation, morkRepresentationOfKey, added.concept)
        added.newId

    def addRepresentationArray(id: String): String =
        val individual = individuals.assertArray(id)
        properties.assertObjectProperty(individual, morkRepresentationOfKey, individual)
        properties.assertDatatypeProperty(individual, morkOrderedItemsKey, Scalar(false))
        id

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit =
        val individual = individuals.assertAttribute(where, what)
        val (sub, obj) = properties.memberProperty(representationScheme.getNamedIndividual(where), individual)
        taxonomyScheme.assertInScheme(individual, properties)

        individuals.bindPath(sub, obj, what)

        val propIRI = assertDataPropertyRange(what, underlying.xsdType)
        val propIndividual = representationScheme.getNamedIndividual(propIRI.getIRIString)
        properties.assertRepresentedAs(propIndividual, individual) //TODO: verify this
        annotations.assertRdfsSeeAlso(propIRI, individual)

    private def assertDataPropertyRange(id: String, dataType: DataType): IRI =
        properties.assertDataPropertyRange(":ex_prop_" + id, morkExternalPropertyValueKey, dataType)

    // properties are inherent data concepts that must be reified
    private def objectifyAssociation(subjName: String,
                                     subj: OWLNamedIndividual,
                                     objName: String,
                                     obj: OWLNamedIndividual): Unit =
        val sid = subjName.replace("Rep_", "").capitalize
        val oid = objName.replace("Rep_", "").capitalize
        val id = s"${sid}_has_${oid}"
        val objFact: OWLNamedIndividual =
            individuals.assertObjectification(taxonomyScheme.prefixed(id))

        // if data concepts exist, add them to the objectification
        val subjC = individuals.getConceptFor(subjName)
        val objC = individuals.getConceptFor(objName)
        (subjC, objC) match {
            case (Some(sc), Some(oc)) =>
                properties.objectifyAssociation(sc, objFact, oc)
            case _ => ()
        }

        properties.objectifyAssociation(subj, objFact, obj)

        val subjectID = taxonomyScheme.prefixed(subjName)
        val txt = s"Models an association between $subjectID and $objName, created-by krom"
        annotations.assertSkosNote(txt, objFact)
        annotations.assertRdfsSeeAlso(morkAssociativeBroadConceptRoleIRI, objFact)
        annotations.assertRdfsSeeAlso(morkAssociativeNarrowConceptRoleIRI, objFact)

}

object Ontology {
    def openOntology(config: KromConfig): Ontology = {
        val manager = OWLManager.createOWLOntologyManager()
        val ontology = manager.createOntology(IRI.create(config.kromBaseIRI))
        val ont = new Ontology(config, ontology, manager.getOWLDataFactory, manager)
        ont.init()
    }
}

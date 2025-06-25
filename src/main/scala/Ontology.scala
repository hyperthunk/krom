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

class Ontology private (val config: KromConfig, val ontology: OWLOntology,
                        val manager: OWLOntologyManager) extends LazyLogging {

    private val counter = new AtomicLong(Integer.MAX_VALUE)
    private val factory: OWLDataFactory = manager.getOWLDataFactory
    private val morkIRI = IRI.create(config.morkURL)
    private val morkBaseIRI = IRI.create(config.morkBaseIRI)
    private val kromBaseIRI = IRI.create(config.kromBaseIRI)
    private val skosBaseIRI = IRI.create(config.skosBaseIRI)
    private val morkPrefixManager = new DefaultPrefixManager(null, null, config.morkBaseIRI + "#")
    private val mappingPrefixManager = new DefaultPrefixManager(null, null, config.kromBaseIRI + "#")
    private val anonymousEntities: mutable.Set[String] = mutable.HashSet.empty

    private val morkAttribute: OWLClass = getMorkClass(":Attribute")
    private val morkAttributeDef: OWLClassExpression = morkAttribute.getNNF
    private val morkAnonEntity: OWLClass = getMorkClass(":AnonymousEntity")
    private val morkAnonEntityDef: OWLClassExpression = morkAnonEntity.getNNF
    private val morkEntity: OWLClass = getMorkClass(":Entity")
    private val morkEntityDef: OWLClassExpression = morkEntity.getNNF
    private val morkArray: OWLClass = getMorkClass(":Array")
    private val morkArrayDef: OWLClassExpression = morkArray.getNNF

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
        assertObjectProperty(broader, child, ":memberProperty")
    }

    def addRepEntityAnon(to: String): String = {
        if anonymousEntities.contains(to) then to
        else
            val id = anonID
            anonymousEntities += id
            val individual = assertIndividual(id, morkAnonEntityDef, noIdent = true)
            assertObjectProperty(to, individual, ":elementType")
            id
        // TODO: add a SWRL rule to prevent duplicate element types for anonymous objects in arrays
    }

    def addRepresentationEntity(id: String): Unit = assertIndividual(id, morkEntityDef)

    def addRepresentationArray(id: String): Unit = assertIndividual(id, morkArrayDef)

    def addRepresentationAttribute(where: String, what: String, underlying: Scalar): Unit = {
        val individual = assertIndividual(what, morkAttributeDef)
        assertObjectProperty(where, individual, ":memberProperty")

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

    private def assertObjectProperty(broader: String, child: OWLIndividual, propertyName: String): Unit = {
        val parent = getNamedIndividual(broader)
        val expr = factory.getOWLObjectProperty(propertyName, morkPrefixManager)
        val propAssertion = factory.getOWLObjectPropertyAssertionAxiom(expr, parent, child)

        manager.applyChange(AddAxiom(ontology, propAssertion))
    }

    private def assertIndividual(id: String, ofClass: OWLClassExpression,
                                 noIdent: Boolean = false): OWLNamedIndividual = {
        val individual = getNamedIndividual(id)
        val clsAssertion = factory.getOWLClassAssertionAxiom(ofClass, individual)

        manager.applyChange(AddAxiom(ontology, clsAssertion))

        if noIdent then identifyIndividual("", individual)
        else identifyIndividual(id, individual)
    }

    private def getNamedIndividual(id: String) = {
        factory.getOWLNamedIndividual(":" + prefix(id), mappingPrefixManager)
    }

    // TODO: SWRL (or SHACL) axioms to correlate anonymous members that share the same type def
    private def anonID: String = prefix("Anon_" + counter.incrementAndGet().toString)

    @inline
    private def prefix(id: String): String = {
        // TODO: configurable prefix/suffix setup?
        if id.startsWith("Rep_") then id
        else "Rep_" + id
    }
}

object Ontology {
    def openOntology(config: KromConfig): Ontology = {
        // openOntology(IRI.create(config.morkURL))
        val mgr = OWLManager.createOWLOntologyManager()
        val ontology = mgr.createOntology(IRI.create(config.kromBaseIRI))
        val ont = new Ontology(config, ontology, mgr)
        ont.init()
        ont
    }
}

package org.nebularis.krom

import org.semanticweb.owlapi.model.{AddAxiom, OWLDataFactory, OWLDataProperty, OWLDataPropertyAssertionAxiom, OWLDatatype, OWLNamedIndividual, PrefixManager}
import org.semanticweb.owlapi.util.DefaultPrefixManager
import org.semanticweb.owlapi.vocab.OWL2Datatype

sealed trait DataType {
    def toOwlDataType(factory: OWLDataFactory): OWLDatatype = this match {
        case XsdString => OWL2Datatype.XSD_STRING.getDatatype(factory)
        case XsdInt => OWL2Datatype.XSD_INT.getDatatype(factory)
        case XsdFloat => OWL2Datatype.XSD_FLOAT.getDatatype(factory)
        case XsdBool => OWL2Datatype.XSD_BOOLEAN.getDatatype(factory)
        case XsdNull => null // TODO: throw!?
    }
}
case object XsdString extends DataType
case object XsdInt extends DataType
case object XsdFloat extends DataType
case object XsdBool extends DataType
case object XsdNull extends DataType


trait Typeable[A]:
    extension (a: A) def asType[T]: T
    extension (b: A) def asString: String
    extension (c: A) def xsdType: DataType

class Scalar(val value: Any) {
    override def toString: String = "JsonValue(" + value.toString + ")"

    def assertDatatypeProperty(propertyKey: String,
                               individual: OWLNamedIndividual,
                               factory: OWLDataFactory,
                               mappingPrefixManager: PrefixManager): Option[OWLDataPropertyAssertionAxiom] = {
        val dpEx = factory.getOWLDataProperty(propertyKey, mappingPrefixManager)
        this.value match {
            case i: Int => Some(write(i, dpEx, individual, factory))
            case f: Float => Some(write(f, dpEx, individual, factory))
            case d: Double => Some(write(d, dpEx, individual, factory))
            case b: Boolean => Some(write(b, dpEx, individual, factory))
            case s: String => {
                if s == null then None
                else Some(write(s, dpEx, individual, factory))
            }
        }
    }

    def write(s: String,
              dpEx: OWLDataProperty,
              individual: OWLNamedIndividual,
              factory: OWLDataFactory): OWLDataPropertyAssertionAxiom = {
        factory.getOWLDataPropertyAssertionAxiom.apply(dpEx, individual, s)
    }

    def write(i: Int,
              dpEx: OWLDataProperty,
              individual: OWLNamedIndividual,
              factory: OWLDataFactory): OWLDataPropertyAssertionAxiom = {
        factory.getOWLDataPropertyAssertionAxiom.apply(dpEx, individual, i)
    }

    def write(f: Float,
              dpEx: OWLDataProperty,
              individual: OWLNamedIndividual,
              factory: OWLDataFactory): OWLDataPropertyAssertionAxiom = {
        factory.getOWLDataPropertyAssertionAxiom.apply(dpEx, individual, f)
    }

    def write(d: Double,
              dpEx: OWLDataProperty,
              individual: OWLNamedIndividual,
              factory: OWLDataFactory): OWLDataPropertyAssertionAxiom = {
        factory.getOWLDataPropertyAssertionAxiom.apply(dpEx, individual, d)
    }

    def write(b: Boolean,
              dpEx: OWLDataProperty,
              individual: OWLNamedIndividual,
              factory: OWLDataFactory): OWLDataPropertyAssertionAxiom = {
        factory.getOWLDataPropertyAssertionAxiom.apply(dpEx, individual, b)
    }
}
object Scalar {
    def wrap(any: Any): Scalar = Scalar(any)
}
private object JsonNull extends Scalar(Some(null))

given Typeable[Scalar] with
    extension (w: Scalar) def asType[T]: T = w.value.asInstanceOf[T]
    extension (w: Scalar) def asString: String = w.value.toString
    extension (w: Scalar) def xsdType: DataType = {
        //TODO: this is SO gross, refactor to use higher order types...
        /* Co-Pilot Suggests:
        trait XsdTypeMapper[A] {
          def xsdType: DataType
        }

        object XsdTypeMapper {
          given XsdTypeMapper[Int] with { def xsdType = XsdInt }
          given XsdTypeMapper[Float] with { def xsdType = XsdFloat }
          given XsdTypeMapper[Double] with { def xsdType = XsdFloat } // Or a Double variant
          given XsdTypeMapper[Boolean] with { def xsdType = XsdBool }
          given XsdTypeMapper[String] with { def xsdType = XsdString }
        }

        class Scalar(val value: Any) {
          override def toString: String = s"JsonValue($value)"
          def xsdType: DataType = value match {
            case null      => XsdNull
            case v: Int    => summon[XsdTypeMapper[Int]].xsdType
            case v: Float  => summon[XsdTypeMapper[Float]].xsdType
            case v: Double => summon[XsdTypeMapper[Double]].xsdType
            case v: Boolean=> summon[XsdTypeMapper[Boolean]].xsdType
            case v: String => summon[XsdTypeMapper[String]].xsdType
            case _         => XsdString // fallback
          }
        }
         */
        w.value match {
            case _: Int => XsdInt
            case _: Float => XsdFloat
            case _: Double => XsdFloat
            case _: Boolean => XsdBool
            case _ => if w.value == null then XsdNull else XsdString
        }
    }


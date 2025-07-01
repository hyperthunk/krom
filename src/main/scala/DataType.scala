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

    trait DataPropertyWriter[T] {
        def write(
                     value: T,
                     dpEx: OWLDataProperty,
                     individual: OWLNamedIndividual,
                     factory: OWLDataFactory
                 ): OWLDataPropertyAssertionAxiom
    }

    object DataPropertyWriter {
        given DataPropertyWriter[Int] with {
            def write(value: Int, dpEx: OWLDataProperty, individual: OWLNamedIndividual, factory: OWLDataFactory) =
                factory.getOWLDataPropertyAssertionAxiom(dpEx, individual, value)
        }

        given DataPropertyWriter[Float] with {
            def write(value: Float, dpEx: OWLDataProperty, individual: OWLNamedIndividual, factory: OWLDataFactory) =
                factory.getOWLDataPropertyAssertionAxiom(dpEx, individual, value)
        }

        given DataPropertyWriter[Double] with {
            def write(value: Double, dpEx: OWLDataProperty, individual: OWLNamedIndividual, factory: OWLDataFactory): OWLDataPropertyAssertionAxiom =
                factory.getOWLDataPropertyAssertionAxiom(dpEx, individual, value)
        }

        given DataPropertyWriter[Boolean] with {
            def write(value: Boolean, dpEx: OWLDataProperty, individual: OWLNamedIndividual, factory: OWLDataFactory): OWLDataPropertyAssertionAxiom =
                factory.getOWLDataPropertyAssertionAxiom(dpEx, individual, value)
        }

        given DataPropertyWriter[String] with {
            def write(value: String, dpEx: OWLDataProperty, individual: OWLNamedIndividual, factory: OWLDataFactory): OWLDataPropertyAssertionAxiom =
                factory.getOWLDataPropertyAssertionAxiom(dpEx, individual, value)
        }
    }

    private def writeValue[T](value: T,
                              dpEx: OWLDataProperty,
                              individual: OWLNamedIndividual,
                              factory: OWLDataFactory)
                             (using writer: DataPropertyWriter[T]) =
        writer.write(value, dpEx, individual, factory)

    def assertDatatypeProperty(
                                  propertyKey: String,
                                  individual: OWLNamedIndividual,
                                  factory: OWLDataFactory,
                                  mappingPrefixManager: PrefixManager
                              ): Option[OWLDataPropertyAssertionAxiom] = {
        val dpEx = factory.getOWLDataProperty(propertyKey, mappingPrefixManager)
        value match {
            case null => None
            case v: Int => Some(writeValue(v, dpEx, individual, factory))
            case v: Float => Some(writeValue(v, dpEx, individual, factory))
            case v: Double => Some(writeValue(v, dpEx, individual, factory))
            case v: Boolean => Some(writeValue(v, dpEx, individual, factory))
            case v: String => if (v == null) None else Some(writeValue(v, dpEx, individual, factory))
            case _ => None
        }
    }
}
object Scalar {
    def wrap(any: Any): Scalar = Scalar(any)
}
private object JsonNull extends Scalar(Some(null))

given Typeable[Scalar] with
    private trait ToDataType[A] {
        def toDataType: DataType
    }

    private object ToDataType {
        given ToDataType[Int] with
            def toDataType: DataType = XsdInt

        given ToDataType[Float] with
            def toDataType: DataType = XsdFloat

        given ToDataType[Double] with
            def toDataType: DataType = XsdFloat // Or a Double variant

        given ToDataType[Boolean] with
            def toDataType: DataType = XsdBool

        given ToDataType[String] with
            def toDataType: DataType = XsdString
    }
    extension (w: Scalar) def asType[T]: T = w.value.asInstanceOf[T]
    extension (w: Scalar) def asString: String = w.value.toString
    extension (w: Scalar) def xsdType: DataType = {
        w.value match {
            case null      => XsdNull
            case _: Int    => summon[ToDataType[Int]].toDataType
            case _: Float  => summon[ToDataType[Float]].toDataType
            case _: Double => summon[ToDataType[Double]].toDataType
            case _: Boolean=> summon[ToDataType[Boolean]].toDataType
            case _: String => summon[ToDataType[String]].toDataType
            case _         => XsdString // fallback
        }
    }


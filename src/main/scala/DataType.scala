package org.nebularis.krom

import org.semanticweb.owlapi.model.{OWLDataFactory, OWLDatatype}
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

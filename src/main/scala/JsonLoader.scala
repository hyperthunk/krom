package org.nebularis.krom

import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.core.JsonToken.*
import com.typesafe.scalalogging.LazyLogging

import java.io.InputStream
import scala.annotation.{tailrec, unused}
import scala.language.postfixOps

type JacksonParser = com.fasterxml.jackson.core.JsonParser

class JsonLoader(private val io: InputStream,
                 private val factory: JsonFactory) extends Logger("JsonLoader") {

    def load(ontology: Ontology): Unit =
        val parser = factory.createParser(io)
        parse(parser, JBOFState(parser, ontology))

    @tailrec
    private def parse(parser: JacksonParser, state: ParserState): Unit =
        val token = parser.nextToken()
        if token == null then ()
        else
            val id = parser.getCurrentName
            debug(("parse", token, id))
            parse(parser, state.nextState(id, token))

}

object JsonLoader {
    private val factory: JsonFactory = new JsonFactory()

    def load(config: KromConfig): Ontology =
        val ontology = Ontology.openOntology(config)
        config.withInputStream(load.apply(_, ontology))

    private def load(io: InputStream, ontology: Ontology): Ontology =
        new JsonLoader(io, factory).load(ontology)
        ontology

}

/** Base exception for throwing parsing failures as RuntimeExceptions.
 */
class JsonParserException(msg: String) extends RuntimeException(msg)

private sealed trait Logger(id: String) extends LazyLogging {
    def debug(op: String, token: JsonToken): Unit =
        logger.whenDebugEnabled {
            logger.debug("Op: " + op + ", Token: " + token + ", id: " + id)
        }

    def debug(msg: Any): Unit = logger.whenDebugEnabled { logger.debug(msg.toString) }
}

trait ParserState(outer: ParserState = null, parser: JacksonParser, ontology: Ontology) extends Logger {
    def nextState(id: String, token: JsonToken): ParserState = this

    protected def invalidState[T >: ParserState](id: String, token: JsonToken): T = {
        throwJPEx(this, "Invalid State: " + id + "/" + token)
    }

    def throwJPEx[T](@unused scope: T, msg: String): T = {
        val ex = new JsonParserException(msg)
        logger.error(msg, ex)
        throw ex
    }
}

case class JBOFState(parser: JacksonParser, ontology: Ontology)
        extends ParserState(parser = parser, ontology = ontology), Logger("BOF") {
    override def nextState(id: String, token: JsonToken): ParserState = {
        token match {
            case START_OBJECT => JObjState(id, this, parser, ontology)
            case START_ARRAY => JListState(id, this, parser, ontology)
            case _ => invalidState(id, token)
        }
    }
}

case class JObjState(id: String, outer: ParserState, parser: JacksonParser, ontology: Ontology)
        extends ParserState(outer, parser, ontology), Logger(id) {
    override def nextState(id: String, token: JsonToken): ParserState = {
        token match {
            case END_OBJECT => outer
            case FIELD_NAME =>
                debug((id, token))
                JPropState(this, parser, ontology)
            case VALUE_EMBEDDED_OBJECT =>
                debug("VALUE_EMBEDDED_OBJECT - CURRENTLY UNSUPPORTED")
                throwJPEx(this, "VALUE_EMBEDDED_OBJECT @" + id)
            case _ => invalidState(id, token)
        }
    }
}

case class JListState(listID: String, outer: JBOFState | JPropState, parser: JacksonParser, ontology: Ontology)
        extends ParserState(outer, parser, ontology), Logger("ARRAY") {

    override def nextState(id: String, token: JsonToken): ParserState = {
        (token, token.isScalarValue, outer) match {
            case (START_OBJECT, _, _: JBOFState) => JObjState(id, this, parser, ontology)
            case (START_OBJECT, _, _: JPropState) =>
                debug((listID, token))
                val repID = ontology.addRepEntityAnon(listID)
                JObjState(repID, this, parser, ontology)
            case (END_OBJECT, _, _) => this
            case (END_ARRAY, _, _) => outer
            case (_, true, _) =>
                def addAttr[T](att: T): ParserState = {
                    val underlying = Scalar(att)
                    debug(("write value", id, underlying.asType[T], underlying.xsdType, token))
                    ontology.addCollectionElement(listID, underlying.xsdType)
                    this
                }
                debug((listID, token))
                token match {
                    case VALUE_STRING => addAttr[String](parser.getValueAsString)
                    case VALUE_NUMBER_INT => addAttr[Int](parser.getValueAsInt)
                    case VALUE_NUMBER_FLOAT => addAttr[Double](parser.getValueAsDouble)
                    case VALUE_FALSE => addAttr[Boolean](parser.getValueAsBoolean)
                    case VALUE_TRUE => addAttr[Boolean](parser.getValueAsBoolean)
                    case _ => this
                }
            // TODO: if we see a scalar value then we should keep a list of potential type bindings
            case _ => invalidState(id, token)
        }
    }
}

case class JPropState(outer: JObjState, parser: JacksonParser, ontology: Ontology)
        extends ParserState(outer, parser, ontology), Logger(outer.id) {

    override def nextState(id: String, token: JsonToken): ParserState = {

        def addRepProp(sid: String)(ent: String => Unit): Unit = {
            ent(sid)
            if outer.id != null then ontology.addMemberProperty(outer.id, sid)
        }

        def addAttr[T](att: T): ParserState = {
            val underlying = Scalar(att)
            debug(("write value", id, underlying.asType[T], underlying.xsdType, token))
            ontology.addRepresentationAttribute(outer.id, id, underlying)
            this
        }

        token match {
            case START_OBJECT =>
                debug((id, token))
                addRepProp(id) { ontology.addRepresentationEntity.apply }
                JObjState(id, this, parser, ontology)
            case START_ARRAY =>
                debug((id, "array", token))
                addRepProp(id) { ontology.addRepresentationArray.apply }
                JListState(id, this, parser, ontology)
            case END_OBJECT => outer.nextState(id, token)
            case FIELD_NAME =>
                debug((id, token))
                JPropState(outer, parser, ontology)
            case VALUE_STRING => addAttr[String](parser.getValueAsString)
            case VALUE_NUMBER_INT => addAttr[Int](parser.getValueAsInt)
            case VALUE_NUMBER_FLOAT => addAttr[Double](parser.getValueAsDouble)
            case VALUE_FALSE => addAttr[Boolean](parser.getValueAsBoolean)
            case VALUE_TRUE => addAttr[Boolean](parser.getValueAsBoolean)
            case VALUE_NULL =>
                val underlying = JsonNull
                debug(("write value", id, underlying, underlying.xsdType, token))
                ontology.addRepresentationAttribute(outer.id, id, underlying)
                this
            case VALUE_EMBEDDED_OBJECT =>
                debug("VALUE_EMBEDDED_OBJECT - CURRENTLY UNSUPPORTED")
                throwJPEx(this, "VALUE_EMBEDDED_OBJECT @" + id)
            case NOT_AVAILABLE =>
                debug("NON_BLOCKING I/O OP - CURRENTLY UNSUPPORTED")
                throwJPEx(this, "Non blocking I/O @" + id)
            case _ =>
                invalidState(id, token)
        }
    }
}

/*
package org.nebularis.krom

import JsonParserException.{throwInvalid, throwUnsupported}

import com.fasterxml.jackson.core.*
import com.fasterxml.jackson.core.JsonToken.*
import com.typesafe.scalalogging.{LazyLogging, Logger}
import java.io.InputStream
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.Stack
import scala.collection.{immutable, mutable}

type JacksonParser = com.fasterxml.jackson.core.JsonParser

/** Base exception for throwing parsing failures as RuntimeExceptions.
 */
class JsonParserException(msg: String) extends RuntimeException(msg)

// companion object
object JsonParserException {
    @inline
    def throwInvalid(msg: String, place: String): JsonElem =
        throwJPEx(msg + " [" + place + "]")

    @inline
    def throwInvalid(place: String): JsonElem =
        throwInvalid("Invalid Json Detected", place)

    @inline
    def throwInvalid(token: JsonToken, path: String): JsonElem =
        throwJPEx("Unexpected token [" + token + "] at path [" + path + "]")

    @inline
    def throwUnsupported(token: JsonToken, path: String): JsonElem =
        throwJPEx("Unsupported Operation (token [" + token + "]) at path [" + path + "]")

    @inline
    def throwInvalidState(token: JsonToken, state: JsonThing, addMsg: Option[String] = None): Unit =
        throwJPEx("Invalid Parse State: " + state + " (token: " + token + ")" + addMsg.getOrElse(""))

    private def throwJPEx(msg: String): JsonElem = {
        val ex = new JsonParserException(msg)
        Parser.log.error(msg, ex)
        throw ex
        JsonRoot()
    }
}

// leverage lazy logging in your classes - takes a `Paths` to facilitate nice messages
private sealed trait Logging(val paths: Paths) extends LazyLogging {
    def debug(token: JsonToken, cursor: JsonElem): Unit = {
        logger.whenDebugEnabled {
            Parser.log.debug("Token: " + token + ", path: " + paths.path + ", Cursor: " + cursor)
        }
    }

    def debug(op: String, token: JsonToken, cursor: JsonThing): Unit = {
        logger.whenDebugEnabled {
            Parser.log.debug("Token: " + token + ", path: " + paths.path + ", Cursor: " + cursor)
        }
    }

    def debug(msg: Any): Unit = {
        logger.whenDebugEnabled{
            Parser.log.debug(msg.toString)
        }
    }
}

/** Provides a parsing function, based on the Jackson streaming parser
 */
class Parser private(private val io: InputStream, private val factory: JsonFactory)
        extends Logging(new Paths(mutable.Stack.empty)) {

    // first attempt at an implementation (IGNORE)
    def parse(): JsonElem = {
        val root = JsonRoot()
        val cursor = parse(root, root, paths, factory.createParser(io))
        debug(cursor)
        root
    }

    def parse2(): ParserState = {
        val state = ParserState()
        parse2(state, factory.createParser(io))
        state
    }

    // better attempt at an implementation
    private final def parse2(state: ParserState, parser: JacksonParser): Unit = {
        val token = parser.nextToken()
        if token == null then ()
        else {
            (token.isScalarValue, token) match {
                case (false, _) =>
                    state.parse(token, parser.getCurrentName)
                case (true, VALUE_STRING) =>
                    state.parse(token, parser.getCurrentName, parser.getValueAsString)
                case (true, VALUE_NUMBER_INT) =>
                    state.parse(token, parser.getCurrentName, parser.getValueAsInt)
                case (true, VALUE_NUMBER_FLOAT) =>
                    state.parse(token, parser.getCurrentName, parser.getFloatValue)
                case (true, VALUE_FALSE) =>
                    state.parse(token, parser.getCurrentName, parser.getBooleanValue)
                case (true, VALUE_TRUE) =>
                    state.parse(token, parser.getCurrentName, parser.getBooleanValue)
                case (true, VALUE_NULL) =>
                    state.parse(token, parser.getCurrentName, JsonNull)
                case (true, VALUE_EMBEDDED_OBJECT) =>
                    throwInvalid(token, state.paths.path)
                case (_, NOT_AVAILABLE) =>
                    debug("NON_BLOCKING I/O OP - CURRENTLY UNSUPPORTED")
                    throwInvalid("Non blocking I/O", state.paths.toString)
                case (true, FIELD_NAME) =>
                    state.parse(token, parser.getCurrentName)
                case (true, _) =>
                    throwInvalid(token, state.paths.path)
            }
            parse2(state, parser)
        }
    }

    // first (inefficient and wrong) approach (IGNORE)
    @tailrec
    private final def parse(root: JsonElem, cursor: JsonElem,
                            paths: Paths, parser: JacksonParser) : JsonElem = {

        def field(fn: String, el: JsonElem) = {
            paths.appendPath(fn)
            JsonElem.elem(el)(JsonField.apply(fn, paths.path, _))
        }

        def jsonValue(jf: JsonField, v: Any) = {
            if v == null then JsonElem.value(jf)(JsonValue.apply(JsonNull, _))
            else JsonElem.value(jf)(JsonValue.apply(wrap(v), _))
        }

        def jsonArrElem(prev: JsonElem): JsonElem = {
            println("starting arr elem " + paths.path)
            paths.appendIndex()
            JsonElem.elem(prev)(JsonObj.apply)
        }

        val token = parser.nextToken()
        if token == null
            then cursor
        else
            val node = (token.isScalarValue, token, cursor) match {
                case (false, START_OBJECT, JsonArray(_)) =>
                    // array elements belong within the array (which is the current cursor)
                    debug(token, cursor); paths.appendIndex(); JsonElem.elem(cursor)(JsonObj.apply)
                case (false, START_OBJECT, _) =>
                    debug(token, cursor); JsonElem.elem(cursor)(JsonObj.apply)  //NB: currying is not lazy here

                /*    we have a field-Name processing error
                    see contract.conditions.exclCosts.coolingOffPeriodDays.maximumNonNegotiatedPayment


Token: FIELD_NAME, path: Left(contract), Cursor: JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
Token: START_OBJECT, path: contract.conditions, Cursor: JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
Token: FIELD_NAME, path: contract.conditions, Cursor: JsonObj[parent=JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]]
Token: VALUE_STRING, path: contract.conditions.exclCosts, Cursor: JsonField[id=exclCosts, parent=JsonObj[parent=JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]]
Token: FIELD_NAME, path: contract.conditions.exclCosts, Cursor: JsonField[id=exclCosts, parent=JsonObj[parent=JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]]
Token: VALUE_NUMBER_INT, path: contract.conditions.exclCosts.coolingOffPeriodDays, Cursor: JsonField[id=coolingOffPeriodDays, parent=JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
Token: FIELD_NAME, path: contract.conditions.exclCosts.coolingOffPeriodDays, Cursor: JsonField[id=coolingOffPeriodDays, parent=JsonField[id=conditions, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
Token: VALUE_NUMBER_FLOAT, path: contract.conditions.exclCosts.coolingOffPeriodDays.maximumNonNegotiatedPayment, Cursor: JsonField[id=maximumNonNegotiatedPayment, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
Token: END_OBJECT, path: contract.conditions.exclCosts.coolingOffPeriodDays.maximumNonNegotiatedPayment, Cursor: JsonField[id=maximumNonNegotiatedPayment, parent=JsonObj[parent=JsonField[id=contract, parent=JsonObj[parent=]]
popped Left(maximumNonNegotiatedPayment) - remaining[Stack(Left(coolingOffPeriodDays), Left(exclCosts), Left(conditions), Left(contract))]
popped Left(coolingOffPeriodDays) - remaining[Stack(Left(exclCosts), Left(conditions), Left(contract))]
Token: END_OBJECT, path: contract.conditions.exclCosts, Cursor: JsonField[id=contract, parent=JsonObj[parent=]

                */

                case (false, FIELD_NAME, JsonObj(_)) =>
                    debug(token, cursor); field(parser.getCurrentName, cursor)
                case (false, FIELD_NAME, JsonField(_, _, JsonObj(JsonArray(_)))) =>
                    // a new field implies we attach the parent (of the cursor) as 'up'
                    debug(token, cursor); paths.pop; field(parser.getCurrentName, cursor.up)
                case (false, FIELD_NAME, JsonField(_, _, JsonObj(_))) =>
                    debug(token, cursor); paths.pop; field(parser.getCurrentName, cursor.up.up)
                case (false, FIELD_NAME, JsonField(_, _, prev)) =>
                    // a new field implies we attach the parent (of the cursor) as 'up'
                    debug(token, cursor); field(parser.getCurrentName, prev.up)
                // TODO: add this as a precondition
                // NOTE: remediate this by never putting a JsonValue into parse/4 in the position of the cursor
                // case (false, FIELD_NAME, JsonValue(_, prev)) => p(token, cursor); field(parser.getCurrentName, prev)
                case (false, FIELD_NAME, _) =>
                    debug(token, cursor); throwInvalid(paths.path(parser.getCurrentName))
                case (false, END_OBJECT, JsonObj(prev)) =>
                    debug(token, cursor); paths.pop; prev
                case (false, END_OBJECT, JsonField(_, _, JsonObj(JsonArray(_)))) =>
                    // if the object is an element in an array, we need to keep the array index in the path...
                    debug(token, cursor); paths.pop; cursor.up.up
                case (false, END_OBJECT, JsonField(_, _, prev)) =>
                    debug(token, cursor); paths.pop; prev.up
                // TODO: add this as a precondition
                // NOTE: remediate this by never putting a JsonValue into parse/4 in the position of the cursor
                // case (false, END_OBJECT, JsonValue(_, prev)) => p(token, cursor); paths.pop; paths.pop; prev.up.up.up
                case (false, END_OBJECT, _) =>
                    // this assumes we will never see END_ARRAY before END_OBJECT!
                    debug(token, cursor);
                    if cursor.eq(root) then return root
                    else throwInvalid(token, paths.path)
                case (false, START_ARRAY, _) =>
                    debug(token, cursor); paths.startIndex(); JsonElem.elem(cursor)(JsonArray.apply)
                case (false, END_ARRAY, JsonArray(prev)) =>
                    debug(token, cursor); paths.resetIndex(); prev
                case (false, END_ARRAY, JsonField(_, _, prev)) =>
                    debug(token, cursor); paths.pop; prev
                case (false, END_ARRAY, JsonValue(_, prev)) =>
                    debug(token, cursor); paths.pop; prev.up
                case (false, END_ARRAY, JsonObj(prev)) =>
                    debug(token, cursor); paths.pop; prev.up.up
                case (false, END_ARRAY, _) =>
                    debug(token, cursor); throwInvalid(token, paths.path(parser.getCurrentName))
                case (_, VALUE_STRING, jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, parser.getValueAsString); jf
                case (_, VALUE_NUMBER_INT, jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, parser.getValueAsInt); jf
                case (_, VALUE_NUMBER_FLOAT, jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, parser.getFloatValue); jf
                case (_, VALUE_FALSE, jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, parser.getBooleanValue); jf
                case (_, VALUE_TRUE , jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, parser.getBooleanValue); jf
                case (_, VALUE_NULL, jf: JsonField) =>
                    debug(token, cursor); jsonValue(jf, null); jf
                case (_, NOT_AVAILABLE, _) =>
                    throwInvalid(token, paths.path(parser.getCurrentName))
                case (_, _, _) =>
                    throwUnsupported(token, paths.path(parser.getCurrentName))
            }
            parse(root, node, paths, parser)
    }
}

object Parser {
    private val factory: JsonFactory = new JsonFactory()
    val log: Logger = Logger(classOf[Parser])
    def parse(io: InputStream): Parser = new Parser(io, factory)
}

type Path = String

// Ensures the creation of a consistent graph, enforcing valid json and
// keeping track of the paths for every item in the structure
private sealed class ParserState extends Logging(Paths(mutable.Stack.empty)) {
    // TODO: make `stack' immutable
    private val stack: mutable.Stack[JsonThing] = mutable.Stack.empty
    private val objects: mutable.Stack[JsonThing] = mutable.Stack.empty
    private val arrays: mutable.Stack[JsonThing] = mutable.Stack.empty
    private var processed: Queue[Op[JsonThing]] = immutable.Queue.empty
    private var scope: Option[JsonThing] = None

    def nodes(): List[JsonThing] =
        processed.foldLeft(List.empty) { (list, op) => op.jsonThing :: list }

    def parse(token: JsonToken, id: String): Unit = {
        scope = Some(nextState(token, id)(paths.path))
        recordStateTransition(Push(scope.get))
    }

    // parse a scalar value from the underlying json stream
    def parse(token: JsonToken, id: String, jsonVal: Any): Unit = {
        val scopeR = scope
        scopeR match {
            case Some(prop: JsonProperty) => {
                // don't touch the path, just add the value
                stack.push(prop)
                recordStateTransition(Push(prop))

                val jVal = JsonVal(Scalar(jsonVal), prop.name, prop.path)
                stack.head.compositeNarrower(jVal)

                // JsonAssociation is never appended - it only marks the cursor
                val assoc = JsonAssociation(prop, jVal, prop.id)
                scope = Some(assoc)
                recordStateTransition(Mark(assoc))
            }
            case Some(arr: JsonList) => {
                // we are a scalar value within an array - do not alter the stack/state
                // since scalar/literal values are leaves in the document structure
                paths.appendIndex()
                val idx = paths.currentId.getOrElse(0).toString
                arr.compositeNarrower(JsonVal(Scalar(jsonVal), idx, paths.path))
            }
            case Some(JsonElement(JsonList(_, _), prop: JsonProperty, _, _)) =>
                // we are a property of an anonymous object within an array
                scope = Some(prop)
                parse(token, id, jsonVal)
                scope = scopeR
            case _ =>
                throwInvalidStateM(token, scope.get,
                    Some("Unable to parse json value (" + jsonVal.toString +
                         ") at path: " + paths.path(id)))
        }
    }

    // parse a json element from the underlying stream - excludes scalar values
    private def nextState(token: JsonToken, id: String): Path => JsonThing = {
        def wrapCompose(fn: Path => JsonThing): Path => JsonThing =
            wrap.apply(fn) { stack.head.compositeNarrower }

        def wrap(fn: Path => JsonThing)
                (innerFn: JsonThing => Unit): Path => JsonThing = (path) => {
            val thing = fn(path)
            innerFn(thing)
            thing
        }

        def resetStack(pred: JsonThing => Boolean): Path => JsonThing = {
            resetStackToLastInstance("Roll Up Stack", pred, token)
            validatePath.apply(_)
        }

        def validatePath = { (p: Path) =>
            val hd = stack.head
            if hd.path != p then {
                throwInvalidStateM(token, hd)
                hd // crying for liftM
            } else hd
        }

        // another compelling reason to move to a stateM + arrows :(
        val scopeM = scope

        (stack.isEmpty, scopeM, token) match {
            // special cases for the root object or array
            case (true, None, START_ARRAY) => {
                paths.startIndex()
                wrap(JsonList.apply(id, _)) { arrays.push }
            }
            case (true, None, START_OBJECT) =>
                debug("Initial object ID: " + id)
                wrap(JsonObject.apply(id, _)) { objects.push }
            // preconditions of the stack + scope
            case (true, None, _) =>
                throwInvalidStateM(token, JsonBOF)
            case (false, None, FIELD_NAME) =>
                throwInvalidStateM(token, stack.head)
            // constructors for json properties
            case (_, Some(JsonObject(_, _)), FIELD_NAME) => {
                paths.appendPath(id)
                val sc = scopeM.get
                stack.push(sc)
                recordStateTransition(Push(sc))

                // ensure the head of the stack is connected to this "narrower" json data
                wrapCompose(JsonProperty.apply(id, _))
            }
            // a field name adjunct an association is a sibling field in an object
            case (false, Some(JsonAssociation(prop, _, _)), FIELD_NAME) => {
                // JsonAssociations are never added to the stack - they are markers only
                // therefore the head of the stack will be 'prop'
                val hd = stack.pop
                // TODO: it is gross that paths.pop has side-effects (!) use state monad instead
                precondition(paths.pop.exists(hd.name.equals(_)), "Paths Invalid", id)
                precondition(hd.eq(prop), "Invalid Stack (JsonAssociation)", id)
                precondition(prop.name == hd.name, "Association Property Name Match", id)
                recordStateTransition(Pop(hd))

                // now we roll-up from the property to the containing object
                val obj = stack.pop
                // condition(obj.path == paths.path(hd.name), "Paths Invalid", id)
                recordStateTransition(Pop(obj))
                postcondition(paths.pop.exists(obj.name.equals(_)), "Paths Invalid", id)

                // now we're in good shape to add the new property/field-name
                scope = Some(obj)
                nextState(token, id)
            }
            // field name which is a sibling property for an object nested in an arry
            case (false, Some(JsonElement(arr: JsonList, prop: JsonProperty, _, _)), FIELD_NAME) =>
                // TODO: dedup this
                // JsonElements are never added to the stack - they are markers only
                // therefore the head of the stack will be 'prop'
                val hd = stack.pop
                val path = paths.pop
                precondition(path.isLeft && path.left.exists(hd.name.equals(_)), "Paths Invalid", id)
                precondition(hd.eq(prop), "Invalid Stack (JsonAssociation)", id)
                precondition(prop.name == hd.name, "Association Property Name Match", id)
                recordStateTransition(Pop(hd))

                // now we roll-up from the property to the containing object
                val obj = stack.pop

                // condition(obj.path == paths.path(hd.name), "Paths Invalid", id)
                recordStateTransition(Pop(obj))
                postcondition(paths.pop.exists(obj.name.equals(_)), "Paths Invalid", id)

                scope = Some(obj)
                val propN = nextState(token, id).apply(paths.path)
                stack.push(propN)
                recordStateTransition(Push(propN))

                // ensure the head of the stack is connected to this "narrower" json data
                { (p: Path) =>
                    obj.compositeNarrower(prop)

                    // scope needs to be an array pointing to the property
                    JsonElement(arr, propN, id, p)
                }
            // JsonElement(JsonList(contractingParties,contract.signatories.issuer.contractingParties[-1]),JsonObject(null,contract.signatories.issuer.contractingParties[0]),null,contract.signatories.issuer.contractingParties[0])
            // field name for a json object nested within an array
            case (false, Some(JsonElement(arr, obj: JsonObject, _, _)), FIELD_NAME) =>
                val hd = stack.head
                logger.whenDebugEnabled {
                    debug("compare " + hd + " with " + obj)
                }
                condition(hd.eq(arr), "'Object in Array' / array must point to stack", id)
                paths.appendPath(id)

                // issuer is getting issuerID *AND* issuer.contractingParties (latter is wrong)
                // Prop contract.signatories.issuer.contractingParties (!) is getting allocated
                // JsonList(id contractingParties, path issuer.contractingParties)
                // jsonList(members[0] = JObj(contract.signatories.issuer[0])

                // so paths are all fucked up

                // maybe stop computing the paths?
                // also objects are going in the wrong place...

                // ensure the head of the stack is connected to this "narrower" json data
                { (p: Path) =>
                    val prop = JsonProperty(id, p)
                    obj.compositeNarrower(prop)

                    // scope needs to be an array pointing to the property
                    JsonElement(arr, prop, id, p)
                }

            // catch the case where we see a field name but aren't "inside" an object
            case (false, Some(_), FIELD_NAME) =>
                throwInvalidStateM(token, scopeM.get, Some("Property defined outside of json object"))
            // properties pointing to objects or arrays - no path adjustment, as prop is set already
            case (false, Some(p: JsonProperty), START_OBJECT) =>
                stack.push(p)
                wrap(JsonObject.apply(id, _)) { p.compositeNarrower }
            case (false, Some(p: JsonProperty), START_ARRAY) =>
                stack.push(p)
                wrap(JsonList.apply(id, _)) { p.compositeNarrower }
            // handle non-scalar content in arrays
            case (false, Some(arr: JsonList), START_OBJECT) =>
                stack.push(arr)
                recordStateTransition(Push(arr))
                paths.appendIndex()
                {
                    (p: Path) =>
                        val obj = JsonObject(id, p)
                        arr.compositeNarrower(obj)
                        JsonElement(arr, obj, id, p)
                }
            case (false, Some(JsonElement(arr, _, _, _)), END_OBJECT) =>
                val hd = stack.head
                condition(hd.eq(arr), "Array Element must point to stack", id)
                validatePath
            // reposition the stack when we see the end of an object or array
            case (false, _, END_OBJECT) => {
                // pop the stack until we reach the object
                debug(("Reset Stack [JsonObject]", scopeM, stack, token))
                resetStack {
                    case JsonObject(_, _) => true
                    case _ => false
                }
            }
            case (false, _, END_ARRAY) => {
                // pop the stack until we reach the array
                debug(("Reset Stack [JsonList]", scopeM, stack, token))
                resetStack {
                    case JsonList(_, _) => true
                    case _ => false
                }
            }
            case (_, _, _) =>
                throwInvalidStateM(token, scopeM.get, Some("Undefined State Transition"))
        }
    }

    @inline
    private def precondition(pred: Boolean, explain: String, id: String): Unit =
        condition(pred, explain, id)

    @inline
    private def postcondition(pred: Boolean, explain: String, id: String): Unit =
        condition(pred, explain, id)

    private def condition(pred: Boolean, explain: String, id: String): Unit = {
        if !pred then {
            logger.whenDebugEnabled {
                debug(explain + " (at id:" + id + ")")
            }
            throwInvalid("Precondition Failed: " + explain, paths.path(id))
        }
    }

    // enqueue 'thing' as a transitioned state
    // TODO: this seems a little pointless right now, but will be refactored to an actor/vertex
    // that asynchronously records state transitions as axioms in the attached OWL ontology
    @inline
    def recordStateTransition(thing: Op[JsonThing]): Unit = processed = processed.enqueue(thing)

    // roll up the stack until we get to the last (most recent) instance of T0
    @tailrec
    private def resetStackToLastInstance(op: String, pred: JsonThing => Boolean,
                                         token: JsonToken): Unit = {
        if stack.isEmpty then ()
        else if pred(stack.head) then paths.resetPath(stack.head.name)
        else {
            val hd = stack.pop
            recordStateTransition(Pop(hd))
            debug(op, token, hd)
            resetStackToLastInstance(op, pred, token)
        }
    }

    private def throwInvalidStateM(token: JsonToken, state: JsonThing,
                                   addMsg: Option[String] = None): Path => JsonThing = {
        val msg = "Invalid Parse State: " + state + " (token: " + token + ")" + addMsg.getOrElse("")
        val ex = new JsonParserException(msg)
        Parser.log.error(msg, ex)
        throw ex
        // TODO: we need a `lifted' version of this - maybe use the cats Monad in nextState??
        return null
    }
}

// performs state management for json paths during parsing
// this could easily be replaced by traversing the result structure, however the desire
// to have a pure streaming implementation in the future suggests that's not the way to go
//
private sealed class Paths(private val buff: mutable.Stack[Either[String, Int]]) {

    /*
    *
    * */
    def appendPath(s: String): Unit = buff.push(Left(s))

    def startIndex(): Unit = buff.push(Right(0))

    def appendIndex(): Unit = {
        buff.pop match {
            case Left(s) => startIndex()
            case Right(i) => buff.push(Right(i + 1))
        }
    }

    def resetIndex(): Unit = {
        val hd = buff.pop
        if hd.isLeft then throwInvalid("Invalid index reset: " + path)
    }

    def currentId: Either[String, Int] = buff.head

    @tailrec
    final def resetPath(id: String): Unit =
        if (currentId.isLeft && currentId.equals(Left(id))) then {
            ()
        } else {
            pop
            resetPath(id)
        }

    def pop: Either[String, Int] = {
        val hd = buff.pop()
        // TODO: remove
        Parser.log.debug("popped " + hd + " - remaining[" + buff + "]")
        hd
    }

    override def toString: String = path

    def path: String = {
        if buff.isEmpty then return ""
        if buff.size == 1 then return buff.head.toString
        else {
            val sBuff = buff.foldRight(new StringBuilder()) { (item, sb) =>
                item match {
                    case Left(s: String) => {
                        if sb.isEmpty then sb.append(s)
                        else sb.append(".").append(s)
                    }
                    case Right(i: Int) => sb.append("[").append(i.toString).append("]")
                }
                sb
            }
            sBuff.toString()
        }
    }

    def path(ap: String): String = {
        appendPath(ap)
        path
    }

    def length: Int = buff.size
}

// internal/private type classes, traits, and case classes
trait Showable[A <: JsonThing]:
    extension (a: A) def toString: String

trait Typeable[A]:
    extension (a: A) def asType[T]: T
    extension (b: A) def asString: String
    extension (c: A) def xsdType: String

//
private class Scalar(val value: Any) {
    override def toString: String = "JsonValue(" + value.toString + ")"
}
private object JsonNull extends Scalar(Some(null))
given Typeable[Scalar] with
    extension (w: Scalar) def asType[T]: T = w.value.asInstanceOf[T]
    extension (w: Scalar) def asString: String = w.value.toString
    extension (w: Scalar) def xsdType: String = {
        //TODO: this is gross....
        w.value match {
            case _: Int => "xsd:int"
            case _: Float => "xsd:float"
            case _: Boolean => "xsd:boolean"
            case _ => if w.value == null then "xsd:null" else "xsd:string"
        }
    }

def wrap(any: Any): Scalar = Scalar(any)

// NOTE: it may seem awkward that we tag things like objects and lists with a name
// however this makes path management a lot easier!
abstract class JsonThing(val name: String, val path: Path) {
    private val members: mutable.Queue[JsonThing] = mutable.Queue.empty

    def compositeNarrower(thing: JsonThing): Unit =
        members.append(thing)

    @inline
    def compositeNarrower(prop: JsonProperty, thing: JsonThing): Unit =
        compositeNarrower(JsonAssociation(prop, thing, prop.path))
}
private case object JsonBOF extends JsonThing("", "")
private case object JsonEOF extends JsonThing("", "")
private case class JsonObject(id: String, p: Path) extends JsonThing(id, p)
private case class JsonProperty(id: String, p: Path) extends JsonThing(id, p)
private case class JsonList(id: String, p: Path) extends JsonThing(id, p)
private case class JsonAssociation(prop: JsonProperty, thing: JsonThing,
                                   id: String) extends JsonThing(id, prop.path)
private case class JsonElement(arr: JsonList, obj: JsonThing,
                               id: String, p: Path) extends JsonThing(id, p)
private case class JsonVal(value: Scalar, id: String, p: Path) extends JsonThing(id, p)
given Showable[JsonThing]:
    extension (js: JsonThing) def toString: String = js match {
        case JsonBOF => "Beginning Of File"
        case JsonEOF => "End Of File"
        case _ => js.toString
    }

private sealed trait Op[T <: JsonThing](thing: T) {
    def jsonThing: JsonThing = thing
}
private case class Pop[T <: JsonThing](thing: T) extends Op[T](thing)
private case class Push[T <: JsonThing](thing: T) extends Op[T](thing)
private case class Mark[T <: JsonThing](thing: T) extends Op[T](thing)
given Typeable[Op[JsonThing]] with
    extension (w: Op[JsonThing]) def asType[T]: T = w.asInstanceOf[T]
    extension (w: Op[JsonThing]) def asString: String = w.toString
    extension (w: Op[JsonThing]) def xsdType: String = ""

// JsonElem implementation classes (IGNORE - NOT USED)
private abstract class JsonElem(val up: JsonElem) {
    private var downs: Vector[JsonElem] = Vector()

    override def toString: String = if up == null then "" else up.toString
    def children: Vector[JsonElem] = downs
}
private case class JsonRoot() extends JsonElem(null)
private case class JsonObj(e: JsonElem) extends JsonElem(e) {
    override def toString: String = "JsonObj[parent=" + e.toString + "]"
}
private case class JsonField(id: String, path: String, e: JsonElem) extends JsonElem(e) {
    override def toString: String = "JsonField[id=" + id + ", parent=" + e.toString
}
private case class JsonArray(e: JsonElem) extends JsonElem(e) {
    override def toString: String = "JsonArray[parent=" + e.toString + "]"
}
private case class JsonValue(value: Scalar, e: JsonField | JsonArray) extends JsonElem(e) {
    override def toString: String = "JsonValue[value=" + value.toString + "parent=" + e.toString + "]"
}
private object JsonElem {
    def elem(root: JsonElem)(mk: JsonElem => JsonElem): JsonElem = {
        val el = mk(root)
        root.downs = root.downs :+ el
        el
    }

    // because type-safety
    def value(root: JsonField | JsonArray)(mk: JsonField | JsonArray => JsonElem): JsonElem = {
        val el = mk(root)
        root.downs = root.downs :+ el
        el
    }
}

*/

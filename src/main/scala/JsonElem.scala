/*
package org.nebularis.krom

import argonaut.Json
import argonaut.Json.JsonField
import com.fasterxml.jackson.core.JsonToken

type LabelType = String | Int

private sealed class Paths(private var buff: Vector[LabelType]) {
    private var upper: Vector[LabelType] = Vector.empty

    def appendPath(p: JsonField): Unit = buff = p +: buff

    def appendPath(i: Int): Unit = buff = i +: buff

    def pop: LabelType = {
        val l = buff.head
        buff = buff.tail
        l
    }

    def path: String = {
        val sb = new StringBuilder()
        buff.foldRight(sb, (z: StringBuilder, o: LabelType) => {
            o match {
                case i: Int => z.append("[", i, "]")
                case _ => z.append(o, ".")
            }
            z
        })
        buff.toString()
    }

    def length: Int = buff.size
}

/*
private class JsonValue(json: Json) {
    def print() : String = {
        if json.isBool then json.as[Boolean].toString
        else if json.isNumber then json.number.get.toString
        else if json.isString then json.string.get
        else if json.isNull then "Null"
        else ""
    }
}

private class JsonNull() extends JsonValue(null) {
    override def print() : String = "JsonNull"
}
*/

private abstract class JsonElem(id: Option[String] = None, path: Option[String] = None) {
    private var broader: Option[JsonElem] = None
    private var narrower: Vector[JsonElem] = Vector()

    def compositeNarrower(token: JsonToken)(paths: Paths): JsonElem = {
        
    }

    def compositeNarrower(elem: JsonElem): JsonElem = {
        elem.broader = Some(this)
        narrower = narrower :+ elem
        this
    }

    def branch: JsonElem = broader.orElse(Some(JsonRoot())).get
    def leaves: Vector[JsonElem] = narrower
}
private case class JsonRoot() extends JsonElem()
private case class JsonObj() extends JsonElem()
private case class JsonArray() extends JsonElem()
private case class JsonAttribute() extends JsonElem()
private case class JsonValue() extends JsonElem()

private object JsonElem {
    def entity(paths: Paths)(id: String): JsonElem = {
        paths.appendPath(id)
        JsonObj(id, paths.path)
    }
}
*/

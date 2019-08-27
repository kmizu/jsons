package com.github.kmizu.jsons

import com.github.kmizu.jsons.JsonAst._
import com.github.kmizu.scomb.SCombinator

object JsonParser extends SCombinator[JObject] {
  override def root: Parser[JObject] = space.* >> jsonObject

  lazy val jsonObject: Parser[JObject]  = for {
    _ <- token("{")
    members <- jsonMember.repeat0By(token(","))
    _ <- token("}")
  } yield JObject(members.toIndexedSeq)

  lazy val jsonArray: Parser[JArray] = for {
    _ <- token("[")
    elements <- jsonValue.repeat0By(token(","))
    _ <- token("]")
  } yield JArray(elements.toIndexedSeq)

  lazy val jsonMember = (jsonString << token(":")) ~ jsonValue ^^ { case name ~ value => JMember(name.value, value) }

  lazy val jsonValue: Parser[JValue] = (
    predict(
      '{' -> jsonObject,
      '[' -> jsonArray,
      '"' -> jsonString
    )
  | jsonNumber
  | token("true") ^^ {_ => JBoolean(true)}
  | token("false") ^^ {_ => JBoolean(false)}
  | token("null") ^^{_ => JNull}
  )

  lazy val jsonString: Parser[JString] = for {
    _ <- $("\"")
    elements <- (not($("\"")) >> any).*
    _ <- token("\"")
  } yield JString(elements.mkString(""))

  lazy val jsonNumber: Parser[JNumber] = set(('0' to '9').toSeq).+.map{_.mkString} << space.* ^^ {string =>
    JNumber(ULeft(string.toInt))
  }
}

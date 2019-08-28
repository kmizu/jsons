package com.github.kmizu.jsons

import com.github.kmizu.jsons.JsonAst._
import com.github.kmizu.scomb.SCombinator

object JsonParser extends SCombinator[JObject] {
  private def unescape(string: String): String = {
    val result = new StringBuilder
    var i = 0
    while(i < string.length) {
      val ch = string.charAt(i)
      ch match {
        case '\\' =>
          i += 1
          val escaped = string.charAt(i)
          escaped match {
            case '"' | '\\' | '/'=>
              result.append(escaped)
              i += 1
            case 'b' =>
              i += 1
              result.append('\b')
            case 'f' =>
              i += 1
              result.append('\f')
            case 'n' =>
              i += 1
              result.append('\n')
            case 'r' =>
              i += 1
              result.append('\r')
            case 't' =>
              i += 1
              result.append('\t')
            case 'u' =>
              i += 1
              val digits = string.substring(i, i + 4)
              val codePoint = Integer.parseInt(digits, 16).toChar
              result.append(codePoint)
              i += 4
          }
        case _ =>
          result.append(ch)
          i += 1
      }
    }
    result.toString
  }
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
    elements <- r("""((?!")(\[\"/bfnrt]|\[0-9a-fA-F]{4}|.))*""".r)
    _ <- token("\"")
  } yield JString(unescape(elements.mkString("")))

  lazy val jsonNumber: Parser[JNumber] = set(('0' to '9').toSeq).+.map{_.mkString} << space.* ^^ {string =>
    JNumber(ULeft(string.toInt))
  }
}

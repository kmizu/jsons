package com.github.kmizu.jsons

import scala.collection.immutable.{Seq => ImmutableSeq}

object JsonAst {
  implicit class RichString(self: String) {
    def to(value: JValue): JMember = JMember(self, value)
  }

  sealed abstract class JValue

  case object JNull extends JValue

  case class JString(value: String) extends JValue

  case class JNumber(value: Union[Long, Double]) extends JValue

  sealed abstract class JBoolean(value: Boolean) extends JValue

  object JBoolean {
    def apply(value: Boolean): JBoolean = {
      if (value)
        JTrue
      else
        JFalse
    }
  }

  case object JTrue extends JBoolean(true)

  case object JFalse extends JBoolean(false)

  case class JMember(name: String, value: JValue) extends JValue

  case class JObject(members: ImmutableSeq[JMember]) extends JValue
  object JObject {
    def make(members: JMember*): JObject = JObject(members.toIndexedSeq)
  }

  case class JArray(elements: ImmutableSeq[JValue]) extends JValue
  object JArray {
    def make(elements: JValue*): JArray = JArray(elements.toIndexedSeq)
  }
}

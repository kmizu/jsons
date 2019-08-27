package com.github.kmizu.jsons

import wvlet.airspec._
import com.github.kmizu.jsons.JsonParser._
import com.github.kmizu.jsons.JsonAst._
import com.github.kmizu.scomb.Result

import scala.collection.immutable.Seq

class JsonParserSpec extends AirSpec {
  def `empty object is parsed`: Unit = {
    val result = parse("{}")
    result.value.get shouldBe JObject(Seq())
  }

  def `simple object, which value is String, is parsed`: Unit = {
    val result = parse(""" {"test" : "Kota Mizushima"} """)
    result.value.get shouldBe JObject(Seq(JMember("test", JString("Kota Mizushima"))))
  }

  def `simple object, which value is Number, is parsed`: Unit = {
    val result = parse(""" {"test" : 35} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JNumber(ULeft(35)))))
  }

  def `simple object, which value is true, is parsed`: Unit = {
    val result = parse(""" {"test" : true} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JBoolean(true))))
  }

  def `simple object, which value is false, is parsed`: Unit = {
    val result = parse(""" {"test" : false} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JBoolean(false))))
  }

  def `simple object, which value is null, is parsed`: Unit = {
    val result = parse(""" {"test" : null} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JNull)))
  }

  def `simple object, which value is [], is parsed`: Unit = {
    val result = parse(""" {"test" : []} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JArray(Vector()))))
  }

  def `simple object, which value is [1], is parsed`: Unit = {
    val result = parse(""" {"test" : [1]} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JArray(Vector(JNumber(ULeft(1)))))))
  }

  def `simple object, which value is {}, is parsed`: Unit = {
    val result = parse(""" {"test" : { }} """)
    result.value.get shouldBe JObject(Vector(JMember("test",JObject(Vector()))))
  }

  def `simple object, which value is {"test2":1}, is parsed`: Unit = {
    val result = parse(""" {"test" : {"test2" : 1 }} """)
    JObject(Vector(JMember("test1",JObject(Vector(JMember("test2",JNumber(ULeft(1))))))))
  }
}

package com.digitalcipher.spiked.logging.json

import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, JsonFormat}

/**
  * Created by rob on 3/11/17.
  */
object EnumJsonConverter extends DefaultJsonProtocol {
  def jsonEnum[T <: Enumeration](enum: T): JsonFormat[T#Value] = new JsonFormat[T#Value] {

    override def write(obj: T#Value) = JsString(obj.toString)

    override def read(json: JsValue): enum.Value = json match {
      case JsString(txt) => enum.withName(txt)
      case something => throw DeserializationException(s"Expected a value from enum $enum instead of $something")
    }
  }
}

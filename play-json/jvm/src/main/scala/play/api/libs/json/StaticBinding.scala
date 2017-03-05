/*
 * Copyright (C) 2009-2016 Lightbend Inc. <https://www.lightbend.com>
 */
package play.api.libs.json

import play.api.libs.json.jackson.JacksonJson

object StaticBinding {
  /** Parses a [[JsValue]] from raw data. */
  def parseJsValue(data: Array[Byte]): JsResult[JsValue] =
    JsResult(JacksonJson parseJsValue data)

  /** Parses a [[JsValue]] from a string content. */
  def parseJsValue(input: String): JsResult[JsValue] =
    JsResult(JacksonJson parseJsValue input)

  /** Parses a [[JsValue]] from a stream. */
  def parseJsValue(stream: java.io.InputStream): JsResult[JsValue] =
    JsResult(JacksonJson parseJsValue stream)

  def generateFromJsValue(jsValue: JsValue, escapeNonASCII: Boolean): String =
    JacksonJson.generateFromJsValue(jsValue, escapeNonASCII)

  def prettyPrint(jsValue: JsValue): String = JacksonJson.prettyPrint(jsValue)

  def toBytes(jsValue: JsValue): Array[Byte] =
    JacksonJson.jsValueToBytes(jsValue)
}

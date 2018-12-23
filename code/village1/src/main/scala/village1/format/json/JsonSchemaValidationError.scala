package village1.format.json

import play.api.libs.json.{JsResult, JsValue, Json}

import JsonUtils.errorsToString

class JsonSchemaValidationError(result: String) extends Error(result) {

  def this(result: JsResult[JsValue]) {
    this(errorsToString(result))
  }
}

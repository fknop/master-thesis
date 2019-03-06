package village1.json

import play.api.libs.json.{JsResult, JsValue}
import village1.json.JsonUtils.errorsToString

class JsonSchemaValidationError(result: String) extends Error(result) {

  def this(result: JsResult[JsValue]) {
    this(errorsToString(result))
  }
}

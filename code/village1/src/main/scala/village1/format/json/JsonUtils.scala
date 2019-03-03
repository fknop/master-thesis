package village1.format.json

import com.eclipsesource.schema._
import play.api.libs.json.{JsResult, JsValue, Json}
import village1.util.FileUtils.{readFile, writeFile}

object JsonUtils {
  def errorsToString (result: JsResult[JsValue]): String = {
    val errors = result.fold(
      invalid = { _.toJson },
      valid = { post => post }
    )

    Json.prettyPrint(errors)
  }

  def parseJsonFile (path: String): JsValue = {
    val content = readFile(path)
    Json.parse(content)
  }

  def writeJsonFile (path: String, json: JsValue): Unit = {
    val content = Json.prettyPrint(json)
    writeFile(path, content)
  }
}

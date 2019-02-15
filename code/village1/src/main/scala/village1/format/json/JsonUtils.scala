package village1.format.json

import java.io.PrintWriter

import play.api.libs.json.{JsResult, JsValue, Json}
import com.eclipsesource.schema._
import village1.util.FileUtils.{writeFile, readFile}

import scala.io.Source

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

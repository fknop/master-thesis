package village1.format.json

import java.io.PrintWriter

import play.api.libs.json.{JsResult, JsValue, Json}
import com.eclipsesource.schema._

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

    val file = Source.fromFile(path)
    val content = file.getLines().mkString("\n")
    file.close()

    Json.parse(content)
  }

  def writeJsonFile (path: String, content: JsValue): Unit = {
    new PrintWriter(path) {
      write(Json.prettyPrint(content))
      close()
    }
  }
}

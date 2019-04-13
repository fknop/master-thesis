package village1.benchmark.charts

import play.api.libs.json.Json
import village1.benchmark.api.BenchmarkOverTimeNSerie
import village1.util.FileUtils

object OOTChart {

  def generate(data: Array[BenchmarkOverTimeNSerie]): String => Unit = {
    val path = ChartPaths.OOT
    val template = FileUtils.readFile(path)

    val content = ChartUtils.replaceData(template, json(data))

    out: String => {
      FileUtils.writeFile(out, content)
    }
  }

  def json(data: Array[BenchmarkOverTimeNSerie]): String = {

    val json = data.map( serie =>
      Json.obj(
        "name" -> serie.name,
        "x" -> data(0).results.indices,
        "y" -> serie.means
      )
    )

    Json.prettyPrint(Json.toJson(json))
  }
}

package village1.benchmark.api.json

import play.api.libs.json._
import village1.benchmark.api._
import village1.json.JsonUtils

object JsonBenchmark {
  implicit val problemSizeReads = Json.reads[ProblemSize]
  implicit val bennchmarkMeasurementReads = Json.reads[BenchmarkMeasurement]
  //  implicit private val benchmarkResultReads = Json.reads[BenchmarkResult]
  implicit val benchmarkSeriReads= Json.reads[BenchmarkSerie]
  implicit val benchmarkReads = Json.reads[BenchmarkInstance]
  implicit val benchmarkOOTNSerieReads = Json.reads[BenchmarkOverTimeNSerie]
  implicit val benchmarkOOTSerieReads = Json.reads[BenchmarkOverTimeSerie]

  def parse(path: String): BenchmarkInstance = {
    val json = JsonUtils.parseJsonFile(path)
    val result = Json.fromJson[BenchmarkInstance](json)
    result match {
      case JsSuccess(b: BenchmarkInstance, _) => b
      case e: JsError => throw JsResultException(e.errors)
    }
  }

  implicit val problemSizeWrites = Json.writes[ProblemSize]
  implicit val bennchmarkMeasurementWrites = Json.writes[BenchmarkMeasurement]
  implicit val benchmarkSerieWrites = Json.writes[BenchmarkSerie]
  implicit val benchmarkWrites: OWrites[BenchmarkInstance] = Json.writes[BenchmarkInstance]
  implicit val benchmarkOOTNSerieWrites: OWrites[BenchmarkOverTimeNSerie] = Json.writes[BenchmarkOverTimeNSerie]
  implicit val benchmarkOOTSerieWrites: OWrites[BenchmarkOverTimeSerie] = Json.writes[BenchmarkOverTimeSerie]

  def serialize (benchmark: BenchmarkInstance): String => Unit = {
    val json = Json.toJson(benchmark)
    path => {
      JsonUtils.writeJsonFile(path, json)
    }
  }
}

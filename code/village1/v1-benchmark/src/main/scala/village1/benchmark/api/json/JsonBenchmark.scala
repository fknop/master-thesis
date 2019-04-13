package village1.benchmark.api.json

import play.api.libs.json._
import village1.benchmark.api._
import village1.json.JsonUtils

object JsonBenchmark {
  implicit private val problemSizeReads = Json.reads[ProblemSize]
  implicit private val bennchmarkMeasurementReads = Json.reads[BenchmarkMeasurement]
  //  implicit private val benchmarkResultReads = Json.reads[BenchmarkResult]
  implicit private val benchmarkSeriReads= Json.reads[BenchmarkSerie]
  implicit private val benchmarkReads = Json.reads[BenchmarkInstance]
  implicit private val benchmarkOOTSerieReads = Json.reads[BenchmarkOverTimeNSerie]

  def parse(path: String): BenchmarkInstance = {
    val json = JsonUtils.parseJsonFile(path)
    val result = Json.fromJson[BenchmarkInstance](json)
    result match {
      case JsSuccess(b: BenchmarkInstance, _) => b
      case e: JsError => throw JsResultException(e.errors)
    }
  }

  implicit private val problemSizeWrites = Json.writes[ProblemSize]
  implicit private val bennchmarkMeasurementWrites = Json.writes[BenchmarkMeasurement]
  implicit private val benchmarkSerieWrites = Json.writes[BenchmarkSerie]
  implicit private val benchmarkWrites: OWrites[BenchmarkInstance] = Json.writes[BenchmarkInstance]
  implicit private val benchmarkOOTSerieWrites: OWrites[BenchmarkOverTimeNSerie] = Json.writes[BenchmarkOverTimeNSerie]

  def serialize (benchmark: BenchmarkInstance): String => Unit = {
    val json = Json.toJson(benchmark)
    path => {
      JsonUtils.writeJsonFile(path, json)
    }
  }
}

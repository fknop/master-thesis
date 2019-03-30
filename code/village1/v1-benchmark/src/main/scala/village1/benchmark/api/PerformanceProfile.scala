package village1.benchmark.api

import play.api.libs.json.Json
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.charts.PerformanceProfileChart

object PerformanceProfile extends App {

  def metric(obj: Array[Double], i: Int) = obj(i)

  def ratio(obj: Array[Double], i: Int, baseline: Array[Array[Double]]): Double = {
    val m = metric(obj, i)
    val min = baseline.map(_(i)).min
    m.toDouble / min.toDouble
  }

  def F(obj: Array[Double], T: Double, baseline: Array[Array[Double]]): Double = {
    var n = 0
    for (i <- obj.indices) {
      if (ratio(obj, i, baseline) <= T) {
        n += 1
      }
    }

    n.toDouble / obj.length.toDouble
  }


  def ratios(obj: Array[Double], baseline: Array[Array[Double]]): IndexedSeq[Double] = {
    val ratios = for (i <- obj.indices) yield ratio(obj, i, baseline)
    ratios.sorted
  }

  def cumulative(obj: Array[Double], ratios: IndexedSeq[Double], baseline: Array[Array[Double]]): IndexedSeq[Double] = {
    ratios.map(r => F(obj, r, baseline))
  }


  def get_xy(obj: Array[Double], baseline: Array[Array[Double]]): (IndexedSeq[Double], IndexedSeq[Double]) = {
    val x = ratios(obj, baseline)
    val y = cumulative(obj, x, baseline)

    (x, y)
  }

  def line(arr: IndexedSeq[Double]) = arr.mkString(",")

  def csv(results: Array[(IndexedSeq[Double], IndexedSeq[Double])]): String = {
    val matrix: Array[Array[Double]] = results.flatMap(c => Array(c._1.toArray, c._2.toArray))
    val lines = matrix.transpose
    lines.map(l => line(l)).mkString("\n")
  }

  def json(results: Array[(String, (IndexedSeq[Double], IndexedSeq[Double]))]): String = {
    val json = Json.obj(
    "series" -> results.map { result =>
        Json.obj(
          "name" -> result._1,
          "x" -> result._2._1,
          "y" -> result._2._2
        )
      }
    )

    Json.prettyPrint(json)
  }

  def generate(baseline: Array[Array[Double]], values: Array[Array[Double]], names: Array[String]): String = {
    val results = values.map(get_xy(_, baseline))
    json(names.zip(results))
  }

  val name = "cp,mip,cp+mip106"
  val benchmark = JsonBenchmark.parse(s"data/benchmark/$name.json")
  val values: Array[Array[Double]] = benchmark.objectiveSeries.map(_.results.map(_.mean)).toArray

  val names = Array("CP", "MIP", "CP+MIP")

  val baselines = Array(
    Array(0, 1, 2),
    Array(0),
    Array(1),
    Array(2),
    Array(0, 1),
    Array(0, 2),
    Array(1, 2)
  )

  for (baseline <- baselines) {
    val profile = generate(baseline.map(values(_)), values, names)
    val bName = baseline.map(names(_)).mkString(",")
    PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-B=$bName.html")
  }
}
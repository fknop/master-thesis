package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api._
import village1.benchmark.charts.PerformanceProfileChart
import village1.util.Utils

object CP_MIP_CPMIP_Benchmark extends CommandLineBenchmark {

  val name = s"cp,mip,cp+mip${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP", solveCP(benchmark))
  val (t1, o1) = benchmark.run("MIP", solveMIP(benchmark))
  val (t2, o2) = benchmark.run("CP+MIP", solveCP_MIP(benchmark))

  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1, t2), objectiveSeries = Seq(o0, o1, o2, benchmark.lowerBoundSerie()))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)

  def m(s: BenchmarkSerie) = s.results.map(_.mean)

  val values = Array(m(o0), m(o1), m(o2))
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
    val profile = PerformanceProfile.generate(baseline.map(values(_)), values, names)
    val bName = baseline.map(names(_)).mkString(",")
    PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-B=$bName.html")
  }
}

package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api._
import village1.benchmark.charts.{OOTChart, PerformanceProfileChart}
import village1.benchmark.util.MathUtils
import village1.util.Utils

object CP_MIP_CPMIP_Benchmark extends CommandLineBenchmark {

  val name = s"cp,mip,cp+mip${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))
  println(MathUtils.estimatedTime(options, 3))

  val benchmark = new BenchmarkRunner(options = options)
  val (t0, o0, oot0) = benchmark.run("CP", solveCP(benchmark))
  val (t1, o1, oot1) = benchmark.run("MIP", solveMIP(benchmark))
  val (t2, o2, oot2) = benchmark.run("CP+MIP", solveCP_MIP(benchmark))


  val normalized = benchmark.normalize(options.timeLimit, oot0, oot1, oot2)


  OOTChart.generate(normalized)(s"data/benchmark/html/oot-$name.html")

  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1, t2), objectiveSeries = Seq(o0, o1, o2, benchmark.lowerBoundSerie()))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)


  val objectiveValues = Array(o0.means, o1.means, o2.means)
  val timeValues = Array(t0.means, t1.means, t2.means)
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
    val profile = PerformanceProfile.generate(baseline.map(objectiveValues(_)), objectiveValues, names)
    val timeProfile = PerformanceProfile.generate(baseline.map(timeValues(_)), timeValues, names)

    val bName = baseline.map(names(_)).mkString(",")
    PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-B=$bName.html")
    PerformanceProfileChart.generate(timeProfile)(s"data/benchmark/html/$name-TIME-B=$bName.html")
  }

  JsonBenchmark.serialize(instance)(options.out)
}

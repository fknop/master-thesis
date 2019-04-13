package village1.benchmark

import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.util.Utils


object CPLNSBenchmark extends CommandLineBenchmark with Branchings {

  val name = s"cp-lns-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  BenchmarkRunner.run(
    name,
    options,
    Array("CP-LNS", "CP"),
    Array(
      b => solveCP(b),
      b => solveCP_NoLNS(b)
    )
  )

//  println(MathUtils.estimatedTime(options, 2))
//
//
//  val benchmark = new BenchmarkRunner(options = options)
//
//  val names = Array("CP-LNS", "CP")
//  val (t0, o0, oot0) = benchmark.run(names(0), solveCP(benchmark))
//  val (t1, o1, oot1) = benchmark.run(names(1), solveCP_NoLNS(benchmark))
//
//  val normalized = benchmark.normalize(oot0, oot1)
//
//
////  val lb = benchmark.lowerBoundSerie()
////  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))
//
//
//  OOTChart.generate(normalized)(s"data/benchmark/html/oot-${name}.html")
//
//  val values = Array(o0.means, o1.means)
//
//  val baselines = Array(
//    Array(0),
//    Array(1),
//    Array(0, 1)
//  )
//
//  for (baseline <- baselines) {
//    val profile = PerformanceProfile.generate(baseline.map(values(_)), values, names)
//    val bName = baseline.map(names(_)).mkString(",")
//    PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-B=$bName.html")
//  }

//  val writer = JsonBenchmark.serialize(instance)
//  writer(options.out)
}

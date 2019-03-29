package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.charts.PerformanceProfileChart
import village1.search.cp.relaxations.PropagationGuidedRelaxation
import village1.util.{FileUtils, Utils}

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val name = s"cp-relaxations-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP-Random", solveCP(benchmark))
  val (t1, o1) = benchmark.run("CP-Prop", solveCP(benchmark, applyToSearch = search => {
    search.relax {
      val relaxation = new PropagationGuidedRelaxation()
      () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 2)
    }
  }))

  def m(s: BenchmarkSerie) = s.results.map(_.mean)

  val values = Array(m(o0), m(o1))

  val profile = PerformanceProfile.generate(values, values, Array("CP-Random", "CP-Prop"))
  val profile2 = PerformanceProfile.generate(Array(values(0)), values, Array("CP-Random", "CP-Prop"))
  val profile3 = PerformanceProfile.generate(Array(values(1)), values, Array("CP-Random", "CP-Prop"))

  FileUtils.writeFile(s"data/benchmark/profile/$name.json", profile)
  PerformanceProfileChart.generate(profile)(s"data/benchmark/html/$name-1.html")
  PerformanceProfileChart.generate(profile2)(s"data/benchmark/html/$name-2.html")
  PerformanceProfileChart.generate(profile3)(s"data/benchmark/html/$name-3.html")

  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, benchmark.lowerBoundSerie()))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}

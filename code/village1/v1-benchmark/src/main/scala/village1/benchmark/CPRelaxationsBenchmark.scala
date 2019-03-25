package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}
import village1.search.cp.relaxations.PropagationGuidedRelaxation

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-relaxations.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP-Random", solveCP(benchmark))
  val (t1, o1) = benchmark.run("CP-Prop", solveCP(benchmark, applyToSearch = search => {
    search.relax {
      val relaxation = new PropagationGuidedRelaxation()
      () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 2)
    }
  }))

  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, benchmark.lowerBoundSerie()))

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}

package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.relaxations.{PropagationGuidedRelaxation, RandomRelaxation}
import village1.util.Utils

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val name = s"cp-relaxations-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val names = Array(
    "CP-Random-20",
    "CP-Random-30",
    "CP-Prop-1",
    "CP-Prop-2",
    "CP-Prop-3"
  )

  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.2)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.3)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation =
      new PropagationGuidedRelaxation(search, search.flatWorkers, search.flatWorkers.length)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation =
      new PropagationGuidedRelaxation(search, search.flatWorkers, search.flatWorkers.length / 2)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation =
      new PropagationGuidedRelaxation(search, search.flatWorkers, search.flatWorkers.length / 3)),
  )

  BenchmarkRunner.run(name, options, names, solvers)
}

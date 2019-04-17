package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.relaxations.RandomRelaxation
import village1.util.Utils

object CPRelaxationsBenchmark extends CommandLineBenchmark {

  val name = s"cp-relaxations-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val names = Array(
    "CP-Random-20",
    "CP-Random-30",
    "CP-Random-40",
    "CP-Random-50",
    "CP-Prop"
  )

  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.2)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.3)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.4)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.5)),
    (b: BenchmarkRunner) => solveCP(b)
  )

  BenchmarkRunner.run(name, options, names, solvers)
}

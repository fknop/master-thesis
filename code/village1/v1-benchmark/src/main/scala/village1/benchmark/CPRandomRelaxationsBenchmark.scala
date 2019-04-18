package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api._
import village1.search.cp.relaxations.RandomRelaxation
import village1.util.Utils

object CPRandomRelaxationsBenchmark extends CommandLineBenchmark {

  val name = s"cp-relaxations-${Utils.randomInt(0, 1000)}"

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/$name.json"))

  val names = Array(
    "CP-Random-10",
    "CP-Random-20",
    "CP-Random-30",
    "CP-Random-40",
    "CP-Random-50",
    "CP-Random-60",
    "CP-Random-70",
    "CP-Random-80",
    "CP-Random-90",
  )

  val solvers = Array(
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.1)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.2)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.3)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.4)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.5)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.6)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.7)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.8)),
    (b: BenchmarkRunner) => solveCP(b, applyToSearch = search => search.relaxation = new RandomRelaxation(search, 0.9)),
  )

  BenchmarkRunner.run(name, options, names, solvers)
}

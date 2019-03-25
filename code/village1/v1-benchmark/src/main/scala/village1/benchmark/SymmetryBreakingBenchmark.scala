package village1.benchmark

import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.{BenchmarkArgs, CommandLineBenchmark, SolverBenchmark}

object SymmetryBreakingBenchmark extends CommandLineBenchmark {

  val options = parseArgs(BenchmarkArgs(out = "data/benchmark/cp-symmetry.json"))

  val benchmark = new SolverBenchmark(options = options)
  val cpResults = benchmark.run("CP-NoSym", solveCP(benchmark))
  val cpResultsSB = benchmark.run("CP-Sym", solveCPWithSymmetries(benchmark))

//  val instance = benchmark.makeInstance(cpResults, cpResultsSB)
//
//  val writer = JsonSerializer.serialize(instance)
//  writer(options.out)
}

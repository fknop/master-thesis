package village1.benchmark

import oscar.algo.search.Branching
import oscar.cp.modeling.Branchings
import village1.benchmark.BenchmarkSolverFunctions._
import village1.benchmark.api.json.JsonBenchmark
import village1.benchmark.api._
import village1.search.cp.heuristic.Heuristic
import village1.util.{FileUtils, Utils}


object CPHeuristicBenchmark extends CommandLineBenchmark with Branchings {

  val options = parseArgs(BenchmarkArgs(out = s"data/benchmark/cp-heuristic${Utils.randomInt(0, 1000)}.json"))

  val benchmark = new SolverBenchmark(options = options)
  val (t0, o0) = benchmark.run("CP-MA", solveCP(benchmark))
  val (t1, o1) = benchmark.run("CP-FF", solveCP(benchmark, applyToSearch = search => {
    search.heuristic {
      new Heuristic {
        override def branching: Branching = binaryFirstFailIdx(search.flatWorkers, i => {
          search.flatWorkers(i).max
        })
      }
    }
  }))

  val lb = benchmark.lowerBoundSerie()
  val instance = benchmark.makeInstance(timeSeries = Seq(t0, t1), objectiveSeries = Seq(o0, o1, lb))

  def m(s: BenchmarkSerie) = s.results.map(_.mean)


  val values = Array(m(o0), m(o1))

  println(m(o0).mkString(" "))
  println(m(o1).mkString(" "))

  val json = PerformanceProfile.generate(values, values, Array("CP-MA", "CP-FF"))
  FileUtils.writeFile(s"data/benchmark/profile/cp-heuristic${Utils.randomInt(0, 1000)}.json", json)

  val writer = JsonBenchmark.serialize(instance)
  writer(options.out)
}

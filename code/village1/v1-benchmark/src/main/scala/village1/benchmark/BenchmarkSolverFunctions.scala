package village1.benchmark

import village1.benchmark.api.BenchmarkRunner
import village1.modeling.VillageOneModel
import village1.modeling.cp.CPModelOptions
import village1.search._
import village1.search.cp.{LNSOptions, VillageOneLNS, VillageOneSearch}
import village1.search.mip.MIPSearch

object BenchmarkSolverFunctions {
  def solveMIP (b: BenchmarkRunner): VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {
    base: VillageOneModel =>
      val solver = new VillageOneSolver(base)

      def solve(): (Long, Int) = {
        val options = SolverOptions(method = SolverType.MIP)
        val results = solver.solve(b.TimeLimit, b.SolutionLimit, silent = true, Some(options))
        (results.time, results.solution.get.objective)
      }

      (solver, solve)
    }




  def solveCP (b: BenchmarkRunner, options: CPModelOptions = CPModelOptions(), lnsOptions: LNSOptions = LNSOptions(), applyToSearch: VillageOneLNS => Unit = _ => {}):
      VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base, options)
      applyToSearch(search)


      def solve(): (Long, Int) = {
        val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true, options = Some(lnsOptions))
        if (results.solution.isEmpty) null
        else (results.time, results.solution.get.objective)
      }

      (search, solve)
    }
  }

  def solveCP_NoLNS (b: BenchmarkRunner, options: CPModelOptions = CPModelOptions()): VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {
    base: VillageOneModel => {
      val search = new VillageOneSearch(base, options)

      def solve(): (Long, Int) = {
        val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
        if (results.solution.isEmpty) null
        else (results.time, results.solution.get.objective)
      }

      (search, solve)
    }
  }

  def solveCPPropagationRelax (b: BenchmarkRunner): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base)

      val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
      assert(search.lastSolution != null)
      (results.time, results.solution.get.objective)
    }
  }

    def solveCP_MIP (b: BenchmarkRunner, startMipProbability: Double = 0.5):  VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {

      base: VillageOneModel =>
        val solver = new VillageOneSolver(base)

        def solve(): (Long, Int) = {
          val options = SolverOptions(method = SolverType.CP_MIP)
          val results = solver.solve(b.TimeLimit, b.SolutionLimit, silent = true, Some(options))
          (results.time, results.solution.get.objective)
        }

        (solver, solve)
    }

}

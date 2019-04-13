package village1.benchmark

import village1.benchmark.api.BenchmarkRunner
import village1.modeling.VillageOneModel
import village1.modeling.cp.CPModelOptions
import village1.search.{SearchResult, SolutionEmitter}
import village1.search.cp.{LNSOptions, VillageOneLNS, VillageOneSearch}
import village1.search.mip.MIPSearch

object BenchmarkSolverFunctions {
  def solveMIP (b: BenchmarkRunner): VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {
     base: VillageOneModel => {
      val search = new MIPSearch(base)

       def solve(): (Long, Int) = {
        val results: SearchResult = search.solve(silent = true, timeLimit = b.TimeLimit, solutionLimit = b.SolutionLimit)

        if (results.solution.isEmpty) null
        else (results.time, results.solution.get.objective)
       }

       (search, solve)
    }
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

//      search.relax {
//        val relaxation = new PropagationGuidedRelaxation()
//        () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 3)
//      }

      val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
      assert(search.lastSolution != null)
      (results.time, results.solution.get.objective)
    }
  }

    def solveCP_MIP (b: BenchmarkRunner, startMipProbability: Double = 0.5):  VillageOneModel => (SolutionEmitter, () => (Long, Int)) = {

      base: VillageOneModel => {
        val cp = new VillageOneSearch(base)
        val mip = new MIPSearch(base)


        def solve(): (Long, Int) = {
          val cpResults = cp.solve(solutionLimit = 1, timeLimit = b.TimeLimit, silent = true)

          val remaining = (((b.TimeLimit * 1000.0) - cpResults.time) / 1000.0).round.toInt


          if (cpResults.solution.isDefined) {
            mip.setInitialSolution(cpResults.solution.get, startMipProbability)
          }

          val nSols = if (cpResults.solution.isDefined) 1 else 0

          val results = mip.solve(silent = true, timeLimit = remaining, solutionLimit = math.max(b.SolutionLimit - nSols, 1))
          if (mip.lastSolution.isEmpty && cp.lastSolution.isEmpty) null
          else if (mip.lastSolution.isEmpty && cp.lastSolution.isDefined) (cpResults.time + results.time, cp.lastSolution.get.objective)
          else (cpResults.time + results.time, mip.lastSolution.get.objective)
        }

        (mip, solve)
      }
    }

}

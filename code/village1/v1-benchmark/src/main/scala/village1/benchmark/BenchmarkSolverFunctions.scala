package village1.benchmark

import village1.benchmark.api.SolverBenchmark
import village1.modeling.VillageOneModel
import village1.modeling.cp.CPModelOptions
import village1.modeling.mip.MipModelOptions
import village1.search.SearchResult
import village1.search.cp.relaxations.PropagationGuidedRelaxation
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.search.mip.MIPSearch

object BenchmarkSolverFunctions {
  def solveMIP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
     base: VillageOneModel => {
      val search = new MIPSearch(base)
      val results: SearchResult = search.solve(silent = true, timeLimit = b.TimeLimit, solutionLimit = b.SolutionLimit)

      if (results.solution.isEmpty) null
      else (results.time, results.solution.get.objective)
    }
  }

//  def solveCPWithSymmetries (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
//   base: VillageOneModel => {
//      val options = CPModelOptions(symmetryBreaking = false)
//      val search = new VillageOneLNS(base = base, options = options)
//      val stats = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
//      assert(search.lastSolution != null)
//      (stats, search.lastSolution.objective)
//    }
//  }

  def solveCP (b: SolverBenchmark, options: CPModelOptions = CPModelOptions(), applyToSearch: VillageOneLNS => Unit = _ => {}): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base, options)
      applyToSearch(search)
      val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
      if (results.solution.isEmpty) null
      else (results.time, results.solution.get.objective)
    }
  }

  def solveCPPropagationRelax (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base)

      search.relax {
        val relaxation = new PropagationGuidedRelaxation()
        () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 3)
      }

      val results = search.solve(solutionLimit = b.SolutionLimit, timeLimit = b.TimeLimit, silent = true)
      assert(search.lastSolution != null)
      (results.time, results.solution.get.objective)
    }
  }

    def solveCP_MIP (b: SolverBenchmark, startMipProbability: Double = 0.5): VillageOneModel => (Long, Int) = {

      base: VillageOneModel => {
        val cp = new VillageOneSearch(base)
        val cpResults = cp.solve(solutionLimit = 1, timeLimit = b.TimeLimit, silent = true)

        val remaining = (((b.TimeLimit * 1000.0) - cpResults.time) / 1000.0).round.toInt

        val mip = new MIPSearch(base)

        if (cpResults.solution.isDefined) {
          mip.setInitialSolution(cpResults.solution.get, startMipProbability)
        }

        val nSols = if (cpResults.solution.isDefined) 1 else 0

        val results = mip.solve(silent = true, timeLimit = remaining, solutionLimit = math.max(b.SolutionLimit - nSols, 1))
        if (mip.lastSolution == null && cp.lastSolution == null) null
        else if (mip.lastSolution == null && cp.lastSolution != null) (cpResults.time + results.time, cp.lastSolution.get.objective)
        else (cpResults.time + results.time, mip.lastSolution.get.objective)
      }
    }

}

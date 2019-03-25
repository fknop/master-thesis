package village1.benchmark

import village1.benchmark.api.SolverBenchmark
import village1.modeling.VillageOneModel
import village1.modeling.cp.{CPModelOptions, PropagationGuidedRelaxation}
import village1.modeling.mip.MipModelOptions
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.search.mip.{MIPSearch, MipSolverResult}

object BenchmarkSolverFunctions {
  def solveMIP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
     base: VillageOneModel => {
      val search = new MIPSearch(base)
      val solver: MipSolverResult = search.solve(silent = true, timeLimit = b.TimeLimit, nSols = b.SolutionLimit)

      if (search.lastSolution == null) null
      else (solver.solveTime, search.lastSolution.objective)
    }
  }

  def solveMIPWithSymmetries (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val model = new MIPSearch(base, MipModelOptions(symmetryBreaking = false))
      val solver: MipSolverResult = model.solve(silent = true, timeLimit = b.TimeLimit, nSols = b.SolutionLimit)
      (solver.solveTime, solver.solution.objective)
    }
  }

  def solveCPWithSymmetries (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
   base: VillageOneModel => {
      val options = CPModelOptions(symmetryBreaking = false)
      val search = new VillageOneLNS(base = base, options = options)
      val stats = search.solve(nSols = b.SolutionLimit, timeLimit = b.TimeLimit * 1000, silent = true)
      assert(search.lastSolution != null)
      (stats, search.lastSolution.objective)
    }
  }

  def solveCP (b: SolverBenchmark, options: CPModelOptions = CPModelOptions(), applyToSearch: VillageOneLNS => Unit = _ => {}): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base, options)
      applyToSearch(search)
      val stats = search.solve(nSols = b.SolutionLimit, timeLimit = b.TimeLimit * 1000, silent = true)
      if (search.lastSolution == null) null
      else (stats, search.lastSolution.objective)
    }
  }

  def solveCPPropagationRelax (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base)

      search.relax {
        val relaxation = new PropagationGuidedRelaxation()
        () => relaxation.propagationGuidedRelax(search.solver, search.flatWorkers, search.currentSolution, search.flatWorkers.length / 3)
      }

      val stats = search.solve(nSols = b.SolutionLimit, timeLimit = b.TimeLimit * 1000, silent = true)
      assert(search.lastSolution != null)
      (stats, search.lastSolution.objective)
    }
  }

    def solveCP_MIP (b: SolverBenchmark, startMipProbability: Double = 0.5): VillageOneModel => (Long, Int) = {

      base: VillageOneModel => {
        val cp = new VillageOneSearch(base)
        val stat = cp.solve(nSols = 1, timeLimit = b.TimeLimit * 1000, silent = true)

        val remaining = b.TimeLimit - (stat.time / 1000.0).round.toInt

        val mip = new MIPSearch(base)

        if (cp.lastSolution != null) {
          mip.setInitialSolution(cp.lastSolution, startMipProbability)
        }

        val solver: MipSolverResult = mip.solve(silent = true, timeLimit = remaining, nSols = math.max(b.SolutionLimit - stat.nSols, 1))
        if (mip.lastSolution == null && cp.lastSolution == null) null
        else if (mip.lastSolution == null && cp.lastSolution != null) (stat.time + solver.solveTime, cp.lastSolution.objective)
        else (stat.time + solver.solveTime, mip.lastSolution.objective)
      }
    }

}

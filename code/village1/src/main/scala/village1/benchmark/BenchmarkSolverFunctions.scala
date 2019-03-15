package village1.benchmark

import village1.modeling.VillageOneModel
import village1.modeling.cp.{CPModelOptions, PropagationGuidedRelaxation}
import village1.modeling.mip.MipModelOptions
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.search.mip.{MIPSearch, MipSolverResult}

object BenchmarkSolverFunctions {
  def solveMIP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
     base: VillageOneModel => {
      val model = new MIPSearch(base)
      val solver: MipSolverResult = model.solve(silent = true, timeLimit = b.TimeLimit, nSols = b.SolutionLimit)
      (solver.solveTime, solver.solution.objective)
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

  def solveCP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val search = new VillageOneLNS(base)
      val stats = search.solve(nSols = b.SolutionLimit, timeLimit = b.TimeLimit * 1000, silent = true)
      assert(search.lastSolution != null)
      (stats, search.lastSolution.objective)
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

    def solveCPThenMIP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {

      base: VillageOneModel => {
        val cp = new VillageOneSearch(base)
        val stat = cp.solve(nSols = 1, timeLimit = b.TimeLimit * 1000, silent = true)

        val remaining = b.TimeLimit - (stat.time / 1000.0).round.toInt

        val mip = new MIPSearch(base)

        if (cp.lastSolution != null) {
          mip.setInitialSolution(cp.lastSolution)
        }

        val solver: MipSolverResult = mip.solve(silent = true, timeLimit = remaining, nSols = b.SolutionLimit)
        (solver.solveTime + stat.time, mip.lastSolution.objective)
      }
    }

}

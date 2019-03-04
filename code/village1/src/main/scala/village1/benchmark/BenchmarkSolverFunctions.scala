package village1.benchmark

import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol
import village1.modeling.VillageOneModel
import village1.modeling.cp.CPModelOptions
import village1.modeling.mip.{MipModelOptions, SolverResult, VillageOneMIPModel}
import village1.search.cp.VillageOneLNS

object BenchmarkSolverFunctions {
  def solveMIP (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
     base: VillageOneModel => {
      val model = new VillageOneMIPModel(base)
      val solver: SolverResult = model.solve(silent = true, timeLimit = b.TimeLimit, nSols = b.SolutionLimit)
      solver.dispose()

      (solver.solveTime, solver.solution.objective)
    }
  }

  def solveMIPWithSymmetries (b: SolverBenchmark): VillageOneModel => (Long, Int) = {
    base: VillageOneModel => {
      val model = new VillageOneMIPModel(base, MipModelOptions(symmetryBreaking = false))
      val solver: SolverResult = model.solve(silent = true, timeLimit = b.TimeLimit, nSols = b.SolutionLimit)
      solver.dispose()

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
}

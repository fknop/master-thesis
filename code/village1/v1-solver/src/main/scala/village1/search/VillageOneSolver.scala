package village1.search

import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.cp.{LNSOptions, VillageOneLNS}
import village1.search.mip.{MIPSearch, MipSearchOptions}

object SolverType extends Enumeration {
  val CP, MIP, CP_MIP = Value
}

case class SolverOptions(
  cp: LNSOptions = LNSOptions(),
  mip: MipSearchOptions = MipSearchOptions(),
  method: SolverType.Value = SolverType.CP) extends SearchOptions

class VillageOneSolver(problem: Problem, base: Option[VillageOneModel] = None) extends VillageOneModel(problem) with Search[SolverOptions] {

  def this(base: VillageOneModel) = this(base.problem, Some(base))

  private var currentTime = 0L

  override def solve(timeLimit: Int, solutionLimit: Int, silent: Boolean, options: Option[SolverOptions]): SearchResult = {

    val opts = if (options.isDefined) options.get else SolverOptions()

    currentTime = System.currentTimeMillis()

    opts.method match {
      case SolverType.CP => solve(timeLimit, solutionLimit, silent, opts.cp)
      case SolverType.MIP => solve(timeLimit, solutionLimit, silent, opts.mip)
      case SolverType.CP_MIP => solve(timeLimit, solutionLimit, silent, opts.cp, opts.mip)
    }
  }

  private def solve(timeLimit: Int, solutionLimit: Int, silent: Boolean, options: LNSOptions): SearchResult = {
    val search = new VillageOneLNS(problem, base = Some(this))
    search.onSolutionFound(this.onSol)
    search.solve(timeLimit, solutionLimit, silent, Some(options))
  }

  private def solve(timeLimit: Int, solutionLimit: Int, silent: Boolean, options: MipSearchOptions): SearchResult = {
    val search = new MIPSearch(problem, base = Some(this))
    search.onSolutionFound(this.onSol)
    search.solve(timeLimit, solutionLimit, silent, Some(options))
  }

  private def onSol(solution: Solution): Unit = {
    val curr = System.currentTimeMillis()
    emitSolution(solution.copy(time = curr - currentTime))
    currentTime = curr
  }

  private def solve(timeLimit: Int, solutionLimit: Int, silent: Boolean, cp: LNSOptions, mip: MipSearchOptions): SearchResult = {
    val results = solve(timeLimit, 1, silent, cp)

    if (results.solution.isDefined) {
      val remaining = (((timeLimit * 1000.0) - results.time) / 1000.0).round.toInt
      val mipResults = solve(timeLimit = remaining, math.max(solutionLimit - 1, 1), silent, mip)
      if (mipResults.solution.isDefined) {
        SearchResult(mipResults.solution, mipResults.time + results.time)
      }
      else {
        SearchResult(results.solution, mipResults.time + results.time)
      }
    }
    else {
      results
    }
  }
}

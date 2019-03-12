package village1.search.mip

import gurobi.{GRB, GRBCallback, GRBException}
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.modeling.mip.{MipModelOptions, SolverResult, VillageOneMIPModel}
import village1.search.Search
import village1.search.cp.VillageOneLNS
import village1.util.Benchmark.time

class MIPSearch(problem: Problem, options: MipModelOptions = MipModelOptions(), base: Option[VillageOneModel] = None) extends VillageOneMIPModel(problem, options, base) with Search {
  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: MipModelOptions) = this(base.problem, options, Some(base))


  def solve(timeLimit: Int = -1, nSols: Int = Int.MaxValue, silent: Boolean = false, MIPFocus: Int = 0): SolverResult = {

    if (timeLimit > 0) {
      model.set(GRB.DoubleParam.TimeLimit, timeLimit)
    }

    if (silent) {
      model.set(GRB.IntParam.LogToConsole, 0)
    }

    model.set(GRB.IntParam.MIPFocus, MIPFocus)
    model.set(GRB.IntParam.SolutionLimit, nSols)


    val solutionListener = new SolutionListener(this)
    solutionListener.onSolutionFound(emitSolution)
    model.setCallback(solutionListener)

    val t = time {
      model.optimize()
    }

    new SolverResult {
      lazy val solution: Solution = solutionListener.solution
      val solveTime: Long = t
      override def dispose(): Unit = {
        model.dispose()
      }
    }
  }
}


object MipMain2 extends App {

  val name = "t10d50w300-638"
  val path = s"data/instances/generated/${name}.json"
  val problem = JsonParser.parse(path)
  val cpSearch = new VillageOneLNS(problem)

  val stat = cpSearch.solve(timeLimit = 10 * 1000)

  val model = new MIPSearch(problem)

  if (cpSearch.lastSolution != null) {
    model.setInitialSolution(cpSearch.lastSolution)
  }


  try {

    val solver: SolverResult = model.solve(timeLimit = 60)
    val solution = solver.solution
    println(solution.valid)
    println(solver.solveTime)
    solver.dispose()
    JsonSerializer.serialize(solution)(s"data/results/mip-${name}-o=${solution.objective}.json")

  }
  catch {
    case exception: GRBException => exception.printStackTrace()
  }
}

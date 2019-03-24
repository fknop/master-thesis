package village1.search.mip

import gurobi.{GRB, GRBException}
import village1.generator.{InstanceGenerator, InstanceOptions}
import village1.modeling.mip.{MipModelOptions, VillageOneMIPModel}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.Search
import village1.search.cp.VillageOneLNS
import village1.util.BenchmarkUtils.time

trait MipSolverResult {
  val solution: Solution
  val optimal: Boolean
  val solveTime: Long
}


class MIPSearch(problem: Problem, options: MipModelOptions = MipModelOptions(), base: Option[VillageOneModel] = None) extends VillageOneMIPModel(problem, options, base) with Search {
  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))
  def this(base: VillageOneModel, options: MipModelOptions) = this(base.problem, options, Some(base))

  def presolve(): Unit = {
    model = model.presolve()
  }

  def solve(timeLimit: Int = -1, nSols: Int = Int.MaxValue, silent: Boolean = false, MIPFocus: Int = 1): MipSolverResult = {

    if (timeLimit > 0) {
      model.set(GRB.DoubleParam.TimeLimit, timeLimit)
    }

    if (silent) {
      model.set(GRB.IntParam.LogToConsole, 0)
    }

//    model.set(GRB.IntParam.Symmetry, 2)
    model.set(GRB.IntParam.MIPFocus, MIPFocus)
    model.set(GRB.IntParam.SolutionLimit, nSols)


    val solutionListener = new SolutionListener(this)
    solutionListener.onSolutionFound(emitSolution)
    model.setCallback(solutionListener)

    val t = time {
      model.optimize()
    }

    val status = model.get(GRB.IntAttr.Status)

    if (status == GRB.INFEASIBLE) {
      // TODO: throw infeasible
    }

    model.dispose()

    new MipSolverResult {
      lazy val solution: Solution = solutionListener.solution
      val solveTime: Long = t
      val optimal: Boolean = status == GRB.OPTIMAL
    }
  }
}


object MipMain extends App {

  val name = "t10d50w300-638"
  val path = s"data/instances/generated/${name}.json"
//  val problem = JsonParser.parse(path)
  val generator = new InstanceGenerator()

  val problem = generator.generate(
    InstanceOptions(
      t = 15,
      clients = 10,
      demands = 50,
      workers = 300,
      skills = 10
    )
  )
  val cpSearch = new VillageOneLNS(problem)

  val stat = cpSearch.solve(timeLimit = 10 * 1000)

  val model = new MIPSearch(problem)


  if (cpSearch.lastSolution != null) {
    println(cpSearch.lastSolution.valid)
    model.setInitialSolution(cpSearch.lastSolution)
  }


  try {

    val solver: MipSolverResult = model.solve(timeLimit = 20)
    val solution = solver.solution
    println(solution.valid)
    println(solver.solveTime)
    println(solver.optimal)
//    JsonSerializer.serialize(solution)(s"data/results/mip-${name}-o=${solution.objective}.json")
  }
  catch {
    case exception: GRBException => exception.printStackTrace()
  }
}

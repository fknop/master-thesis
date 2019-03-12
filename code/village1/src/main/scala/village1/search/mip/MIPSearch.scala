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

  class SolutionListener(model: VillageOneMIPModel) extends GRBCallback {

    type WorkerVariablesSolution = Array[Array[Array[Array[Double]]]]

    private var _solution: Solution = null

    def solution: Solution = _solution

    private def getValues(): WorkerVariablesSolution = {
      val variables = model.workerVariables
      variables.map(
        _.map(
          _.map(
            _.map(v =>
              if (v == null) 0 else getSolution(v)
            )
          )
        )
      )
    }

    // Only call this once model is optimized
    private def createSolution (values: WorkerVariablesSolution): Solution = {
      var demandAssignments: Array[DemandAssignment] = Array()

      for (d <- model.Demands) {
        val demand = model.demands(d)
        var workerAssignments: Array[WorkerAssignment] = Array()

        for (t <- demand.periods) {
          var workers = Array[Int]()

          for (p <- demand.positions; w <- model.Workers) {
            val value = values(t)(d)(p)(w)
            if (value == 1.0) {
              workers :+= w
            }
          }

          workerAssignments :+= WorkerAssignment(workers, t)
        }

        demandAssignments :+= DemandAssignment(d, workerAssignments, None, None)
      }


      val objective = this.getDoubleInfo(GRB.CB_MIPSOL_OBJ)
      Solution(model.problem, demandAssignments, objective.toInt)
    }


    override def callback(): Unit = {
      if (where == GRB.CB_MIPSOL) {
        _solution = createSolution(getValues())
        emitSolution(solution)
      }
    }
  }

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

package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.util.Benchmark.time
import village1.util.Utilities

import scala.util.Random

trait SolverResult {
  def dispose(): Unit
  val solution: Solution
  val solveTime: Long
}



object VillageOneMIPModel {
  private val env: GRBEnv = new GRBEnv("mip.log")
  sys.ShutdownHookThread {
    env.dispose()
  }
}

class SolutionListener(model: VillageOneMIPModel) extends GRBCallback {

  type WorkerVariablesSolution = Array[Array[Array[Array[Double]]]]

  private var _solution: Solution = null

  def solution: Solution = _solution

  private def getValues(): WorkerVariablesSolution = {
    val variables = model.variables
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
    }
  }
}


case class MipModelOptions(symmetryBreaking: Boolean = true, objective: Boolean = true)

class VillageOneMIPModel(problem: Problem, options: MipModelOptions = MipModelOptions(), v1model: Option[VillageOneModel] = None) extends VillageOneModel(problem, v1model) {

  def this(v1model: VillageOneModel) = this(v1model.problem, v1model = Some(v1model))
  def this(v1model: VillageOneModel, options: MipModelOptions) = this(v1model.problem, options, v1model = Some(v1model))

  type WorkerVariables = Array[Array[Array[Array[GRBVar]]]]


  private val env = VillageOneMIPModel.env
  val model: GRBModel = new GRBModel(env)
  val variables: WorkerVariables = createWorkersVariables(model)

  initialize()


  def createWorkersVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D) { (t, d) =>
      Array.tabulate(demands(d).requiredWorkers, W)  {(p, w) =>

        val impossible = (!demands(d).periods.contains(t)) ||
          (!possibleWorkersForDemands(d)(t)(p).contains(w))

        if (impossible) null
        else model.addVar(0, 1, 0.0, GRB.BINARY, s"w[$t][$d][$p][$w]")
      }
    }
  }

  private def removeWorkerSymmetries (): Unit = {
    val expression = new GRBLinExpr()
    for (d <- Demands) {
      for (t <- demands(d).periods) {
        val symmetries = Utilities.groupByEquality(possibleWorkersForDemands(d)(t))
        if (symmetries.nonEmpty) {
          val possibleWithoutSymmetries = Utilities.removeSymmetries(possibleWorkersForDemands(d)(t), symmetries)
          for (symmetry <- symmetries) {
            for (p <- symmetry) {
              val possible = possibleWithoutSymmetries(p)
              for (value <- possible) {
                expression.addTerm(1, variables(t)(d)(p)(value))
//                variables(t)(d)(p)(value).set(GRB.DoubleAttr.UB, 0)
              }
            }
          }
        }
      }
    }

    model.addConstr(expression, GRB.EQUAL, 0, "symmetries")
  }

  // TODO: remove constraints and add to initialization
  def removeImpossibleValues (model: GRBModel, variables: WorkerVariables): Unit = {
    val expression = new GRBLinExpr()
    for (t <- Periods; d <- Demands; w <- Workers) {

      val impossible = (!demands(d).periods.contains(t)) ||
                       (!availableWorkers(d).contains(t)) ||
                       (!availableWorkers(d)(t).contains(w))

      if (impossible) {
        for (p <- demands(d).positions) {
         // variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0.0)
          expression.addTerm(1, variables(t)(d)(p)(w))
//          model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"imp[$t][$d][$p][$w]")
        }
      }
    }

    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val workers = possibleWorkersForDemands(d)(t)(p)
      for (w <- allWorkers.diff(workers)) {
//        variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0)
        expression.addTerm(1, variables(t)(d)(p)(w))
//        model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"requiredSkill[$t][$d][$p][$w]")
      }
    }

    model.addConstr(expression, GRB.EQUAL, 0, s"impossibleValues")
  }



  def allDifferentWorkers (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- Periods; w <- Workers) {
      val expression = new GRBLinExpr()
      for (d <- Demands if demands(d).periods.contains(t); p <- demands(d).positions) {
        if (variables(t)(d)(p)(w) != null) {
          expression.addTerm(1, variables(t)(d)(p)(w))
        }
      }

      model.addConstr(expression, GRB.LESS_EQUAL, 1, s"c1[$t][$w]")
    }
  }

  def workerNumberSatisfied (model: GRBModel, variables: WorkerVariables): Unit = {
    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val expression = new GRBLinExpr()
      for (w <- possibleWorkersForDemands(d)(t)(p)) {
        expression.addTerm(1, variables(t)(d)(p)(w))
      }

      model.addConstr(expression, GRB.EQUAL, 1, s"c2[$t][$d][$p]")
    }
  }

  def workerWorkerIncompatibilities (model: GRBModel, variables: WorkerVariables): Unit = {
    val incompatibilities = problem.workerWorkerIncompatibilities
    for (incompatibility <- incompatibilities) {
      val w0 = incompatibility(0)
      val w1 = incompatibility(1)

      for (t <- Periods; d <- Demands if demands(d).periods.contains(t)) {
        val expression = new GRBLinExpr()
        for (p <- demands(d).positions) {
          expression.addTerm(1.0, variables(t)(d)(p)(w0))
          expression.addTerm(1.0, variables(t)(d)(p)(w1))
        }
        model.addConstr(expression, GRB.LESS_EQUAL, 1, s"Iww[$w0][$w1]")
      }

    }
  }


  def workerClientIncompatibilities (model: GRBModel, variables: WorkerVariables): Unit = {
    val incompatibilities = problem.workerClientIncompatibilities
    for (incompatibility <- incompatibilities) {
      val iw = incompatibility(0)
      val ic = incompatibility(1)


      for (d <- Demands; t <- demands(d).periods) {
        if (demands(d).client == ic) {
          for (p <- demands(d).positions) {
            model.addConstr(variables(t)(d)(p)(iw), GRB.EQUAL, 0, s"iwc[$t][$d][$p][$iw]")
          }
        }
      }
    }
  }
//
//  def applySkills (model: GRBModel, variables: WorkerVariables): Unit = {
//    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
//      val workers = possibleWorkersForDemands(d)(t)(p)
//      for (w <- allWorkers.diff(workers)) {
//        model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"requiredSkill[$t][$d][$p][$w]")
//      }
//    }
//  }


  // TODO: this works for now for simple models - check for larger ones
  /**
    * Minimize shift change between workers at one position
    */
  def minimizeShiftChange (model: GRBModel, variables: WorkerVariables): Unit = {
    val expression = new GRBLinExpr()
    for (d <- Demands; p <- demands(d).positions) {

      for (w <- Workers) {
        // All the variables for this worker at demand d and position p
        val vars = demands(d).periods.map(variables(_)(d)(p)(w)).filterNot(_ == null).toArray

        // Binary variable: is the worker working for that shift at at least one time period ?
        val isWorking = model.addVar(0, 1.0, 0, GRB.BINARY, s"isWorker[$d][$p][$w]")
        model.addGenConstrOr(isWorking, vars, s"isWorkingConstr[$d][$p][$w]")

        expression.addTerm(1, isWorking)
      }
    }

    // Minimize the number of working workers at each position
    model.setObjective(expression, GRB.MINIMIZE)
  }

  def setInitialSolution(solution: Solution): Unit = {
          val rand = new Random(0)
    for (demandAssignment <- solution.plannings) {
      val d = demandAssignment.demand
      val workerAssignments = demandAssignment.workerAssignments
      for (assignment <- workerAssignments) {
        val t = assignment.timeslot
        val workers = assignment.workers
        for (i <- workers.indices) {
          if (rand.nextInt(100) < 50) {
            variables(t)(d)(i)(workers(i)).set(GRB.DoubleAttr.Start, 1.0)
          }
        }
      }
    }
  }

  def applyObjectives (model: GRBModel, variables: WorkerVariables): Unit = {
    minimizeShiftChange(model, variables)
  }

  def applyConstraints (model: GRBModel, variables: WorkerVariables): Unit = {
//    removeImpossibleValues(model, variables)
    allDifferentWorkers(model, variables)
    workerNumberSatisfied(model, variables)
    workerWorkerIncompatibilities(model, variables)
    workerClientIncompatibilities(model, variables)
  }




  private def initialize(): Unit = {

    if (options.symmetryBreaking) {
      removeWorkerSymmetries()
    }

    applyConstraints(model, variables)

    if (options.objective) {
      applyObjectives(model, variables)
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

  val model = new VillageOneMIPModel(problem)

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
package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.search.cp.{VillageOneLNS, VillageOneSearch}
import village1.util.Benchmark.time

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

class Callback(model: VillageOneMIPModel) extends GRBCallback {
  override def callback(): Unit = {
    if (where == GRB.CB_MIPSOL) {
      val solution = model.createSolution()
      println("New solution found: " + solution.objective)
    }
  }
}

class VillageOneMIPModel(problem: Problem, v1model: Option[VillageOneModel] = None) extends VillageOneModel(problem, v1model) {

  def this(v1model: VillageOneModel) = this(v1model.problem, Some(v1model))

  type WorkerVariables = Array[Array[Array[Array[GRBVar]]]]


  private val env = VillageOneMIPModel.env
  private val model: GRBModel = new GRBModel(env)
  val variables: WorkerVariables = createWorkersVariables(model)


  def createWorkersVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D) { (t, d) =>
      Array.tabulate(demands(d).requiredWorkers, W)  {(p, w) =>
        model.addVar(0, 1, 0.0, GRB.BINARY, s"w[$t][$d][$p][$w]")
      }
    }
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
        expression.addTerm(1, variables(t)(d)(p)(w))
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
        val vars = demands(d).periods.map(variables(_)(d)(p)(w)).toArray

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
    removeImpossibleValues(model, variables)
    allDifferentWorkers(model, variables)
    workerNumberSatisfied(model, variables)
    workerWorkerIncompatibilities(model, variables)
    workerClientIncompatibilities(model, variables)
  }

  // Only call this once model is optimized
  def createSolution (): Solution = {
    var demandAssignments: Array[DemandAssignment] = Array()

    for (d <- Demands) {
      val demand = demands(d)
      var workerAssignments: Array[WorkerAssignment] = Array()

      for (t <- demand.periods) {
        var workers = Array[Int]()

        for (p <- demand.positions; w <- Workers) {
          val variable = variables(t)(d)(p)(w)
          val value = variable.get(GRB.DoubleAttr.X)
          if (value == 1.0) {
            workers :+= w
          }
        }

        workerAssignments :+= WorkerAssignment(workers, t)
      }

      demandAssignments :+= DemandAssignment(d, workerAssignments, None, None)
    }


    val objective = model.get(GRB.DoubleAttr.ObjVal)
    Solution(problem, demandAssignments, objective.toInt)
  }



  def initialize(withObjective: Boolean = false): Unit = {
    applyConstraints(model, variables)

    if (withObjective) {
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

    model.setCallback(new Callback(this))

    val t = time {
      model.optimize()
    }

    new SolverResult {
      lazy val solution: Solution = createSolution()
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
  val cpSearch = new VillageOneSearch(problem)

  val stat = cpSearch.solve(nSols = 1)

  println(stat)
  val model = new VillageOneMIPModel(problem)
  model.initialize(withObjective = true)

  if (cpSearch.lastSolution != null) {
    model.setInitialSolution(cpSearch.lastSolution)
  }


  try {

    val solver: SolverResult = model.solve()
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
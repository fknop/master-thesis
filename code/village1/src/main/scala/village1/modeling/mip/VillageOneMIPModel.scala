package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.util.Benchmark.time

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


class VillageOneMIPModel(problem: Problem, v1model: Option[VillageOneModel] = None) extends VillageOneModel(problem, v1model) {

  def this(v1model: VillageOneModel) = this(v1model.problem, Some(v1model))

  type WorkerVariables = Array[Array[Array[Array[GRBVar]]]]

  private val env = VillageOneMIPModel.env
  private val model: GRBModel = new GRBModel(env)
  private val variables: WorkerVariables = createWorkersVariables(model)


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
          variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0.0)
//          expression.addTerm(1, variables(t)(d)(p)(w))
//          model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"imp[$t][$d][$p][$w]")
        }
      }
    }

    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val workers = possibleWorkersForDemands(d)(t)(p)
      for (w <- allWorkers.diff(workers)) {
        variables(t)(d)(p)(w).set(GRB.DoubleAttr.UB, 0)
//        expression.addTerm(1, variables(t)(d)(p)(w))
//        model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"requiredSkill[$t][$d][$p][$w]")
      }
    }

//    model.addConstr(expression, GRB.EQUAL, 0, s"impossibleValues")
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

  def applySkills (model: GRBModel, variables: WorkerVariables): Unit = {
    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val workers = possibleWorkersForDemands(d)(t)(p)
      for (w <- allWorkers.diff(workers)) {
        model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"requiredSkill[$t][$d][$p][$w]")
      }
    }
  }


  // TODO: this works for now for simple models - check for larger ones
  /**
    * Minimize shift change between workers at one position
    */
  def minimizeShiftChange (model: GRBModel, variables: WorkerVariables): Unit = {
    val expressions = new GRBLinExpr()
    for (d <- Demands; p <- demands(d).positions) {

      for (w <- Workers) {
        val expression = new GRBLinExpr()
        val sum = model.addVar(0, GRB.INFINITY, 0, GRB.INTEGER, s"sum[$d][$p][$w]")

        expression.addTerm(-1, sum)
        for (t <- demands(d).periods) {
          expression.addTerm(1, variables(t)(d)(p)(w))
        }

        // -sum + w_0jkl + w_1jkl + ... w_ijkl = 0
        // w_0jkl + w_1jkl + ... w_ijkl = sum
        model.addConstr(expression, GRB.EQUAL, 0, s"sameShifts[$d][$p][$w]")

        // min(sum, 1)
        // min(w_0jkl + w_1jkl + ... w_ijkl, 1)
        val min = model.addVar(0, GRB.INFINITY, 0, GRB.INTEGER, s"objMin[$d][$p][$w]")
        model.addGenConstrMin(min, Array(sum), 1, s"constrMin[$d][$p][$w]")

        // The sum of each min variable for each worker represent the number of different workers
        // for a position, we need to minimize the number of different workers for that demand.
        expressions.addTerm(1, min)
      }
    }

    model.setObjective(expressions, GRB.MINIMIZE)
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
//    applySkills(model, variables)
  }

  // Only call this once model is optimized
  def createSolution (variables: WorkerVariables): Solution = {
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


    Solution(problem, demandAssignments)
  }



  def initialize(withObjective: Boolean = false): Unit = {
    applyConstraints(model, variables)

    if (withObjective) {
      applyObjectives(model, variables)
    }
  }

  def solve(timeLimit: Int = -1, consoleLog: Boolean = true): SolverResult = {

    if (timeLimit > 0) {
      model.set(GRB.DoubleParam.TimeLimit, timeLimit)
    }

    if (!consoleLog) {
      model.set(GRB.IntParam.LogToConsole, 0)
    }

    val t = time {
      model.optimize()
    }

    new SolverResult {
      lazy val solution: Solution = createSolution(variables)
      val solveTime: Long = t
      override def dispose(): Unit = {
        model.dispose()
      }
    }
  }
}


object MipMain2 extends App {



  val model = new VillageOneMIPModel(JsonParser.parse("data/instances/generated/t10d50w300-943.json"))
//  val model = new VillageOneMIPModel2(JsonParser.parse("data/instances/generated/t5d5w20-491.json"))
//  val model = new VillageOneMIPModel2(JsonParser.parse("data/instances/problem2.json"))
  println(model.precomputeTime)

  model.initialize()


  try {

    val solver: SolverResult = model.solve()
    val solution = solver.solution
    println(solution.valid)
    println(solver.solveTime)
    solver.dispose()
    JsonSerializer.serialize(solution)("data/results/mip.json")

  }
  catch {
    case exception: GRBException => exception.printStackTrace()
  }
}
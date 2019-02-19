package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, WorkerAssignment}
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}

trait SolveResult {
  def dispose(): Unit
  def solution: Solution
}


class VillageOneMIPModel2(problem: Problem) extends VillageOneModel(problem) {

  type WorkerVariables = Array[Array[Array[Array[GRBVar]]]]

  // TODO: log file path
  def createEnvironment() = new GRBEnv("mip.log")
  def createModel(env: GRBEnv) = new GRBModel(env)

  def createWorkersVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D) { (t, d) =>
      Array.tabulate(demands(d).requiredWorkers, W)  {(p, w) =>
        model.addVar(0, 1, 0.0, GRB.BINARY, s"w[$t][$d][$p][$w]")
      }
    }
  }

  // TODO: remove constraints and add to initialization
  def removeImpossibleValues (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- Periods; d <- Demands; w <- Workers) {

      val impossible = (!demands(d).periods.contains(t)) ||
                       (!availableWorkers(d).contains(t)) ||
                       (!availableWorkers(d)(t).contains(w))

      if (impossible) {
        for (p <- demands(d).positions) {
          model.addConstr(variables(t)(d)(p)(w), GRB.EQUAL, 0, s"imp[$t][$d][$p][$w]")
        }
      }
    }
  }



  def allDifferentWorkers (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- Periods; w <- Workers) {
      val expression = new GRBLinExpr()
      for (d <- Demands; p <- demands(d).positions) {
        expression.addTerm(1, variables(t)(d)(p)(w))
      }

      model.addConstr(expression, GRB.LESS_EQUAL, 1, s"c1[$t][$w]")
    }
  }

  def workerNumberSatisfied (model: GRBModel, variables: WorkerVariables): Unit = {
    for (d <- Demands; t <- demands(d).periods; p <- demands(d).positions) {
      val expression = new GRBLinExpr()
      for (w <- Workers) {
        expression.addTerm(1, variables(t)(d)(p)(w))
      }

      model.addConstr(expression, GRB.EQUAL, 1, s"c2[$t][$d]")
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
    applySkills(model, variables)
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


  def solve(): SolveResult = {

    val env = createEnvironment()
    val model = createModel(env)
    val variables = createWorkersVariables(model)
    applyConstraints(model, variables)
    applyObjectives(model, variables)

    model.write("mip.lp")

    model.optimize()

    new SolveResult {
      override def solution: Solution = createSolution(variables)
      override def dispose(): Unit = {
        model.dispose()
        env.dispose()
      }
    }
  }

}


object MipMain2 extends App {



  val model = new VillageOneMIPModel2(JsonParser.parse("data/instances/generated/t5d5w20-491.json"))
//  val model = new VillageOneMIPModel2(JsonParser.parse("data/instances/problem2.json"))

  try {

    val solver: SolveResult = model.solve()
    val solution = solver.solution
    println(solution.valid())
    solver.dispose()
    JsonSerializer.serialize(solution)("results/mip.json")

  }
  catch {
    case exception: GRBException => exception.printStackTrace
  }
}
package village1.modeling.mip

import gurobi._
import village1.data.{DemandAssignment, Worker, WorkerAssignment}
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}

class VillageOneMIPModel(problem: Problem) extends VillageOneModel(problem) {

  type WorkerVariables = Array[Array[Array[GRBVar]]]





  // TODO: log file path
  def createEnvironment() = new GRBEnv("mip.log")
  def createModel(env: GRBEnv) = new GRBModel(env)

  def createWorkersVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D, W) { (t, d, w) =>
      val variable = model.addVar(0, 1, 0.0, GRB.BINARY, s"w[$t][$d][$w]")
//      if (!availableWorkers(t).contains(w)) {
//        variable.set(GRB.DoubleAttr.X, 0.0)
//      }

      variable
    }
  }

  // TODO: move set to init
  def removeImpossibleValues (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- 0 until T; d <- Demands; w <- Workers) {

      val impossible = (!demands(d).periods.contains(t)) ||
                       (!availableWorkers(d).contains(t)) ||
                       (!availableWorkers(d)(t).contains(w))

      if (impossible) {
        model.addConstr(variables(t)(d)(w), GRB.EQUAL, 0, s"imp[$t][$d][$w]")
      }
    }
  }



  def allDifferentWorkers (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- 0 until T; w <- Workers) {
      val expression = new GRBLinExpr()
      for (d <- Demands) {
        expression.addTerm(1, variables(t)(d)(w))
      }

      model.addConstr(expression, GRB.LESS_EQUAL, 1, s"c1[$t][$w]")
    }
  }

  def workerNumberSatisfied (model: GRBModel, variables: WorkerVariables): Unit = {
    for (d <- Demands; t <- demands(d).periods) {
      val expression = new GRBLinExpr()
      for (w <- Workers) {
        expression.addTerm(1, variables(t)(d)(w))
      }

      model.addConstr(expression, GRB.EQUAL, demands(d).requiredWorkers, s"c2[$t][$d]")
    }
  }



  def applyConstraints (model: GRBModel, variables: WorkerVariables): Unit = {
    removeImpossibleValues(model, variables)
    allDifferentWorkers(model, variables)
    workerNumberSatisfied(model, variables)
  }

  // Only call this once model is optimized
  def createSolution (variables: WorkerVariables): Solution = {

    var demandAssignments: Array[DemandAssignment] = Array()

    for (d <- Demands) {
      val demand = demands(d)
      var workerAssignments: Array[WorkerAssignment] = Array()

      for (t <- demand.periods) {
        var workers = Array[Int]()

        for (w <- Workers) {
          val variable = variables(t)(d)(w)
          val value = variable.get(GRB.DoubleAttr.X)
          if (value == 1.0) {
            workers :+= w
          }
        }

        workerAssignments :+= WorkerAssignment(workers.toArray, t)
      }

      demandAssignments :+= DemandAssignment(d, workerAssignments, None, None)
    }


    Solution(problem, demandAssignments)
  }


  def solve(): () => Unit = {

    val env = createEnvironment()
    val model = createModel(env)
    val variables = createWorkersVariables(model)
    applyConstraints(model, variables)


    model.optimize()

    val solution = createSolution(variables)
    JsonSerializer.serialize(solution)("results/mip.json")

    () => {
      model.dispose()
      env.dispose()
    }
  }

}


object MipMain extends App {



  val model = new VillageOneMIPModel(JsonParser.parse("data/instances/problem.json"))

  try {

    model.solve()()
  }
  catch {
    case exception: GRBException => exception.printStackTrace
  }
}
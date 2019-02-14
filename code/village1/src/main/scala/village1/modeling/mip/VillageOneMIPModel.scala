package village1.modeling.mip

import gurobi._
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}

class VillageOneMIPModel(problem: Problem) extends VillageOneModel(problem) {

  type WorkerVariables = Array[Array[Array[GRBVar]]]





  // TODO: log file path
  def createEnvironment() = new GRBEnv("mip.log")
  def createModel(env: GRBEnv) = new GRBModel(env)

  def createWorkersVariables (model: GRBModel): WorkerVariables = {

    Array.tabulate(T, D, W) { (t, d, w) =>
      val variable = model.addVar(0.0, 1.0, 0.0, GRB.BINARY, s"w[$t][$d][$w]")
      if (!availableWorkers(t).contains(w)) {
        variable.set(GRB.DoubleAttr.X, 0.0)
      }

      variable
    }
  }

  def removeImpossibleValues (variables: WorkerVariables): Unit = {
    for (t <- 0 until T; d <- Demands; w <- Workers) {

      if (!demands(d).periods.contains(t)) {
        variables(t)(d)(w).set(GRB.DoubleAttr.X, 0.0)
      }
      else if (!availableWorkers(t).contains(w)) {
        variables(t)(d)(w).set(GRB.DoubleAttr.X, 0.0)
      }
    }
  }



  def allDifferentWorkers (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- 0 until T; w <- Workers) {
      val expression = new GRBLinExpr()
      for (d <- Demands) {
        expression.addTerm(1.0, variables(t)(d)(w))
      }

      model.addConstr(expression, GRB.LESS_EQUAL, 1.0, s"c1[$t][$w]")
    }
  }

  def workerNumberSatisfied (model: GRBModel, variables: WorkerVariables): Unit = {
    for (t <- 0 until T; d <- Demands) {
      val expression = new GRBLinExpr()
      for (w <- Workers) {
        expression.addTerm(1.0, variables(t)(d)(w))
      }

      model.addConstr(expression, GRB.EQUAL, demands(d).requiredWorkers, s"c2[$t][$d]")
    }
  }



  def applyConstraints (model: GRBModel, variables: WorkerVariables): Unit = {
    removeImpossibleValues(variables)
    allDifferentWorkers(model, variables)
    workerNumberSatisfied(model, variables)
  }

  // Only call this once model is optimized
  def createSolution (variables: WorkerVariables): Unit = {
    for (t <- 0 until T; d <- Demands; w <- Workers) {
      val variable = variables(t)(d)(w)
      println(variable.get(GRB.StringAttr.VarName) + " = " + variable.get(GRB.DoubleAttr.X))
    }
  }


  def solve(): () => Unit = {

    val env = createEnvironment()
    val model = createModel(env)
    val variables = createWorkersVariables(model)
    applyConstraints(model, variables)

    model.optimize()

    createSolution(variables)

    () => {
      model.dispose()
      env.dispose()
    }
  }

}


object MipMain extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "instance-t=10-d=30-w=400-350.json",
    "instance-t=7-d=5-w=20-985.json"
  ).map(f => s"$generatedFolder/$f")



  val model = new VillageOneMIPModel(JsonParser.parse("data/instances/problem.json"))
  model.solve()()
}
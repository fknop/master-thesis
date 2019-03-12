package village1.search.cp

import oscar.algo.search.SearchStatistics
import oscar.cp._
import village1.json.{JsonParser, JsonSerializer}
import village1.modeling.cp.{CPModelOptions, VillageOneCPModel}
import village1.modeling.{Problem, VillageOneModel}
import village1.search.Search

class VillageOneSearch(problem: Problem, options: CPModelOptions = CPModelOptions(), base: Option[VillageOneModel] = None) extends VillageOneCPModel(problem, options, base) with Search {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))


  def solve(nSols: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, silent: Boolean = false): SearchStatistics = {

    solver.silent = silent

    val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
    val flatMachines: Array[CPIntVar] = machineVariables.flatten
    val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

    val heuristic = new MostAvailableHeuristic(this, flatWorkers)

    minimize(objective)
    search {

      var branching = binaryIdx(flatWorkers, heuristic.varHeuristic, heuristic.valueHeuristic)

      if (flatMachines.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatLocations)
      }

      branching
    }

    onSolution {
      emitSolution(createSolution())
    }

    start(nSols = nSols, timeLimit = timeLimit / 1000)
  }
}

object Main extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem2.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "t5d5w20-491.json",
    "t10d50w300-638.json"
  ).map(f => s"$generatedFolder/$f")


  val problem = JsonParser.parse(instance)
  val search = new VillageOneSearch(problem)
  val stats = search.solve(nSols = 1, timeLimit = 20 * 1000)

  println(stats)
  if (search.lastSolution != null) {
    JsonSerializer.serialize(search.lastSolution)("results/results3.json")
    println(search.lastSolution.valid)
  }

}
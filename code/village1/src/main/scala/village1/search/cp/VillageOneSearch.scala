package village1.search.cp

import oscar.algo.search.SearchStatistics
import oscar.util.time
import oscar.cp._
import village1.data._
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.{Problem, Solution, VillageOneModel}
import village1.modeling.cp.VillageOneCPModel

class VillageOneSearch(problem: Problem, baseModel: Option[VillageOneModel] = None) extends VillageOneCPModel(problem, baseModel) with Search {

  def this(baseModel: VillageOneModel) = this(baseModel.problem, Some(baseModel))

  def solve(nSols: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue): SearchStatistics = {

    val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
    val flatMachines: Array[CPIntVar] = machineVariables.flatten
    val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

    search {

      var branching = binaryFirstFail(flatWorkers)

      if (flatMachines.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatLocations)
      }

      branching ++= binarySplit(shiftDifferences)

      branching
    }

    onSolution {
      emitSolution(createSolution())
    }

    // use restarts to break heavy tails phenomena
    start(nSols = nSols, timeLimit = timeLimit)
  }
}

object Main extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "t5d5w20-491.json",
    "t10d50w300-638.json"
  ).map(f => s"$generatedFolder/$f")


  val problem = JsonParser.parse(generatedInstances(1))
  val search = new VillageOneSearch(problem)
  val stats = search.solve(nSols = 10)

  println(stats)
  if (search.lastSolution != null) {
    JsonSerializer.serialize(search.lastSolution)("results/results3.json")
    println(search.lastSolution.valid)
  }

}
package village1.search.cp

import oscar.cp._
import village1.json.{JsonParser, JsonSerializer}
import village1.modeling.cp.{CPModelOptions, VillageOneCPModel}
import village1.modeling.{Problem, VillageOneModel}
import village1.search.cp.heuristic.MostAvailableHeuristic
import village1.search.{Search, SearchResult}

case class CPSearchOptions()

class VillageOneSearch(problem: Problem, options: CPModelOptions = CPModelOptions(), base: Option[VillageOneModel] = None)
  extends VillageOneCPModel(problem, options, base)
  with Search[CPSearchOptions] {

  def this(base: VillageOneModel) = this(problem = base.problem, base = Some(base))

  override def solve(timeLimit: Int = Int.MaxValue,
                     solutionLimit: Int = Int.MaxValue,
                     silent: Boolean = false,
                     options: Option[CPSearchOptions] = None
                    ): SearchResult = {

    solver.silent = silent

    val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
    val flatMachines: Array[CPIntVar] = machineVariables.flatten
    val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

    val heuristic = new MostAvailableHeuristic(this, flatWorkers, workerVariables)

    minimize(objective)
    search {
      var branching = heuristic.branching

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


    val stats = start(nSols = solutionLimit, timeLimit = timeLimit)

    SearchResult(lastSolution, stats.time, stats.completed)
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


  val problem = JsonParser.parse("data/test/not-enough-workers.json")
  val search = new VillageOneSearch(problem)
  val result = search.solve()


  val solution = search.lastSolution

  solution match {
    case Some(s) =>
      JsonSerializer.serialize(s)(s"data/results/results-cp.json")
      println(s.valid)
      println("Partial: " + s.partial)
    case _ => println("No solution found")
  }

}
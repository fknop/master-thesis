package village1.search.cp

import oscar.algo.search.SearchStatistics
import oscar.util.time
import oscar.cp._
import village1.data._
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.Solution
import village1.modeling.cp.VillageOneCPModel

class VillageOneSearch(path: String) extends VillageOneCPModel(JsonParser.parse(path)) with Search {

  def solve(nSols: Int = Int.MaxValue): SearchStatistics = {

    search {
      val flatWorkers: Array[CPIntVar] = workerVariables.flatten.flatten
      val flatMachines: Array[CPIntVar] = machineVariables.flatten
      val flatLocations: Array[CPIntVar] = locationVariables.filter(_ != null)

      var branching = binaryFirstFail(flatWorkers)

      if (flatMachines.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatMachines)
      }

      if (flatLocations.nonEmpty) {
        branching = branching ++ binaryFirstFail(flatLocations)
      }

      /*binarySplit(sameWorkerViolations) ++ */

      branching
    }

    onSolution {

      println("Solution found")

      var demandAssignments: Array[DemandAssignment] = Array()

      for (d <- Demands) {
        val demand = demands(d)
        val slots = demand.periods

        val machineValues = machineVariables(d)
        val locationValue = locationVariables(d)

        val machineAssignments: Option[Array[Int]] =
          if (machineValues.nonEmpty)
            Some(machineValues.map(_.value))
          else
            None

        val locationAssignment =
          if (locationValue != null) Some(locationValue.value)
          else None

        var workerAssignments: Array[WorkerAssignment] = Array()

        for (t <- slots) {
          val workerValues = workerVariables(t)(d)

          if (workerValues.nonEmpty) {
            val workers: Array[Int] = workerValues.map(_.value)
            val assignment = WorkerAssignment(workers, t)
            workerAssignments :+= assignment
          }
        }

        demandAssignments :+= DemandAssignment(d, workerAssignments, machineAssignments, locationAssignment)
      }

      emitSolution(Solution(problem, demandAssignments))
    }

    // use restarts to break heavy tails phenomena
    start(nSols = nSols, failureLimit = 50000)
  }
}

object Main extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem.json"
  val generatedFolder = s"$folder/generated/"
  val generatedInstances: Array[String] = Array(
    "t5d5w20-491.json"
  ).map(f => s"$generatedFolder/$f")


  val search = new VillageOneSearch(generatedInstances(0))
  val stats = search.solve(nSols = 1)
  println(stats)
  if (search.lastSolution != null) {
    JsonSerializer.serialize(search.lastSolution)("results/results3.json")
    println(search.lastSolution.valid())
  }

}
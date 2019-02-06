package village1.search.cp

import oscar.util.time
import oscar.cp._
import village1.data._
import village1.format.json.{JsonParser, JsonSerializer}
import village1.modeling.Solution
import village1.modeling.cp.VillageOneCPModel

class VillageOneSearch(path: String) extends VillageOneCPModel(JsonParser.parse(path)) with Search {

  def solve(): Unit = {


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

      var demandAssignments: List[DemandAssignment] = List()

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

        var workerAssignments: List[WorkerAssignment] = List()

        for (t <- slots) {
          val workerValues = workerVariables(t)(d)

          if (workerValues.nonEmpty) {
            val workers: Array[Int] = workerValues.map(_.value)
            val assignment = WorkerAssignment(workers, t)
            workerAssignments = assignment :: workerAssignments
          }
        }

        demandAssignments = DemandAssignment(d, workerAssignments, machineAssignments, locationAssignment) :: demandAssignments
      }

      emitSolution(Solution(demandAssignments))
    }

    // use restarts to break heavy tails phenomena
    val t = time {
      val stats = start(nSols = 100, failureLimit = 50000)
      println(stats)
    }
  }
}

object Main extends App {

  val folder = "data/instances"
  val instance = s"$folder/problem.json"
  val generatedInstances: Array[String] = Array(
    s"$folder/generated/instance-t=10-d=30-w=400-122.json"
  )

  val search = new VillageOneSearch(instance)
  search.onSolutionFound { solution =>
    JsonSerializer.serialize(solution)("results/problem.json")
  }
  search.solve()
}
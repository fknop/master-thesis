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
      val flatVars = workerVariables.flatten.flatten
      //    val flatVehiclesVars = vehicleVars.flatten.flatten

      val variables = flatVars ++ sameWorkerViolations //++ flatVehiclesVars
      conflictOrderingSearch(variables, variables(_).size, variables(_).min)
    }

    var solFound = false
    onSolution {

      solFound = true
      println("sol found")

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
    var restart = 0
    val t = time {
      do {
        start(nSols = 1, failureLimit = 5000)
        restart += 1
      } while (!solFound)
    }
  }
}

object Main extends App {

  val search = new VillageOneSearch("data/instances/problem.json")
  search.onSolutionFound { solution =>
    JsonSerializer.serialize(solution)("results/problem.json")
  }
  search.solve()
}
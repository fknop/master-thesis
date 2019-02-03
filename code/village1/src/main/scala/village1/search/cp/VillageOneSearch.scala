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

      val variables = flatVars //++ flatVehiclesVars
      conflictOrderingSearch(variables, variables(_).size, variables(_).min)
    }

    var solFound = false
    onSolution {

      var plannings = List[Planning]()

      solFound = true
      println("sol found")

      for (t <- Periods) {
        var demandAssignments: List[DemandAssignment] = List()

        for (d <- Demands) {
          val demand = demands(d)
          if (demand.periods.contains(t)) {
            val workerValues = workerVariables(t)(d)
            val machineValues = machineVariables(t)(d)
            val locationValue = locationVariables(t)(d)

            var workerAssignments: List[WorkerAssignment] = List()
            var machineAssignments: List[MachineAssignment] = List()
            var locationAssignments: List[LocationAssignment] = List()

            if (!workerValues.isEmpty) {
              workerAssignments = workerValues
                  .map(_.value)
                  .map(workers(_))
                  .map(worker => WorkerAssignment(worker, demand, t)) ++: workerAssignments
            }

            if (!machineValues.isEmpty) {
              machineAssignments = machineValues
                  .map(_.value)
                  .map(machine => MachineAssignment(machine, demand, t)) ++: machineAssignments
            }

            if (locationValue != null) {
              locationAssignments = LocationAssignment(locationValue.value, demand, t) :: locationAssignments
            }

            demandAssignments = DemandAssignment(t, demand, workerAssignments, locationAssignments, machineAssignments) :: demandAssignments
          }
        }

        plannings = Planning(t, demandAssignments) :: plannings
      }

      emitSolution(Solution(plannings))
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
    JsonSerializer.serialize(solution)("results/results.json")
  }
  search.solve()
}
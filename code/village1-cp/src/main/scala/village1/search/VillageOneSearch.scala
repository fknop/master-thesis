package village1.search

import oscar.cp.{conflictOrderingSearch, onSolution, search, start}
import oscar.util.time
import village1.modeling.VillageOneCPModel

object VillageOneSearch extends VillageOneCPModel with App {

  search {
    val flatVars = workerVars.flatten.flatten
//    val flatVehiclesVars = vehicleVars.flatten.flatten

    val variables = flatVars //++ flatVehiclesVars
    conflictOrderingSearch(variables, i => variables(i).size, i => variables(i).min)
  }


  var solFound = false
  onSolution {
    solFound = true
    println("sol found")

    for (period <- Periods) {
      for (demand <- Demands) {
        val workersTD = workerVars(period)(demand)
        if (!workersTD.isEmpty) {
          println(s"Period = $period, demand = $demand")
          workersTD.map(_.value).map(workers(_).name).foreach(println(_))
//          println("Workers: " + workerVars(period)(demand).deep)

//          val vehicles = vehicleVars(period)(demand)
//          if (!vehicles.isEmpty) {
//            println(s"Vehicles: ${vehicles.map(_.value).mkString(" ")}")
//          }
        }
      }
    }
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

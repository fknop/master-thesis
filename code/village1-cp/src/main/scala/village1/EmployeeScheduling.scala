package village1

import village1.data.{Demand, Skill, Worker}
import oscar.cp._
import oscar.util._
import village1.format.json.JsonSerializer

object EmployeeScheduling extends CPModel with App {



  val T = 5

  val workers = Array(
    Worker(0, Set(0, 1, 2)),
    Worker(0, Set(0, 1, 2)),
    Worker(1, Set(1, 2, 3)),
    Worker(2, Set(3, 4, 0))
  )

  val demands = Array(
    Demand(0, Set(0, 1, 2), 2),
    Demand(0, Set(1, 2, 3), 1),
    Demand(0, Set(0, 3, 4), 1)
  )

  val V = 2

  val W = workers.length
  val D = demands.length

  val Demands = 0 until D
  val Workers = 0 until W
  val Periods = 0 until T
  val Vehicles = 0 until V


  // Array 3 x 5
  // Array[i][j] does worker i works on time j
  // For all i Array[i], sum j Array[i][j] <= 1 for all j
  val workerVars = Array.tabulate(W, T, D)((n, t, d) => {
    val demand = demands(d)
    val worksOnTForD = workers(n).availabilities.contains(t) && demand.periods.contains(t)
    val name = s"Worker $n works for demand: $d at time $t"

    // We're sure that the worker is not available for period t or demand is not "working" for period t
    if (!worksOnTForD) {
      CPBoolVar(worksOnTForD, name)
    }
    else {
      CPBoolVar(name)
    }
  })


  val vehicleVars = Array.tabulate(V, T, D)((v, t, d) => {
    CPBoolVar(s"Vehicle $v at time $t for demand $d")
  })

  // Worker cannot work for two demands at the same time
  for (worker <- Workers) {
    for (time <- Periods) {
      val workerTD = workerVars(worker)(time)
      add(atMost(1, workerTD, 1))
    }
  }

  // Demand need to have the required number of workers
  for (time <- Periods) {
    for (demand <- Demands) {
      val workerTD = for (worker <- Workers) yield workerVars(worker)(time)(demand)
      val nWorkers = demands(demand).workers
      add(countEq(CPIntVar(nWorkers), workerTD, 1))
    }
  }


  // Vehicle cannot be used for two demands at the same time
  for (vehicle <- Vehicles) {
    for (time <- Periods) {
      val vehicleTD = vehicleVars(vehicle)(time)
      add(atMost(1, vehicleTD, 1))
    }
  }

  // Demand need the required number of vehicles
  for (time <- Periods) {
    for (demand <- Demands) {
      val vehicleTD = for (vehicle <- Vehicles) yield vehicleVars(vehicle)(time)(demand)
      val nVehicles = demands(demand).vehicles
      add(countEq(CPIntVar(nVehicles), vehicleTD, 1))
    }
  }



  search {
    binary(workerVars.flatten.flatten.toSeq, _.size, _.randomValue) // our randomized solution
  }

  var solFound = false
  onSolution {
    solFound = true
    println("sol found")

    for (d <- Demands) {
      for (t <- Periods) {
        for (w <- Workers) {
          val variable = workerVars(w)(t)(d)
          if (variable.isTrue) {
            println(s"d = $d, t = $t, w = $w")
          }
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


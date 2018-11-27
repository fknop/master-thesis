package village1

import oscar.cp._
import oscar.util._

object EmployeeScheduling2 extends CPModel with App {

  val problem = Problem.parse("data/problem.json")

  val T = problem.T
  val demands = problem.demands
  val workers = problem.workers
  val V = problem.vehicles
  val Z = problem.zones
  val W = workers.length
  val D = demands.length
  val Demands = 0 until D
  val Workers = 0 until W
  val Periods = 0 until T
  val Vehicles = 0 until V
  val Zones = 0 until Z


  val vars = Array.tabulate(T)(t => {
    Array.tabulate(D)(d => {
      val demand = demands(d)
      if (demand.periods.contains(t)) {
        Array.tabulate(demand.workers)(_ => CPIntVar(0, W - 1))
      }
      else {
        Array.empty[CPIntVar]
      }
    })
  })


  val vehicleVars = Array.tabulate(T)(t => {
    Array.tabulate(D)(d => {
      val demand = demands(d)
      Array.tabulate(demand.vehicles)(_ => CPIntVar(0, V - 1))
    })
  })


  val zonesVar = Array.tabulate(T)(t => {
    Array.tabulate(D)(d => {
      val demand = demands(d)
      Array.tabulate(demand.zones.size)(_ => CPIntVar(0, Z - 1))
    })
  })


  // All workers for a given time must be different
  for (period <- Periods) {
    val workersForPeriod = vars(period).flatten
    add(allDifferent(workersForPeriod))
  }

  // All zones for a given time must be different
  for (period <- Periods) {
    val zonesForPeriod = zonesVar(period).flatten
    if (zonesForPeriod.length >= 2) {
      add(allDifferent(zonesForPeriod))
    }
  }

  // All workers must work in a time in which they are available
  for (worker <- Workers; period <- Periods) {
    val isAvailable = workers(worker).availabilities.contains(period)
    if (!isAvailable) {
      for (demand <- Demands) {
        for (i <- vars(period)(demand).indices) {
          val workerVar = vars(period)(demand)(i)
          add(workerVar !== worker)
        }
      }
    }
  }


  // All vehicles used for a given time must be different
  for (period <- Periods) {
    val vehiclesForPeriod = vehicleVars(period).flatten
    if (vehiclesForPeriod.length >= 2) {
      add(allDifferent(vehiclesForPeriod))
    }
  }


  // Worker incompatibilities

  // Incompatibilities i -> j and j -> i
  val incompatibilities = problem.workersIncompatibilities ++ problem.workersIncompatibilities.map(_.reverse)

  // Workers with incompatibilities cannot work together
  for (period <- Periods; demand <- Demands) {
    val demandVar = vars(period)(demand)
    if (demandVar.length >= 2) {
      val permutations = Util.generatePermutationsOfTwo(demandVar.length)
      for (permutation <- permutations) {
        val (i, j) = permutation
        add(negativeTable(Array(demandVar(i), demandVar(j)), incompatibilities))
//          add(negativeTable(Array(demandVar(j), demandVar(i)), problem.workersIncompatibilities))
      }
    }
  }



  search {
    val flatVars = vars.flatten.flatten
    val flatVehiclesVars = vehicleVars.flatten.flatten

    val variables = flatVars ++ flatVehiclesVars
    conflictOrderingSearch(variables, i => variables(i).size, i => variables(i).min)
  }


  var solFound = false
  onSolution {
    solFound = true
    println("sol found")

    for (period <- Periods) {
      for (demand <- Demands) {
        val workerVars = vars(period)(demand)
        if (!workerVars.isEmpty) {
          println(s"Period = $period, demand = $demand")
          println("Workers: " + vars(period)(demand).deep)

          val vehicles = vehicleVars(period)(demand)
          if (!vehicles.isEmpty) {
            println("Vehicles: " + vehicles.deep)
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


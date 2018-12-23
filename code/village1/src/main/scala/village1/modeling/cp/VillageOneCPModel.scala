package village1.modeling.cp

import oscar.cp._
import village1.format.json.JsonParser
import village1.util.Utilities


class VillageOneCPModel(path: String) extends CPModel {

  val problem = JsonParser.parse(path)

  val T = problem.T
  val demands = problem.demands
  val workers = problem.workers
//  val V = problem.vehicles
  val L = problem.locations
  val W = problem.workers.length
  val D = problem.demands.length
  val Demands = 0 until D
  val Workers = 0 until W
  val Periods = 0 until T
//  val Vehicles = 0 until V
  val Locations = 0 until L

  val EMPTY_INT_VAR_ARRAY = Array.empty[CPIntVar]


  val workerVars = Array.tabulate(T, D)((t, d) => {
    val demand = demands(d)
    if (demand.hasPeriod(t)) {
      Array.tabulate(demand.workers)(_ => CPIntVar(0, W - 1))
    }
    else {
      EMPTY_INT_VAR_ARRAY
    }
  })

//
//  val vehicleVars = Array.tabulate(T, D)((_, d) => {
//    val demand = demands(d)
//    Array.tabulate(demand.vehicles)(_ => CPIntVar(0, V - 1))
//  })


  val locationVars = Array.tabulate(T, D)((_, d) => {
    if (demands(d).locations.isEmpty) null
    else CPIntVar(0, L - 1)
  })


  // All workers for a given time must be different
  for (period <- Periods) {
    val workersForPeriod = workerVars(period).flatten
    add(allDifferent(workersForPeriod))
  }

  // All zones for a given time must be different
  for (period <- Periods) {
    val zonesForPeriod = locationVars(period).filter(_ != null)
    if (zonesForPeriod.length >= 2) {
      add(allDifferent(zonesForPeriod))
    }
  }

  // All workers must work in a time in which they are available
  for (worker <- Workers; period <- Periods) {
    val isAvailable = workers(worker).available(period)
    if (!isAvailable) {
      for (demand <- Demands) {
        for (i <- workerVars(period)(demand).indices) {
          val workerVar = workerVars(period)(demand)(i)
          add(workerVar !== worker)
        }
      }
    }
  }


  // All vehicles used for a given time must be different
//  for (period <- Periods) {
//    val vehiclesForPeriod = vehicleVars(period).flatten
//    if (vehiclesForPeriod.length >= 2) {
//      add(allDifferent(vehiclesForPeriod))
//    }
//  }

  // All zones in a demand must be a possible zone for that demand
  for (t <- Periods; d <- Demands) {
    val demand = demands(d)
    val locations = demand.locations

    if (locationVars(t)(d) != null) {
      for (z <- Locations) {
        if (!locations.contains(z)) {
          add(locationVars(t)(d) !== z)
        }
      }
    }
  }



  // Worker incompatibilities

  // Incompatibilities i -> j and j -> i
  val wwIncompatibilities = problem.workerWorkerIncompatibilities ++ problem.workerWorkerIncompatibilities.map(_.reverse)

  // Workers with incompatibilities cannot work together
  for (period <- Periods; demand <- Demands) {
    val demandVar = workerVars(period)(demand)
    if (demandVar.length >= 2) {
      val permutations = Utilities.generatePermutationsOfTwo(demandVar.length)
      for (permutation <- permutations) {
        val (i, j) = permutation
        add(negativeTable(Array(demandVar(i), demandVar(j)), wwIncompatibilities))
//          add(negativeTable(Array(demandVar(j), demandVar(i)), problem.workersIncompatibilities))
      }
    }
  }

  val wcIncompatibilities = problem.workerClientIncompatibilities

  // For each incompatibility between a client and a worker
  // Remove the worker from the demands where the worker cannot work with that client
  for (incompatibility <- wcIncompatibilities) {
    val workerId = incompatibility(0)
    val client = incompatibility(1)
    for (d <- Demands) {
      val demand = demands(d)
      if (demand.client == client) {
        for (period <- demand.periods) {
          val demandWorkers: Array[CPIntVar] = workerVars(period)(demand)
          for (worker <- demandWorkers) {
            add(worker !== workerId)
          }
        }
      }
    }
  }

  // Demands should have workers with required skills
  for (d <- Demands) {
    val demand = demands(d)
    for (w <- 0 until demand.workers) {
      val requirements = demand.worker(w)
      val skills = requirements.skills

      if (!skills.isEmpty) {
        for (worker <- workers.indices) {
          // If worker doesn't have required skills
          if (!workers(worker).satisfySkills(skills)) {
            for (t <- Periods if demand.hasPeriod(t)) {
              add(workerVars(t)(d)(w) !== worker)
            }
          }
        }
      }

    }
  }

}

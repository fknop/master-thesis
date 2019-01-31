package village1.modeling.cp

import oscar.cp._
import village1.format.json.JsonParser
import village1.modeling.UnsolvableException
import village1.util.Utilities


class VillageOneCPModel(path: String) extends CPModel {

  val problem = JsonParser.parse(path)

  val T = problem.T
  val demands = problem.demands
  val workers = problem.workers
  val M = problem.machines.size
  val L = problem.locations
  val W = problem.workers.length
  val D = problem.demands.length
  val Demands = 0 until D
  val Workers = 0 until W
  val Periods = 0 until T
  val Machines = 0 until M
  val Locations = 0 until L

  val EMPTY_INT_VAR_ARRAY = Array.empty[CPIntVar]


  val workerVariables = generateWorkerVariables()
  val locationVariables = generateLocationVariables()

  applyAllDifferentWorkers()
  applyAvailableWorkers()

  applyWorkerWorkerIncompatibilities()
  applyWorkerClientIncompatibilities()

  applyRequiredSkills()

  applyAllDifferentZones()



  // Methods definitions


  def generateWorkerVariables (): Array[Array[Array[CPIntVar]]] = {
    Array.tabulate(T, D)((t, d) => {
      val demand = demands(d)
      if (demand.hasPeriod(t)) {
        Array.tabulate(demand.workers)(_ => CPIntVar(0, W - 1))
      }
      else {
        EMPTY_INT_VAR_ARRAY
      }
    })
  }

  def generateLocationVariables (): Array[Array[CPIntVar]] = {
    Array.tabulate(T, D)((_, d) => {
      if (demands(d).locations.isEmpty) null
      // Filter locations that might be out of range
      // TODO: throw a warning/error if location is out of range.
      else CPIntVar(demands(d).locations.filter(l => 0 <= l && l <= L - 1))
    })
  }

//  def generateMachineVariables (): Array[Array[Array[Any]]] = {
//    Array.tabulate(T, D)((_, d) => {
//      val demand = demands(d)
//      Array.tabulate(/*demand.vehicles*/ 0)(_ => CPIntVar(0, /*V - */1))
//    })
//  }

  // All workers for a given time must be different
  def applyAllDifferentWorkers (): Unit = {
    for (period <- Periods) {
      val workersForPeriod = workerVariables(period).flatten

      if (!workersForPeriod.isEmpty) {
        add(allDifferent(workersForPeriod))
      }
    }
  }

  // All zones for a given time must be different
  def applyAllDifferentZones (): Unit = {
    for (period <- Periods) {
      val zonesForPeriod = locationVariables(period).filter(_ != null)
      if (zonesForPeriod.length >= 2) {
        add(allDifferent(zonesForPeriod))
      }
    }
  }

  // All workers must work in a time in which they are available
  def applyAvailableWorkers (): Unit = {
    for (worker <- Workers; period <- Periods) {
      val isAvailable = workers(worker).available(period)
      if (!isAvailable) {
        for (demand <- Demands) {
          for (i <- workerVariables(period)(demand).indices) {
            val workerVar = workerVariables(period)(demand)(i)
            add(workerVar !== worker)
          }
        }
      }
    }
  }

  def applyWorkerWorkerIncompatibilities(): Unit = {
    val wwIncompatibilities = problem.workerWorkerIncompatibilities ++ problem.workerWorkerIncompatibilities.map(_.reverse)

    // Workers with incompatibilities cannot work together
    for (period <- Periods; demand <- Demands) {
      val demandVar = workerVariables(period)(demand)
      if (demandVar.length >= 2) {
        val permutations = Utilities.generatePermutationsOfTwo(demandVar.length)
        for (permutation <- permutations) {
          val (i, j) = permutation
          add(negativeTable(Array(demandVar(i), demandVar(j)), wwIncompatibilities))
          //          add(negativeTable(Array(demandVar(j), demandVar(i)), problem.workersIncompatibilities))
        }
      }
    }
  }

  def applyWorkerClientIncompatibilities (): Unit = {
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
            val demandWorkers: Array[CPIntVar] = workerVariables(period)(d)
            for (worker <- demandWorkers) {
              add(worker !== workerId)
            }
          }
        }
      }
    }
  }

  // Demands should have workers with required skills
  def applyRequiredSkills (): Unit = {
    for (d <- Demands) {
      val demand = demands(d)
      for (w <- 0 until demand.workers) {
        val requirements = demand.worker(w)
        val skills = requirements.skills

        if (skills.nonEmpty) {
          for (worker <- workers.indices) {
            // If worker doesn't have required skills
            if (!workers(worker).satisfySkills(skills)) {
              for (t <- Periods if demand.hasPeriod(t)) {
                add(workerVariables(t)(d)(w) !== worker)
              }
            }
          }
        }
      }
    }
  }
}

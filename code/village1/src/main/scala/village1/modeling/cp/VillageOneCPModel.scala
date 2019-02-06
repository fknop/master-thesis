package village1.modeling.cp

import oscar.cp._
import oscar.cp.constraints.AtMostNValue
import oscar.cp.core.CPPropagStrength
import village1.modeling.Problem
import village1.util.Utilities


class VillageOneCPModel(problem: Problem) extends CPModel {

  type WorkerVariables = Array[Array[Array[CPIntVar]]]

  // We make a guess (for now) that machines and locations will be the same for the entire duration of the demand
  // and that overlapping demands will not be able to have the same locations.
  type MachineVariables = Array[Array[CPIntVar]]
  type LocationVariables = Array[CPIntVar]

  val T = problem.T
  val demands = problem.demands
  val workers = problem.workers
  val M = problem.machines.length
  val L = problem.locations.length
  val W = problem.workers.length
  val D = problem.demands.length
  val Demands = 0 until D
  val Workers = 0 until W
  val Periods = 0 until T
  val Machines = 0 until M
  val Locations = 0 until L

  val EMPTY_INT_VAR_ARRAY = Array.empty[CPIntVar]

  private[this] val overlappingSets = Utilities.overlappingSets(problem.demands)


  val possibleMachines = precomputeMachineNeeds()
  val workerVariables: WorkerVariables = generateWorkerVariables()
  val machineVariables: MachineVariables = generateMachineVariables()
  val locationVariables: LocationVariables = generateLocationVariables()

  //val sameWorkerViolations = Array.tabulate(D)(d => CPIntVar(0 until demands(d).periods.size))
  val sameWorkerViolations = Array.tabulate(D)(d => CPIntVar(1 to demands(d).periods.size))


  // Workers constraints
  applyAvailableWorkers()
  applyAllDifferentWorkers()
  applyWorkerWorkerIncompatibilities()
  applyWorkerClientIncompatibilities()
  applyRequiredSkills()

  //applyNameTODO()


  // Locations constraints
  applyAllDifferentLocations()

  // Machine constraints
  applyAllDifferentMachines()


  // Precompute

  /**
    * For each machine needed, setup a list of possible machines for that need
    */
  def precomputeMachineNeeds (): Map[String, Set[Int]] = {
    var map = Map[String, Set[Int]]()
    val machines = problem.machines
    for (d <- Demands if demands(d).machineNeeds.nonEmpty) {
      for (m <- demands(d).machineNeeds if !map.contains(m.name)) {
        var set = Set[Int]()
        for (i <- machines.indices if m.name == machines(i).name) {
          set += i
        }

        map = map.updated(m.name, set)
      }
    }

    map
  }


  // Methods definitions


  def generateWorkerVariables (): WorkerVariables = {
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

  def generateLocationVariables (): LocationVariables = {
    Array.tabulate(D)(d => {
      if (demands(d).possibleLocations.isEmpty) null
      // Filter locations that might be out of range
      // TODO: throw a warning/error if location is out of range.
      else CPIntVar(demands(d).possibleLocations.filter(l => 0 <= l && l < L))
    })
  }

  def generateMachineVariables (): MachineVariables = {
    Array.tabulate(D)(d => {
      val demand = demands(d)
      Array.tabulate(demand.machineNeeds.length)(m => {
        val possibleValues = possibleMachines(demand.machineNeeds(m).name)
        CPIntVar(possibleValues)
      })
    })
  }

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
  def applyAllDifferentLocations(): Unit = {
    for (d <- Demands if locationVariables(d) != null) {
      val overlappingDemands = overlappingSets(d)
      val locations = (overlappingDemands + d).map(locationVariables(_)).filter(_ != null)
      add(allDifferent(locations))
    }


    /**
    for (period <- Periods) {
      val zonesForPeriod = locationVariables(period).filter(_ != null)
      if (zonesForPeriod.length >= 2) {
        add(allDifferent(zonesForPeriod))
      }
    }
    **/
  }


  def applyAllDifferentMachines(): Unit = {
    for (d <- Demands) {
      val overlappingDemands = overlappingSets(d)
      val machines = (overlappingDemands + d).flatMap(machineVariables(_))
      if (machines.nonEmpty) {
        add(allDifferent(machines))
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
        for ((i, j) <- permutations) {
          add(negativeTable(Array(demandVar(i), demandVar(j)), wwIncompatibilities))
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

  /**
    * A worker should work on the same demand as time goes on
    * This is a soft constraint, it can be violated but need to be maximized.
    *
    * TODO: implementation (check if this is the best way to do this)
    */
  def applyNameTODO (): Unit = {

//    maximize(sum(sameWorkerViolations))
    minimize(sum(sameWorkerViolations))

    for (d <- Demands) {
      val demand = demands(d)
      for (w <- 0 until demand.requiredWorkers) {
        val workersForDemand = demand.periods.map(t => workerVariables(t)(d)(w)).toArray
        if (workersForDemand.length > 1) {
          //add(softAllDifferent(workersForDemand, sameWorkerViolations(d)), CPPropagStrength.Strong)
          add(new AtMostNValue(workersForDemand, sameWorkerViolations(d)))
        }
      }
    }
  }


}

package village1.modeling.cp

import village1.data.{Demand, Worker}
import village1.modeling.Problem

/**
  * Common precomputed data for CP model(s)
  * @param problem the problem
  */
class CPPrecomputedData(problem: Problem) {

  val T: Int = problem.T
  val demands: Array[Demand] = problem.demands
  val workers: Array[Worker] = problem.workers
  val M: Int = problem.machines.length
  val L: Int = problem.locations.length
  val W: Int = problem.workers.length
  val D: Int = problem.demands.length
  val Demands: Range = 0 until D
  val Workers: Range = 0 until W
  val Periods: Range = 0 until T
  val Machines: Range = 0 until M
  val Locations: Range = 0 until L

  val allWorkers: Set[Int] = workers.indices.toSet
  val workersAvailabilities: Map[Int, Set[Int]] = precomputeWorkersAvailabilities()
  val availableWorkers: Map[Int, Map[Int, Set[Int]]] = precomputeAvailableWorkers()
  val workersWithSkills: Map[String, Set[Int]] = precomputeWorkersWithSkills()

  val possibleWorkersForDemands = precomputePossibleWorkersForDemand()

  val possibleMachines: Map[String, Set[Int]] = precomputeMachineNeeds()



  /**
    * Returns a map:
    *   key: time period
    *   value: set of workers available at that time
    * @return Set of available workers for each time period
    */
  private[this] def precomputeWorkersAvailabilities (): Map[Int, Set[Int]] = {
    var map = Map[Int, Set[Int]]()

    for (w <- Workers) {
      val periods = workers(w).availabilities
      for (t <- periods) {
        val set = if (map.contains(t)) map(t) else Set[Int]()
        map = map.updated(t, set + w)
      }
    }

    map
  }

  /**
    * Returns a map:
    *   key: index of demand
    *   value: set of workers available for that demand for each time period
    * @return Set of available workers for each demand
    */
  private[this] def precomputeAvailableWorkers (): Map[Int, Map[Int, Set[Int]]] = {
    var map =  Map[Int, Map[Int, Set[Int]]]()

    for (d <- Demands) {
      val periods = demands(d).periods

      val workers= periods.foldLeft(Map[Int, Set[Int]]()) { (acc, t) =>
        acc.updated(t, workersAvailabilities(t))
      }

      map = map.updated(d, workers)
    }

    map
  }

  /**
    * Returns a map with:
    *   key: demand index
    *   value: map with:
    *     key: time period
    *     value: array of Set[Int] (array of sets of workers)
    */
  private[this] def precomputePossibleWorkersForDemand (): Map[Int, Map[Int, Array[Set[Int]]]] = {
    Demands.foldLeft(Map[Int, Map[Int, Array[Set[Int]]]]()) { (precomputed, d) =>

      val demand = demands(d)

      val initialPossibleWorkers = Array.tabulate(demand.requiredWorkers) { position =>
        val requirements = demand.worker(position)
        val skills = requirements.skills

        if (skills.nonEmpty) {
          // Possible workers that fit the skills of worker w
          skills.foldLeft(allWorkers) {
            (acc, skill) => acc.intersect(workersWithSkills(skill.name))
          }.filter(w => workers(w).satisfySkills(skills))
        }
        else {
          null
        }
      }


      precomputed.updated(
        d,
        demand.periods.foldLeft(Map[Int, Array[Set[Int]]]()) {
          (acc, t) => {
            val positions = Array.tabulate(demand.requiredWorkers) { i =>
              val possibleWorkers =
                if (initialPossibleWorkers(i) == null)
                  availableWorkers(d)(t)
                else
                  initialPossibleWorkers(i).intersect(availableWorkers(d)(t))

              possibleWorkers
            }

            acc.updated(t, positions)
          }
        }
      )
    }
  }


  private[this] def precomputeMachineNeeds (): Map[String, Set[Int]] = {
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

  /**
    * Precompute workers that have a certain skill, without taking into account the values for now
    */
  private[this] def precomputeWorkersWithSkills (): Map[String, Set[Int]] = {
    var map = Map[String, Set[Int]]()
    for (w <- workers.indices) {
      val worker = workers(w)
      for ((name, _) <- worker.skills) {
        val set = if (map.contains(name)) map(name) else Set[Int]()
        map = map.updated(name, set + w)
      }
    }

    map
  }
}
package village1.modeling.cp

import village1.data.{Demand, Worker}
import village1.modeling.Problem
import village1.util.Utilities

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

  val workersAvailabilities: Map[Int, Set[Int]] = precomputeWorkersAvailabilities()
  val availableWorkers: Map[Int, Map[Int, Set[Int]]] = precomputeAvailableWorkers()
  val workersWithSkills: Map[String, Set[Int]] = precomputeWorkersWithSkills()



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
  private[this] def precomputePossibleWorkersForDemand (): Map[Int, Map[Int, Set[Int]]] = {
    var map = Map[Int, Map[Int, Set[Int]]]()

    for (d <- Demands) {
      val demand = demands(d)
      for (w <- 0 until demand.requiredWorkers) {
        val requirements = demand.worker(w)
        val skills = requirements.skills

        if (skills.nonEmpty) {

          // Possible workers that fit the skills of worker w
          val possibleWorkers: Set[Int] = skills.foldLeft(Set[Int]()) {
            (acc, skill) => acc.intersect(workersWithSkills(skill.name))
          }.filter(w => workers(w).satisfySkills(skills))

          // For each time period, compute intersection of available workers and workers with required skills
          Periods.foldLeft(Map[Int, Set[Int]]()) {
            (acc, t) => acc.updated(t, availableWorkers(d)(t).intersect(possibleWorkers))
          }

        }
      }
    }
  }
    **/

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

package village1.generator

import village1.data.{Client, Demand, Worker}
import village1.format.json.JsonSerializer
import village1.modeling.Problem

object InstanceGenerator extends App {
  val random = new scala.util.Random

  def rand(start: Int, end: Int): Int = start + random.nextInt((end - start) + 1)

  def isTrue(probability: Double = 0.5): Boolean = math.random() < probability

  def generateWorkforce(n: Int): IndexedSeq[Worker] = {
    for (i <- 0 until n) yield Worker(id = i, name = s"Worker $i", availabilities = Set())
  }

  def generateDemands(n: Int, maxWorkers: Int): IndexedSeq[Demand] = {
    for (i <- 0 until n)
      yield Demand(id = i, client = i, periods = Set(), requiredWorkers = rand(1, maxWorkers))
  }

  def updatePeriods(demand: Demand, periods: Set[Int]): Demand = {
    Demand(demand.id, demand.client, periods, demand.requiredWorkers, demand.machines, demand.locations, demand.requiredSkills)
  }

  def generatePeriodForDemand(demand: Demand, t: Int): Demand = {
    val periods = (0 until t).foldLeft(Set[Int]()) {
      // Second condition avoid empty periods
      (acc, i) => if (isTrue(0.6) || (acc.isEmpty && i == t - 1)) acc + i else acc
    }

    updatePeriods(demand, periods)
  }

  def generateWorkersAvailabilities(workforce: IndexedSeq[Worker], demands: IndexedSeq[Demand], T: Int): IndexedSeq[Worker] = {
    var workers = workforce
    for (t <- 0 until T) {
      for (d <- demands.indices) {
        if (demands(d).periods.contains(t)) {
          // TODO: make this cleaner and more efficient
          workers = scala.util.Random.shuffle(workers)
          var done = 0
          var i = 0
          while (i < workers.size && done < demands(d).requiredWorkers) {
            if (!workers(i).available(t)) {
              workers = workers.updated(i, Worker.addAvailability(workers(i), t))
              done += 1
            }
            i += 1
          }
        }
      }
    }

    workers.sortBy(_.id)
  }

  // TODO: generate different clients
  def generate(t: Int, d: Int, w: Int): Unit = {
    val demands = generateDemands(d, Math.min(d, Math.max(w / d, 1)))
        .map(d => generatePeriodForDemand(d, t))

    val workforce = generateWorkforce(w)

    val workers = generateWorkersAvailabilities(workforce, demands, t)


    // Add a random number at the end of the file name to be able to create enough files with the same settings

    val problem = Problem(
      T = t,
      workers = workers.toArray,
      demands = demands.toArray,
      clients = demands.map(d => Client(name = s"Client ${d.client}")).toArray
    )

    val fileName = s"instance-t=$t-d=$d-w=$w-${rand(0, 1000)}"

    JsonSerializer.serialize(problem)(s"data/instances/generated/$fileName.json")
  }


  generate(10, 30, 400)
}

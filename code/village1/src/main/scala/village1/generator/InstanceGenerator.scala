package village1.generator

import village1.data._
import village1.json.JsonSerializer
import village1.modeling.Problem
import village1.util.Utilities.{rand, overlappingSets}

object InstanceGenerator {

  private def isTrue(probability: Double = 0.5): Boolean = math.random() < probability

  private def generateWorkforce(options: InstanceOptions): Array[Worker] = {
    Array.tabulate(options.workers)(i => Worker(id = i, name = s"Worker $i", availabilities = Set()))
  }

  private def takeSkill(skills: Array[Skill]): Skill = {
    skills(rand(0, skills.length - 1))
  }

  private def generateDemands(
      options: InstanceOptions,
      maxWorkers: Int,
      skills: Array[Skill] = Array(),
      skillProbability: Double = 0
  ): Array[Demand] = {

    val n = options.demands
    val clients = options.clients

    val demands = Array.fill[Demand](n)(null)

    for (i <- 0 until n) {
      val requiredWorkers = rand(1, maxWorkers)
      var requiredSkills = Array[Array[Skill]]()

      for (_ <- 0 until requiredWorkers) {
        if (isTrue(skillProbability)) {
          val skill = takeSkill(skills)
          requiredSkills :+= Array(skill)
        }
      }

      demands(i) = Demand(id = i, client = rand(0, clients - 1), periods = Set(), requiredWorkers = requiredWorkers, requiredSkills = requiredSkills)
    }

    demands
  }

  private def generatePeriodForDemand(demand: Demand, t: Int, prob: Double): Demand = {
    val periods = (0 until t).foldLeft(Set[Int]()) {
      // Second condition avoid empty periods
      (acc, i) => if (isTrue(prob) || (acc.isEmpty && i == t - 1)) acc + i else acc
    }

    demand.copy(periods = periods)
  }


  // TODO: make this more efficient and cleaner
  private def generateWorkersAvailabilities(workers: Array[Worker], demands: IndexedSeq[Demand], T: Int, skills: Array[Skill]): Array[Worker] = {
    for (d <- demands.indices) {
      for (t <- demands(d).periods) {
        var done = 0
        var i = rand(0, workers.length - 1)
        while (done < demands(d).requiredWorkers) {

          // Trick to avoid infinite loop for now
          val allAvailable = workers.forall(_.available(t))
          if (allAvailable) {
            done += 1
          }

          // This assure to be solvable
          if (!workers(i).available(t)) {
            workers(i) = workers(i).copy(availabilities = workers(i).availabilities + t)

            val requirements = demands(d).worker(done).skills
            val skills = requirements.foldLeft(workers(i).skills) {
              (current, skill) => current.updated(skill.name, skill)
            }

            workers(i) = workers(i).copy(skills = skills)

            done += 1
          }
          i = rand(0, workers.length - 1)
        }
      }

      // Add all availabilities to random workers (might be workers who already are available)
      var done = 0
      var i = rand(0, workers.length - 1)
      while (done < demands(d).requiredWorkers) {
        workers(i) = workers(i).copy(availabilities = workers(i).availabilities ++ demands(d).periods)
        done += 1
        i = rand(0, workers.length - 1)
      }

      // Add random skills to particular workers with random probability
      for (i <- workers.indices) {
        if (isTrue(0.2)) {
          val skill = takeSkill(skills)
          workers(i) = workers(i).copy(skills = workers(i).skills.updated(skill.name, skill))
        }
      }

    }

    workers
  }

  private def assignLocations(demands: Array[Demand], locations: Array[Location]): Unit = {
    val overlapping = overlappingSets(demands)

    val locationsIndices = locations.indices.toList

    var i = demands.length - 1
    while (i >= 0) {

      if (isTrue(0.5)) {
        val size = overlapping(i).size
        if (size <= locations.length) {
          val shuffled = util.Random.shuffle(locationsIndices)
          val possibleLocations = shuffled.take(size).toSet
          demands(i) = demands(i).copy(possibleLocations = possibleLocations)
        }
      }

      i -= 1
    }
  }

  private def assignMachines(demands: Array[Demand], machines: Array[Machine]): Unit = {
    val overlapping = overlappingSets(demands)
    val machineIndices = machines.indices.toList

    var i = demands.length - 1
    while (i >= 0) {

      if (isTrue(0.3)) {
        val names: Set[String] = overlapping(i).flatMap(demands(_).machineNeeds).map(_.name)
        val needs = machines.filter(m => !names.contains(m.name) && isTrue(0.2))
        if (needs.nonEmpty) {
          demands(i) = demands(i).copy(machineNeeds = needs)
        }
      }

      i -= 1
    }

  }

  // Only generate simple skills for now (does not change much in the solver)
  private def generateSkills(options: InstanceOptions) = Array.tabulate(options.skills)(i => Skill(name = s"Skill $i"))
  private def generateClients(options: InstanceOptions) = Array.tabulate(options.clients)(i => Client(name = s"Client $i"))

  private def generateLocations(options: InstanceOptions) = Array.tabulate(options.locations)(i => Location(s"Location $i"))
  private def generateMachines(options: InstanceOptions) = {
    val different = if (options.machines == 0) 0 else rand(options.machines / 2, options.machines - 1) // Allow to have some duplication (same machines)
    Array.tabulate(options.machines)(i => Machine(s"Machine ${rand(0, different)}"))
  }

  def generate(options: InstanceOptions, prob: Map[String, Double] = Map()): Problem = {

    val clients = generateClients(options)
    val skills = generateSkills(options)
    val locations = generateLocations(options)
    val machines = generateMachines(options)
    val demands = generateDemands(
        options,
        maxWorkers = Math.min(options.demands, Math.max(options.workers / options.demands, 1)),
        skills = skills,
        skillProbability = prob.getOrElse("skill", 0.2)
      )
      .map(d => generatePeriodForDemand(d, options.t, prob = prob.getOrElse("period", 0.5)))


    assignLocations(demands, locations)
    assignMachines(demands, machines)

    val workforce = generateWorkforce(options)
    val workers = generateWorkersAvailabilities(workforce, demands, options.t, skills)


    Problem(
      T = options.t,
      workers = workers,
      demands = demands,
      clients = clients,
      locations = locations,
      machines = machines
    )
  }
}

object InstanceGeneratorApp extends App {

  val t = 10
  val c = 30
  val d = 50
  val w = 300
  val s = 20
  val prob = Map("skill" -> 0.2, "period" -> 0.6)

  val problem = InstanceGenerator.generate(
    InstanceOptions(
      t = t,
      clients = c,
      demands = d,
      workers = w,
      skills = s,
      machines = 10,
      locations = 10
    ),
    prob = prob
  )

  val fileName = s"t${t}d${d}w${w}-${rand(0, 1000)}"
  val folder = "data/instances/generated/"
  JsonSerializer.serialize(problem)(s"$folder/$fileName.json")
}

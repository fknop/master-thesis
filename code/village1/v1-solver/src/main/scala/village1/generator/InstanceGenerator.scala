package village1.generator

import village1.data._
import village1.modeling.Problem
import village1.util.Utils.{overlappingSets, randomInt}

import scala.util.Random

class InstanceGenerator(seed: Long = 0L) {

  private val random: Random = new Random(seed)

  private def isTrue(probability: Double = 0.5): Boolean = random.nextDouble() < probability

  private def generateWorkforce(options: InstanceOptions): Array[Worker] = {
    Array.tabulate(options.workers)(i => Worker(id = i, name = s"Worker $i", availabilities = Set()))
  }

  private def takeSkill(skills: Array[Skill]): Skill = {
    skills(randomInt(random, 0, skills.length - 1))
  }


  private def takeSkills(skills: Array[Skill], n: Int): Array[Skill] = {
    random.shuffle(skills.toList).take(n).toArray
  }

  private def generateDemands(
      options: InstanceOptions,
      maxWorkers: Int,
      skills: Array[Skill] = Array()
  ): Array[Demand] = {

    val n = options.demands
    val clients = options.clients

    val demands = Array.fill[Demand](n)(null)

    for (i <- 0 until n) {
      val requiredWorkers = randomInt(random, 1, maxWorkers)
      var requiredSkills = Array[Array[Skill]]()

      for (_ <- 0 until requiredWorkers) {
        if (options.skills > 0 && isTrue(options.probabilities.getOrElse("assignSkill", 0.5))) {
          val n = randomInt(random, 0, math.min(skills.length, 3))
          val s = takeSkills(skills, n)
          requiredSkills :+= s
        }
      }

      demands(i) = Demand(id = i, client = randomInt(random, 0, clients - 1), periods = Set(), requiredWorkers = requiredWorkers, requiredSkills = requiredSkills)
    }

    demands
  }

  private def generatePeriodForDemand(options: InstanceOptions, demand: Demand, t: Int): Demand = {
    val prob = options.probabilities.getOrElse("assignPeriod", 0.5)
    val periods = (0 until t).foldLeft(Set[Int]()) {
      // Second condition avoid empty periods
      (acc, i) => if (isTrue(prob) || (acc.isEmpty && i == t - 1)) acc + i else acc
    }

    demand.copy(periods = periods)
  }


  // TODO: make this more efficient and cleaner
  private def generateWorkersAvailabilities(options: InstanceOptions, workers: Array[Worker], demands: IndexedSeq[Demand], skills: Array[Skill]): Array[Worker] = {
    val T = options.t
    for (d <- demands.indices) {
      for (t <- demands(d).periods) {
        var done = 0
        var i = randomInt(random, 0, workers.length - 1)
        while (done < demands(d).requiredWorkers) {

          // Trick to avoid infinite loop for now
          val allAvailable = workers.forall(_.available(t))
          if (allAvailable) {
            done += 1
          }

          // This assure to be solvable
          if (!workers(i).available(t)) {
            workers(i) = workers(i).copy(availabilities = workers(i).availabilities + t)

            val requirements = demands(d).requirements(done).skills
            val skills = requirements.foldLeft(workers(i).skills) {
              (current, skill) => current.updated(skill.name, skill)
            }

            workers(i) = workers(i).copy(skills = skills)

            done += 1
          }
          i = randomInt(random, 0, workers.length - 1)
        }
      }

      // Add all availabilities to random workers (might be workers who already are available)
      var done = 0
      var i = randomInt(random, 0, workers.length - 1)
      while (done < demands(d).requiredWorkers) {
        workers(i) = workers(i).copy(availabilities = workers(i).availabilities ++ demands(d).periods)
        done += 1
        i = randomInt(random, 0, workers.length - 1)
      }


      val assignWorkerSkill = options.probabilities.getOrElse("assignWorkerSkill", 0.2)

      // Add random skills to particular workers with random probability
      for (i <- workers.indices) {
        if (options.skills > 0 && isTrue(assignWorkerSkill)) {
          val skill = takeSkill(skills)
          workers(i) = workers(i).copy(skills = workers(i).skills.updated(skill.name, skill))
        }
      }

    }

    // In the case workers has no availabilities
    for (i <- workers.indices) {
      val availabilities = workers(i).availabilities
      if (availabilities.isEmpty) {
        val size = randomInt(random, 1, options.t)
        var set = Set[Int]()
        for (_ <- 0 until size) {
          set += randomInt(random, 0, options.t - 1)
        }

        workers(i) = workers(i).copy(availabilities = set)
      }
    }

    workers
  }

  private def assignLocations(options: InstanceOptions, demands: Array[Demand], locations: Array[Location]): Unit = {
    val overlapping = overlappingSets(demands)

    val locationsIndices = locations.indices.toList

    var i = demands.length - 1
    val prob = options.probabilities.getOrElse("assignLocation", 0.5)
    while (i >= 0) {
      if (isTrue(prob)) {
        val size = overlapping(i).size
        if (size <= locations.length) {
          val shuffled = random.shuffle(locationsIndices)
          val possibleLocations = shuffled.take(size).toSet
          demands(i) = demands(i).copy(possibleLocations = possibleLocations)
        }
      }

      i -= 1
    }
  }

  private def assignMachines(options: InstanceOptions, demands: Array[Demand], machines: Array[Machine]): Unit = {
    val overlapping = overlappingSets(demands)

    var i = demands.length - 1
    val assignMachines = options.probabilities.getOrElse("assignMachines", 0.3)
    val takeMachine = options.probabilities.getOrElse("takeMachine", 0.2)

    while (i >= 0) {

      if (isTrue(assignMachines)) {
        val names: Set[String] = overlapping(i).flatMap(demands(_).machineNeeds).map(_.name)
        val needs = machines.filter(m => !names.contains(m.name) && isTrue(takeMachine))
        if (needs.nonEmpty) {
          demands(i) = demands(i).copy(machineNeeds = needs)
        }
      }

      i -= 1
    }

  }


  private def generateWorkingRequirements(options: InstanceOptions, workers: Array[Worker]): Array[WorkingRequirement] = {
    var requirements = List[WorkingRequirement]()
    val assignRequirement = options.probabilities.getOrElse("assignWorkingRequirements", 0.2)
    val assignBoth = 0.01
    val assignMin = 0.98

    for (w <- workers.indices if isTrue(assignRequirement)) {
      val availabilities = workers(w).availabilities
      val size = availabilities.size
      val prob = random.nextDouble()
      var min: Option[Int] = None
      var max: Option[Int]  = None
      if (prob <= assignBoth) {
        min = Some(randomInt(random, 1, math.max(1, size / 2)))
        max = Some(randomInt(random, min.get, size - 1))
      }
      else if (prob <= assignMin) {
        min = Some(randomInt(random, 1, math.max(1, size / 2)))
      }
      else {
        max = Some(randomInt(random, 1, size))
      }

      requirements = WorkingRequirement(w, min, max) :: requirements
    }

    requirements.toArray
  }

  // Only generate simple skills for now (does not change much in the solver)
  private def generateSkills(options: InstanceOptions) = Array.tabulate(options.skills)(i => Skill(name = s"Skill $i"))
  private def generateClients(options: InstanceOptions) = Array.tabulate(options.clients)(i => Client(name = s"Client $i"))

  private def generateLocations(options: InstanceOptions) = Array.tabulate(options.locations)(i => Location(s"Location $i"))
  private def generateMachines(options: InstanceOptions) = {
    val different = if (options.machines == 0) 0 else randomInt(random, options.machines / 2, options.machines - 1) // Allow to have some duplication (same machines)
    Array.tabulate(options.machines)(i => Machine(s"Machine ${randomInt(random, 0, different)}"))
  }

  def generate(options: InstanceOptions): Problem = {

    val clients = generateClients(options)
    val skills = generateSkills(options)
    val locations = generateLocations(options)
    val machines = generateMachines(options)
    val demands = generateDemands(
        options,
        maxWorkers = Math.min(options.demands, Math.max(options.workers / options.demands, 1)),
        skills = skills
      )
      .map(d => generatePeriodForDemand(options, d, options.t))


    assignLocations(options, demands, locations)
    assignMachines(options, demands, machines)

    val workforce = generateWorkforce(options)
    val workers = generateWorkersAvailabilities(options, workforce, demands, skills)

    val requirements = generateWorkingRequirements(options, workers)


    Problem(
      T = options.t,
      workers = workers,
      demands = demands,
      clients = clients,
      locations = locations,
      machines = machines,
      workingRequirements = requirements
    )
  }
}


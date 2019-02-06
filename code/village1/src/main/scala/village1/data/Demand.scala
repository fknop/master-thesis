package village1.data


case class Demand(
   id: Int,
   client: Int,
   periods: Set[Int],
   requiredWorkers: Int,
   machineNeeds: Array[Machine] = Array(),
   possibleLocations: Set[Int] = Set(),
   requiredSkills: IndexedSeq[IndexedSeq[Skill]] = IndexedSeq()
) {


  val workers: Int = requiredWorkers

  val requirements: IndexedSeq[WorkerRequirement] = (0 until workers).map(i => {
    if (i < requiredSkills.length) {
      WorkerRequirement(skills = requiredSkills(i))
    }
    else {
      WorkerRequirement(skills = IndexedSeq[Skill]())
    }
  })

  def hasPeriod (t: Int): Boolean = periods.contains(t)


  def overlapWith (demand: Demand): Boolean = {
    val (smaller, larger) =
      if (demand.periods.size < periods.size) (demand.periods, periods)
      else (periods, demand.periods)

    var overlap = false
    val iterator = smaller.iterator
    while (iterator.hasNext && !overlap) {
      overlap = larger.contains(iterator.next())
    }

    overlap
  }

  def worker (w: Int): WorkerRequirement = requirements(w)
}





package village1.data


case class Demand(
  id: Int,
  client: Int,
  periods: Set[Int],
  requiredWorkers: Int,
  machines: Set[Int] = Set(),
  locations: Set[Int] = Set(),
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

  def worker (w: Int): WorkerRequirement = requirements(w)
}



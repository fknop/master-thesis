package village1.data

import village1.data.Skill.SkillGroup

case class Demand(
  id: Int,
  periods: Set[Int],
  vehicles: Int = 0,
  zones: Set[Int] = Set(),
  workersRequirements: IndexedSeq[WorkerRequirement],
) {

  val workers: Int = workersRequirements.length

  def hasPeriod (t: Int): Boolean = periods.contains(t)

  def worker (w: Int): WorkerRequirement = workersRequirements(w)
}

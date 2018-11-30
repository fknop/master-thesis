package village1.data


case class Worker(
 id: Int,
 availabilities: Set[Int],
 skills: Map[String, Skill] = Map(),
 restrictions: Map[String, Restriction] = Map()
) {

  def hasSkill (name: String): Boolean = skills.contains(name)
  def hasRestriction (name: String): Boolean = restrictions.contains(name)

  // Suppose that hasSkill(name) is true
  def skill (name: String): Skill = skills(name)

  // Suppose that hasRestriction(name) is true
  def restriction (name: String): Restriction = restrictions(name)

  def available (t: Int): Boolean = availabilities.contains(t)
}


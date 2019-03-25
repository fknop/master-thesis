package village1.modeling.violations

import village1.data.Skill

case class AdditionalSkillViolation(demand: Int, time: Int, skill: Skill) extends Violation {
  override val `type`: String = "AdditionalSkillViolation"
  override val description: String = s"Skill $skill could not be assigned to demand $demand at time $time"
}

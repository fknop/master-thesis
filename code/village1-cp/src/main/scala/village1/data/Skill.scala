package village1.data

case class Skill(
  name: String,
  parameterType: ParameterType,
  value: Int
)


object Skill {
  type SkillGroup = Array[Skill]
}

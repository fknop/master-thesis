package village1.data

import ParameterType._

case class Skill(
  name: String, // Change to enum ?
  parameterType: ParameterType,
  value: Int
)

package village1

import village1.ParameterType._

case class Skill(
  name: String, // Change to enum ?
  parameterType: ParameterType,
  value: Int
)
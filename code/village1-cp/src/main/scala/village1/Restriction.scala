package village1


import ParameterType._


case class Restriction(
  name: String, // Change to enum ?
  parameterType: ParameterType,
  value: Int
)
package village1.data

sealed trait ParameterType

case object Min extends ParameterType
case object Max extends ParameterType
case object None extends ParameterType

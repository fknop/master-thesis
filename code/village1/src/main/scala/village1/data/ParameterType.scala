package village1.data

sealed trait ParameterType

object ParameterType {
  case object Min extends ParameterType
  case object Max extends ParameterType
  case object None extends ParameterType

  def from (t: String): ParameterType = {
    t match {
      case "Min" => ParameterType.Min
      case "Max" => ParameterType.Max
      case _ => ParameterType.None
    }
  }
}

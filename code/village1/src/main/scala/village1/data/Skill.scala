package village1.data

case class Skill(
  name: String,
  parameterType: ParameterType,
  value: Double = .0
) {


  def satisfy (skill: Skill): Boolean = {
    if (this.name != skill.name) false
    else parameterType match {
      case ParameterType.Min => satisfyMin(skill)
      case ParameterType.Max => satisfyMax(skill)
      case ParameterType.None => true
    }
  }

  private def satisfyMin (skill: Skill): Boolean = {
    skill.parameterType match {
      case ParameterType.Min => this.value <= skill.value // Both are min
      case ParameterType.Max => this.value <= skill.value // Min and Max
      case ParameterType.None => false
    }
  }

  private def satisfyMax (skill: Skill): Boolean = {
    skill.parameterType match {
      case ParameterType.Min => this.value >= skill.value // Max and min

      // Is it really >= ?
      case ParameterType.Max => this.value >= skill.value // Both are max
      case ParameterType.None => false
    }
  }


}



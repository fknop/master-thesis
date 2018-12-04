package village1.data

case class Skill(
  name: String,
  parameterType: ParameterType,
  value: Int
) {


  def satisfy (skill: Skill): Boolean = {
    if (this.name != skill.name) false
    else parameterType match {
      case Min => satisfyMin(skill)
      case Max => satisfyMax(skill)
      case None => true
    }
  }

  private def satisfyMin (skill: Skill): Boolean = {
    skill.parameterType match {
      case Min => this.value <= skill.value // Both are min
      case Max => this.value <= skill.value // Min and Max
      case None => false
    }
  }

  private def satisfyMax (skill: Skill): Boolean = {
    skill.parameterType match {
      case Min => this.value >= skill.value // Max and min

      // Is it really >= ?
      case Max => this.value >= skill.value // Both are max
      case None => false
    }
  }


}



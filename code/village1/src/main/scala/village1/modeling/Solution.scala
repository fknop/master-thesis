package village1.modeling

import village1.data.{DemandAssignment}

case class Solution(problem: Problem, plannings: List[DemandAssignment]) {

  // TODO: implement validation logic
  def valid: Boolean = {
    throw new NotImplementedError()
  }
}


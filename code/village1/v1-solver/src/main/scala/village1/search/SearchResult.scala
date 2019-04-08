package village1.search

import village1.modeling.Solution

/**
  * @param solution The last solution found in the search
  * @param time the running time of the solver
  */
case class SearchResult(solution: Option[Solution], time: Long)
package village1.search

import village1.modeling.Solution

/**
  * @param solution The last solution found in the search
  * @param time the running time of the solver
  * @param optimal true if solution is optimal, false means unknown
  */
case class SearchResult(solution: Option[Solution], time: Long, optimal: Boolean)
package village1.search

trait SearchOptions

trait Search[T <: SearchOptions] extends SolutionEmitter {
  def solve(timeLimit: Int = Int.MaxValue, solutionLimit: Int = Int.MaxValue, silent: Boolean = false, options: Option[T] = None): SearchResult
}

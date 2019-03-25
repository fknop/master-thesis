package village1.search

trait Search[T] extends SolutionEmitter {
  def solve(timeLimit: Int, solutionLimit: Int, silent: Boolean, options: Option[T] = None): SearchResult
}

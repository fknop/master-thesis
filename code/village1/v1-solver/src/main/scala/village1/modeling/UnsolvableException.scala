package village1.modeling

/**
  * This is an exception that is thrown on precompute / constraint creation
  * when a model is known to be unsolvable
  * @param message the message
  */
class UnsolvableException(message: String) extends Exception(message)

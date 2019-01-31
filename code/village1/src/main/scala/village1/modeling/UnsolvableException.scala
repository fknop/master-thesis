package village1.modeling

final case class UnsolvableException(private val message: String = "",
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)



package csw.sjreact.util


object OptionOps {
  def when[T](boolean: Boolean)(value: => T): Option[T]
  = if (boolean) Some(value) else None
}

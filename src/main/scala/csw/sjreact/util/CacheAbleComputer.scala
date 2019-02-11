package csw.sjreact.util

class CacheAbleComputer[T, R](fun: T => R) {
  private var cachedValue: Option[R] = None

  def cached: Option[R] = cachedValue

  def compute(v1: T): R = {
    val result = fun(v1)
    cachedValue = Some(result)
    result
  }

  def cachedOrCompute(v1: => T): R = cachedValue.getOrElse(compute(v1))
}

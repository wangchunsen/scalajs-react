package csw.sjreact.util

trait CachedComputer[R] {
  val computer: () => R
  private var cachedValue: Option[R] = None

  def value: R = {
    if (cachedValue.isDefined) cachedValue.get
    else {
      cachedValue = Some(computer())
      cachedValue.get
    }
  }

  def force: R = {
    cachedValue = Some(computer())
    cachedValue.get
  }

}


object CachedComputer {
  def apply[R](fun: () => R, init: Boolean = false): CachedComputer[R] = {
    val cached = new CachedComputer[R] {
      val computer: () => R = fun
    }
    if (init) {
      cached.force
    }
    cached
  }
}
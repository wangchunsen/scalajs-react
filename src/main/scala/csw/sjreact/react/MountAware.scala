package csw.sjreact.react

private[react] trait MountAware[A] {
  def didMount(native: Native.ReactComp[A]): Unit

  def willUnmount(native: Native.ReactComp[A]): Unit
}
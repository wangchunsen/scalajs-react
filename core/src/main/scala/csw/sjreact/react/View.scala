package csw.sjreact.react

import csw.sjreact.WatchScope
import csw.sjreact.dsl.VDom

trait View {
  def render: VDom.Element

  implicit protected val manager: WatchScope = new WatchScope {}

  private[react] def toRenderAble: RenderAbleImp = {
    val mountAware: MountAware[Unit] = new MountAware[Unit] {

      override def didMount(native: Native.ReactComp[Unit]): Unit = {
      }

      override def willUnmount(native: Native.ReactComp[Unit]): Unit = {
        manager.cancelAllWatching()
      }
    }
    val element = NativeBridge.createStaticElement(
      renderFn = () => render,
      componentName = Some("ViewRoot"),
      mountAware = Some(mountAware)
    )
    RenderAbleImp(element)
  }
}

package csw.sjreact.react

import csw.sjreact.dsl.VDom
import csw.sjreact.react.Native.ReactDOM
import csw.sjreact.util.CancelAble
import csw.sjreact.{State, StateMod, Data}
import org.scalajs.dom.Element


object Comp {
  def renderS[S](state: State[S], name: CompName= CompName.empty)
                (renderFn: (S, StateMod[S]) => VDom.Node): RenderAble = {
    val stateMode: StateMod[S] = newValue => state.set(newValue)
    render[S](state, name)(s => renderFn(s, stateMode))
  }


  def render[S](_var: Data[S], name: CompName = CompName.empty)
               (renderFn: S => VDom.Node): RenderAble = {
    val mountAware: MountAware[S] = new MountAware[S] {
      var cancelWatcher: Option[CancelAble] = None

      override def didMount(native: Native.ReactComp[S]): Unit = {
        cancelWatcher = Some(Data.watch(_var)(value => setStateValue(native, value)))
        setStateValue(native, _var.get)
      }

      override def willUnmount(native: Native.ReactComp[S]): Unit = {
        cancelWatcher.foreach(_.cancel())
      }
    }
    val element = NativeBridge.createElement(
      renderFn = renderFn,
      componentName = name.nameStr,
      mountAware = Some(mountAware)
    )
    RenderAbleImp(element)
  }

  def static(node: VDom.Node): RenderAble = RenderAbleImp(NativeBridge.node2Element(node))

  def renderIntoDom(renderAble: RenderAble, container: Element): Unit = renderAble match {
    case RenderAbleImp(element) => ReactDOM.render(element, container)
  }

  private def setStateValue[S](native: Native.ReactComp[S], value: S): Unit = {
    native.setState(Wrapper(value))
  }
}
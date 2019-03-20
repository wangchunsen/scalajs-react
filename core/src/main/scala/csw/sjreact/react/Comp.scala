package csw.sjreact.react

import csw.sjreact.dsl.VDom
import csw.sjreact.react.Native.ReactDOM
import csw.sjreact.{State, StateMod, Var, WatchScope}
import org.scalajs.dom.Element
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object Comp {
  def renderS[S](state: State[S], name: CompName = CompName.empty)
                (renderFn: (S, StateMod[S]) => VDom.Node): RenderAble = {
    val stateMode: StateMod[S] = newValue => state.set(newValue)
    render[S](state, name)(s => renderFn(s, stateMode))
  }


  def render[S](_var: Var[S], name: CompName = CompName.empty, debug: Boolean = false)
               (renderFn: S => VDom.Node): RenderAble = {
    val mountAware: MountAware[S] = new MountAware[S] with WatchScope {
      private var isMounted = false

      override def didMount(native: Native.ReactComp[S]): Unit = {
        isMounted = true
        Var.onChange[S](_var, value => if (isMounted) Future {
          if (isMounted) setStateValue(native, value)
        })
      }

      override def willUnmount(native: Native.ReactComp[S]): Unit = {
        this.cancelAllWatching()
        isMounted = false
      }
    }

    val element = NativeBridge.createElement(
      renderFn = renderFn,
      componentName = name.nameStr,
      mountAware = Some(mountAware)
    )
    RenderAbleImp(element)
  }

  def renderIntoDom(renderAble: RenderAble, container: Element): Unit = renderAble match {
    case RenderAbleImp(element) => ReactDOM.render(element, container)
  }

  private def setStateValue[S](native: Native.ReactComp[S], value: S): Unit = {
    native.setState(Wrapper(value))
  }
}
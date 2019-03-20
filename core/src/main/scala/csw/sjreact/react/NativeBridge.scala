package csw.sjreact.react

import csw.sjreact.dsl.VDom
import csw.sjreact.dsl.VDom._
import csw.sjreact.react.Native.{ReactComp, ReactElement}

import scala.scalajs.js
import scala.scalajs.js.ThisFunction0

private[react] object NativeBridge {
  def createStaticElement(renderFn: () => VDom.Node,
                          componentName: Option[String],
                          mountAware: Option[MountAware[Unit]] = None
                         ): Native.ReactElement = {
    val spec = unitRender(renderFn)
    updateSpec[Unit](componentName, mountAware, spec)
  }

  def createElement[A](renderFn: A => VDom.Node,
                       componentName: Option[String],
                       mountAware: Option[MountAware[A]] = None
                      ): Native.ReactElement = {

    val spec = valueRender(renderFn)

    updateSpec(componentName, mountAware, spec)
  }

  private def updateSpec[A](componentName: Option[String],
                            mountAware: Option[MountAware[A]],
                            renderSpec: js.Dynamic): Native.ReactElement = {
    componentName.foreach(renderSpec.updateDynamic("displayName")(_))
    mountAware foreach { mountAware =>
      renderSpec.updateDynamic("componentDidMount")({ comp: ReactComp[A] =>
        mountAware.didMount(comp)
      }: ThisFunction0[_, _])

      renderSpec.updateDynamic("componentWillUnmount")({ comp: ReactComp[A] =>
        mountAware.willUnmount(comp)
      }: ThisFunction0[_, _])
    }
    val clazz = Native.createReactClass(renderSpec)
    Native.React.createElement(clazz)
  }

  private def unitRender(renderFn: () => VDom.Node): js.Dynamic =
    js.Dynamic.literal(
      "render" -> ({ () =>
        val dom = renderFn()
        node2Element(dom)
      }: js.Function0[_])
    )


  private def valueRender[A](renderFn: A => VDom.Node): js.Dynamic =
    js.Dynamic.literal(
      "render" -> ({ comp: Native.ReactComp[A] =>
        Wrapper.unWrap(comp.state).map { v =>
          val dom = renderFn(v)
          node2Element(dom)
        }.orNull
      }: js.ThisFunction0[_, _])
    )


  def node2Element(node: Node): ReactElement = node match {
    case EmptyNode => null
    case Text(text) =>
      text.asInstanceOf[ReactElement]
    case VoidElement(tag, attributes) =>
      Native.React.createElement(tag, attributes2JsObj(attributes))
    case Element(tag, attributes, children) =>
      Native.React.createElement(
        tag,
        attributes2JsObj(attributes),
        children.map(node2Element): _*)
    case RenderAbleNode(renderAble) =>
      renderAble.asInstanceOf[RenderAbleImp].element
  }

  def attributes2JsObj(attributes: Seq[Attribute]): js.Object = {
    val obj = js.Dynamic.literal()
    attributes.foreach {
      case ValueAttribute(key, value) =>
        val previous = obj.selectDynamic(key)
        if(previous.isInstanceOf[String] && value.isInstanceOf[String]){
          obj.updateDynamic(key)(previous.toString + " " +  value)
        }else {
          obj.updateDynamic(key)(value.asInstanceOf[js.Any])
        }

      case ActionAttribute(key, fun: Function[_, _]) => obj.updateDynamic(key)(fun: js.Function1[_, _])
      case EmptyAttribute => //do nothing
    }
    obj
  }
}

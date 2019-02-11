package csw.sjreact.react

import csw.sjreact.dsl.VDom
import csw.sjreact.dsl.VDom._
import csw.sjreact.react.Native.{ReactComp, ReactElement}

import scala.scalajs.js
import scala.scalajs.js.ThisFunction0

private[react] object NativeBridge {
  def createElement[A](renderFn: A => VDom.Node,
                       componentName: Option[String],
                       mountAware: Option[MountAware[A]] = None
                      ): Native.ReactElement = {

    val spec = js.Dynamic.literal(
      "render" -> ({ comp: Native.ReactComp[A] =>
        Wrapper.unWrap(comp.state).map { v =>
          val dom = renderFn(v)
          node2Element(dom)
        }.orNull
      }: js.ThisFunction0[_, _])
//      "shouldComponentUpdate" -> ({ (comp: Native.ReactComp[A], _: js.Any, nextState: Wrapper[A]) =>
//        !Wrapper.eq(nextState, comp.state)
//      }: js.ThisFunction2[_, _, _, _])
    )

//    spec.updateDynamic("componentDidUpdate")({comp:ReactComp[A] =>
//        println(comp + ": is updated")
//    }:ThisFunction0[_,_])

    componentName.foreach(spec.updateDynamic("displayName")(_))
    mountAware foreach { mountAware =>
      spec.updateDynamic("componentDidMount")({ comp: ReactComp[A] =>
        mountAware.didMount(comp)
      }: ThisFunction0[_, _])

      spec.updateDynamic("componentWillUnmount")({ comp: ReactComp[A] =>
        mountAware.willUnmount(comp)
      }: ThisFunction0[_, _])
    }
    val clazz = Native.createReactClass(spec)
    Native.React.createElement(clazz)
  }


  def node2Element(node: Node): ReactElement = node match {
    case Text(text) => text.asInstanceOf[ReactElement]
    case VoidElement(tag, attributes) => Native.React.createElement(tag, attributes2JsObj(attributes))
    case Element(tag, attributes, children) => Native.React.createElement(tag, attributes2JsObj(attributes), children.map(node2Element): _*)
    case RenderAbleNode(renderAble) => renderAble.asInstanceOf[RenderAbleImp].element
  }

  def attributes2JsObj(attributes: Seq[Attribute]): js.Object = {
    val obj = js.Dynamic.literal()
    attributes.foreach {
      case ValueAttribute(key, value) => obj.updateDynamic(key)(value.asInstanceOf[js.Any])
      case ActionAttribute(key, fun: Function[_, _]) => obj.updateDynamic(key)(fun: js.Function1[_, _])
      case EmptyAttribute => //do nothing
    }
    obj
  }
}

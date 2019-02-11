package csw.sjreact.react

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

private[react] object Native {

  @js.native
  @JSImport("react-dom", JSImport.Namespace)
  object ReactDOM extends js.Object {
    def render(element: ReactElement, node: dom.Node): js.Any = js.native
  }

  @js.native
  trait ReactComp[T] extends js.Object {
    val state: Wrapper[T] = js.native

    def setState(state: Wrapper[T]): Unit = js.native
  }

  @js.native
  trait ReactClass extends js.Object

  @js.native
  trait ReactElement extends js.Object

  @js.native
  @JSImport("create-react-class", JSImport.Default)
  private object NativeCreateReactClass extends js.Object {
    def apply[S](spec: js.Any): ReactClass = js.native
  }

  def createReactClass[T](spec: js.Any): ReactClass = NativeCreateReactClass(spec)


  @js.native
  @JSImport("react", JSImport.Default)
  object React extends js.Object {
    def createElement(tagName: String, attributes: js.Any, children: js.Any*): ReactElement = js.native

    def createElement(reactClass: ReactClass): ReactElement = js.native
  }
}

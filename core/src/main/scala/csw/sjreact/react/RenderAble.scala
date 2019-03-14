package csw.sjreact.react

import csw.sjreact.dsl.VDom

sealed trait RenderAble

private[react] case class RenderAbleImp(element: Native.ReactElement) extends RenderAble

object RenderAble {
  def static(node: VDom.Node): RenderAble = RenderAbleImp(NativeBridge.node2Element(node))

  def ofView(view: View): RenderAble = view.toRenderAble
}

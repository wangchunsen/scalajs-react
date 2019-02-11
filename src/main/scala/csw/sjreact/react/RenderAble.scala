package csw.sjreact.react

sealed trait RenderAble

private[react] case class RenderAbleImp(element: Native.ReactElement) extends RenderAble

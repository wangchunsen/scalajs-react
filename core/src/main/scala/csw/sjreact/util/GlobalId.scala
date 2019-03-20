package csw.sjreact.util

object GlobalId {
  private var globalId = 0

  def nextId: Int = {
    val id = globalId
    globalId += 1
    id
  }
}

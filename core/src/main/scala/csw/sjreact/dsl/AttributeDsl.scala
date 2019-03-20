package csw.sjreact.dsl


trait AttributeDsl {

  import AttrKey._

  val name = attr("name")
  val `class` = attr("className")
  val id = attr("id")
  val `for` = attr("htmlFor")
  val `type` = attr("type")
  val href = attr("href")
  val value = attr("value")
  val placeholder = attr("placeholder")
  val checked = bool("checked")

  val style = obj("style")

  val onClick = action("onClick")
  val onChange = action("onChange")
  val onSubmit = action("onSubmit")
  val onKeyDown = action("onKeyDown")

  def data(dataName: String) = AttrKey.data(dataName)
}
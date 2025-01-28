package codecs

@main def hello(): Unit =
  import Json.*
  import Util.*
  val j = JObject(("name" -> JString("Paul")), ("age" -> JNumber(42)))
  val jArr = JArray(List(j, j))
  println(render(j))

  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

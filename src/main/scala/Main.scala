package codecs

import codecs.Json.JString

case class Person(name: String, age: Int)
// case class CarBuyer(name: String, age: Int, car: Option[String])

// Person companion object
object Person extends PersonCodecs

trait PersonCodecs:
  given Encoder[Person] = ObjectEncoder
    .field[String]("name")
    .zip(ObjectEncoder.field[Int]("age"))
    .encodeAs[Person](p => (p.name, p.age))

  given Decoder[Person] =
    Decoder
      .field[String]("name")
      .zip(Decoder.field[Int]("age"))
      .decodeAs[Person]((name, age) => Person(name, age))
end PersonCodecs

case class Contacts(people: List[Person])
object Contacts extends ContactsCodecs
trait ContactsCodecs

@main def run(): Unit =
  import Json.*
  import Util.*
  val jsonObj = JObject(
    Map(("name" -> JString("Paul")), ("age" -> JNumber(42)))
  )
  println(render(jsonObj)) // prints `String` from facade pattern
  val personAby = Person("Aby", 25)
  println(render(personAby))

  val personJohn = """{"name":"John", "age":30}"""
  val notPerson = """{"name": "Hello", "age": "2A"}"""
  val maybeJson = parseJson(personJohn)
  println(maybeJson.flatMap(_.decodeAs[String]))
  println(maybeJson.flatMap(_.decodeAs[Person]).get)

  val maybeJson2 = parseJson(notPerson)
  println(maybeJson2.flatMap(_.decodeAs[Person]))
  // println(jsonObj.en)

  val carBuyer = """{"name":"John", "age":30, "car":null}"""

  println(msg)

def msg = "I was compiled by Scala 3. :)"

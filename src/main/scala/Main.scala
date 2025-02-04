package codecs

import codecs.Json.JString
import codecs.Json.JObject

case class Person(name: String, age: Int)
// case class CarBuyer(name: String, age: Int, car: Option[String])

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

trait ContactsCodecs:
  given Encoder[Contacts] = ObjectEncoder
    .field[List[Person]]("people")
    .encodeAs[Contacts](contacts => contacts.people)
  given Decoder[Contacts] =
    Decoder
      .field[List[Person]]("people")
      .decodeAs[Contacts](people => Contacts(people))
end ContactsCodecs

@main def run(): Unit =
  import Json.*
  import Util.*
  val jsonObj = JObject(
    Map(("name" -> JString("Paul")), ("age" -> JNumber(42)))
  )
  println(render(jsonObj)) // prints `String` from facade pattern
  val personAby = Person("Aby", 25)
  val personJohn = Person("John", 30)
  println(render(personAby))

  val person = """{"name":"John", "age":30}"""
  val notPerson = """{"name": "Hello", "age": "2A"}"""
  val maybeJson = parseJson(person)
  println(maybeJson.flatMap(_.decodeAs[String]))
  println(maybeJson.flatMap(_.decodeAs[Person]).get)

  val maybeJson2 = parseJson(notPerson)
  println(maybeJson2.flatMap(_.decodeAs[Person]))
  // println(jsonObj.en)

  val carBuyer = """{"name":"John", "age":30, "car":null}"""

  val contacts = Contacts(List(personAby, personJohn))
  println(render(contacts))
  val maybeJsonContact =
    """{"people": [{"name":"John", "age":30}, {"name":"Aby", "age":25}]}"""
  println(parseJson(maybeJsonContact).flatMap(_.decodeAs[Contacts]).get)

  println(msg)

def msg = "I was compiled by Scala 3. :)"

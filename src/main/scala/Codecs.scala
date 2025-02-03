package codecs

sealed trait Json:
  def decodeAs[A](using decoder: Decoder[A]): Option[A] = decoder.decode(this)

object Json:
  case class JNumber(value: BigDecimal) extends Json
  case class JString(value: String) extends Json
  case class JBool(value: Boolean) extends Json
  case class JArray(items: List[Json]) extends Json
  case class JObject(bindings: (String, Json)*) extends Json
  object JNull extends Json

trait Encoder[-A]:
  def encode(data: A): Json

object Encoder extends EncoderInstances:
  /** Convenience method for initializing an instance of encoder from
    */
  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }

import Json.*
trait EncoderInstances:
  /** Encoder for `Unit` values */
  given unitEncoder: Encoder[Unit] = Encoder.fromFunction(_ => JNull)

  /** Encoder for `Int` values */
  given intEncoder: Encoder[Int] =
    Encoder.fromFunction(n => JNumber(BigDecimal(n)))

  /** Encoder for `Boolean` instances */
  given boolEncoder: Encoder[Boolean] = Encoder.fromFunction(b => JBool(b))

  /** Encoder for `String` instances */
  given stringEncoder: Encoder[String] = Encoder.fromFunction(s => JString(s))

  /** Encodes a list of values of type `A` into a JSON array containing the list
    * elements encoded with the implicit conditional type `encoder`
    */
  given listEncoder[A](using encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => JArray(as.map(encoder.encode)))

end EncoderInstances

trait Decoder[+A]:
  /** @param data
    *   The data to de-serialize
    * @return
    *   decoded value wrapped in `Some`, or `None` if decoding failed
    */
  def decode(data: Json): Option[A]

  /** Return the applicative of decode, which is a monad Combines 1
    * @param that
    *   any covriant decoder that returns : `Json`
    * @return
    *   decoded value wrapped
    */
  def zip[B](that: Decoder[B]): Decoder[(A, B)] =
    Decoder.fromFunction { json =>
      this.decode(json).zip(that.decode(json))
    }

  /** Higher order function to map a `Decoder[A]` to `Decoder[B]`
    * @param f
    *   explicit valid conversion for `A` to `B`
    */
  def decodeAs[B](f: A => B): Decoder[B] =
    Decoder.fromFunction(json => this.decode(json).map(f))

object Decoder extends DecoderInstances:
  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  /** Convenience method for decoding an instance of type A
    */
  def fromPartialFunction[A](pf: PartialFunction[Json, A]): Decoder[A] =
    fromFunction(pf.lift)

trait DecoderInstances:
  /** A decoder for the `Unit` value */
  given unitDecoder: Decoder[Unit] =
    Decoder.fromPartialFunction { case JNull => () }

  /** A decoder for `BigDecimal` value */
  given intDecoder: Decoder[Int] = Decoder.fromPartialFunction {
    case JNumber(n) if n.isValidInt => n.intValue
  }

  /** A decoder for `Boolean` value */
  given boolDecoder: Decoder[Boolean] = Decoder.fromPartialFunction {
    case JBool(b) => b
  }

  /** A decoder for `String` values */
  given stringDecoder: Decoder[String] =
    Decoder.fromPartialFunction { case JString(s) => s }

  /** Decoder for JSON arrays. It take an implicit parameter of time `Decoder`,
    * a conditional implicit of `A`. It then `A`
    */
  given listDecoder[A](using decoder: Decoder[A]): Decoder[List[A]] =
    Decoder.fromFunction {
      case JArray(xs) =>
        xs.foldLeft(Option(List.empty[A]))((acc, oe) =>
          acc match
            case Some(acc) => decoder.decode(oe).map(acc :+ _)
            case _         => None
        )
      case _ => Option.empty[List[A]]
    }

  /** Decoder for JSON objects */
  def field[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
    Decoder.fromFunction {
      case JObject(o) if o._1 == name => o._2.decodeAs[A]
      case _                          => null
    }
end DecoderInstances

package codecs

sealed trait Json:
  def decodeAs[A](using decoder: Decoder[A]): Option[A] = decoder.decode(this)

object Json:
  case class JNumber(value: BigDecimal) extends Json
  case class JString(value: String) extends Json
  case class JBool(value: Boolean) extends Json
  case class JArray(items: List[Json]) extends Json
  // case class JObject(bindings: (String, Json)*) extends Json
  case class JObject(bindings: Map[String, Json]) extends Json
  object JNull extends Json

trait Encoder[-A]:
  /** encodes a value of type `A` to json */
  def encode(data: A): Json

  /** Transforms this from `Encoder[A]` => `Encoder[B]`, given a transformation
    * function `B` to `A`. This operation is also known as [[contramap]]
    */
  def encodeAs[B](f: B => A): Encoder[B] =
    Encoder.fromFunction[B](value => this.encode(f(value)))

object Encoder extends EncoderInstances:
  /** Convenience method for initializing an instance of encoder from `map` */
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

/** Shorthand encoder for json object. The type bounds of the object is
  * covariant to prevent undefined inherited object traits
  */
trait ObjectEncoder[-A] extends Encoder[A]:

  def encode(item: A): JObject

  /** A semigroup `combine` method for two Json monads for serial encoding
    * @return
    *
    * a combiner for `this` and `that` instance of Json Object encoders that
    * introduces a SemiGroup
    */
  def zip[B](that: ObjectEncoder[B]): ObjectEncoder[(A, B)] =
    ObjectEncoder.fromFunction((a, b) =>
      JObject(
        this.encode(a).bindings ++ that.encode(b).bindings
      )
    )

end ObjectEncoder

object ObjectEncoder:
  def fromFunction[A](f: A => JObject): ObjectEncoder[A] =
    new ObjectEncoder[A] {
      def encode(item: A): JObject = f(item)
    }

  def field[A](name: String)(using encoder: Encoder[A]): ObjectEncoder[A] =
    ObjectEncoder.fromFunction(a => JObject(Map((name -> encoder.encode(a)))))

end ObjectEncoder

trait Decoder[+A]:
  /** @param data
    *   The data to de-serialize
    * @return
    *   decoded value wrapped in `Some`, or `None` if decoding failed
    */
  def decode(data: Json): Option[A]

  /** Return the applicative of decode, which is a monad for chaining together
    * Json decoder instances
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

end Decoder

object Decoder extends DecoderInstances:
  def fromFunction[A](f: Json => Option[A]): Decoder[A] = new Decoder[A] {
    def decode(data: Json): Option[A] = f(data)
  }

  /** Convenience method for decoding an instance. Throws an exception if Json
    * is an invalid type or invalid conversion type is explicitly supplied
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
    * a conditional implicit of `A`. It then recurse `A` until it reaches the
    * end of a nested block
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

  /** Decoder for JSON objects. Recursively deserializes the Json object until
    * it reaches the end of the nested block
    * @param name
    *   key associated with deserialized json object
    */
  def field[A](name: String)(using decoder: Decoder[A]): Decoder[A] =
    Decoder.fromFunction {
      case JObject(m) if m.contains(name) => m(name).decodeAs[A]
      case _                              => null
    }
end DecoderInstances

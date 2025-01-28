package codecs

sealed trait Json:
  def decodeAs[A](using decoder: Decoder[A]): Option[A] = decoder.decode(this)

object Json: 
  case class JNumber(value: BigDecimal) extends Json
  case class JString(value: String) extends Json
  case class JBool(value: Boolean) extends Json
  case class JArray(items: List[Json]) extends Json
  case class JObject(bindings: Map[String, Json]) extends Json
  object JNull extends Json


trait Encoder[-A]: 
  def encode(data: A): Json 

object Encoder extends EncoderInstances: 
  /**
  * Convenience method for initializing an instance of encoder from
  */
  def fromFunction[A](f: A => Json) = new Encoder[A] {
    def encode(value: A): Json = f(value)
  }

import Json.*            
trait EncoderInstances:
  /** Encoder for `Unit` values */ 
  given unitEncoder: Encoder[Unit] = Encoder.fromFunction(_ => JNull)

  /** Encoder for `Int` values */ 
  given intEncoder: Encoder[Int] = Encoder.fromFunction(n => JNumber(BigDecimal(n)))

  /** Encoder for `Boolean` instances */
  given boolEncoder: Encoder[Boolean] = Encoder.fromFunction(b => JBool(b))
  
  /** Encoder for `String` instances */
  given stringEncoder: Encoder[String] = Encoder.fromFunction(s => JString(s))
  
  /**
  * Encodes a list of values of type `A` into a JSON array containing
  * the list elements encoded with the implicit conditional type `encoder`
  */
  given listEncoder[A](using encoder: Encoder[A]): Encoder[List[A]] =
    Encoder.fromFunction(as => JArray(as.map(encoder.encode)))

end EncoderInstances



trait Decoder[+A]: 
  /**
  * @param data The data to de-serialize
  * @return decoded value wrapped in `Some`, or `None` if decoding failed
  */
  def decode(data: Json): Option[A] 


  /**
    * Return the applicative of decode, which is a monad
    * Combines 1
    * @param data
    * @return decoded value wrapped 
    */
  def zip[B](that: Decoder[B]): Decoder[(A, B)] = ???

object Decoder
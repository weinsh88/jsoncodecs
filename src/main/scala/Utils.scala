package codecs

import org.typelevel.jawn.{Parser}
import org.typelevel.jawn.Facade
import java.math.MathContext

object Util: 
  
  /**
   * Parse a `json` document contained in a `String` value into a `Json` value
   * @param `s` a valid string
   * @return `Json` type. `None` in the case `s` is not a valid `Json` document
  */
  def parseJson(s: String): Option[Json] = Parser.parseFromString[Json](s).toOption

  def render[A](data: A)(using encoder: Encoder[A]): String = 
    render(encoder.encode(data))

  import Json.*
  def render(json: Json): String = json match
    case JNull => "null"
    case JBool(b) => b.toString()
    case JString(s) => fString(s)
    case JNumber(n) => n.toString() 
    case JArray(vs) => vs.map(render).mkString("[", ", " ,"]")
    case JObject(args @ _*) => args match
      case Nil => render(JNull)
      case _ => args.map((k, v) => s"${{fString(k)}}:${render(v)}").mkString("{", ", ", "}")
  
  def fString(s: String): String = 
    val sb = new StringBuilder
    sb.append('"')
    for (alpha <- s) do alpha match {
      case '"' => sb.append("\\\"")
      case '\\' => sb.append("\\\\") 
      case '\r' => sb.append("\\r")
      case '\n' => sb.append("\\n")
      case '\t' => sb.append("\\t")
      case alpha => sb.append(alpha)
    }
    sb.append('"').toString
  
  given Facade.SimpleFacade[Json] with
    def jnull: Json = JNull
    def jarray(vs: List[Json]): Json = JArray(vs)
    def jtrue: Json = JBool(true)
    def jfalse: Json = JBool(false)
    def jnum(s: CharSequence, decIndex: Int, expIndex: Int): Json = JNumber(BigDecimal(s.toString))
    def jstring(s: CharSequence): Json = JString(s.toString)
    def jobject(vs: Map[String, Json]): Json = JObject(vs.toSeq*)

  
end Util
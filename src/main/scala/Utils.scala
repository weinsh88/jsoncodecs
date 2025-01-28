package codecs



object Util: 
  
  /**
   * Parse a `json` document contained in a `String` value into a `Json` value
   * @param `s` a valid string
   * @return `Json` type. `None` in the case `s` is not a valid `Json` document
  */
  // def parseJson(s: String): Option[Json] = Parser.parseFromString[Json](s).toOption

  import Json.*
  private def render(json: Json): String = json match
    case JNull => "null"
    case JBool(b) => b.toString()
    case JString(s) => s
    case JNumber(n) => n.toString()
    case JArray(vs) => vs.map(render).mkString("[", "," ,"]")
    case JObject(vs) => vs.map((k, v)=> s"${k}:${render(v)}").mkString("{", ", ", "}")
  
end Util
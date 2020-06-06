structure JSON_Parser =
struct

  infix 1 <|>
  infix 2 >>= <$> <$ <*> <* *> % <^> 

  fun $ (f,x) = f x
  infixr $

  structure P = MkParser(ListBase)
  open P

  datatype JSON =
      String of string
    | Number of real
    | Bool   of bool
    | Null
    | Object of (string * JSON) list
    | Array  of JSON list

  val op<^> = lift2 op^

  fun mem xs x = List.exists (fn c => x = c) xs

  fun curly x  = between (char #"{") (char #"}") x
  fun square x = between (char #"[") (char #"]") x
  fun keyval k v sep = lift2 Fn.id (k <* sep,v)

  val ws = many $ sat $ mem $ List.map Char.chr [0x0020,0x000A,0x000D,0x0009]

  val escape =  [#"\"",#"\\",#"/",#"\b",#"\f",#"\n",#"\r",#"\t"]
  val plain   =  [#"\"",#"\\",#"/",#"b",#"f",#"n",#"r",#"t"]
  val to_escape = fn
      #"b" => #"\b"
    | #"f" => #"\f"
    | #"n" => #"\n"
    | #"r" => #"\r"
    | #"t" => #"\t"
    | n    => n

  val digits = String.implode <$> many1 digit

  fun json i = element i

  and value i = multi_choice [object,array,String <$> str,num,tt,ff,null] $ i

  and object i = Object <$> (curly (members <|> [] <$ ws)) $ i

  and members i = sepby1 member (char #",") $ i

  and member i = keyval (between ws ws str) element (char #":") $ i

  and array i = Array <$> (square (elements <|> [] <$ ws)) $ i

  and elements i = sepby1 element (char #",") $ i

  and element i = between ws ws value $ i

  and str i = 
    String.implode <$> between (char #"\"") (char #"\"") (many1 $ character) $ i

  and character i = 
    sat (not o mem escape) <|> (char #"\\") *> (to_escape <$> (sat $ mem plain)) $ i

  and num i = Number <$> (valOf o Real.fromString <$> (integer <^> fraction <^> exponent)) $ i

  and integer i = (string "-" <|> return "") <^> digits $ i

  and fraction i = string "." <^> digits <|> return "" $ i

  and exponent i = (string "e" <|> string "E") <^> sign <^> digits <|> return "" $ i

  and sign i = string "+" <|> string "-" <|> return "" $ i

  and tt i = Bool true <$ string "true" $ i

  and ff i = Bool false <$ string "false" $ i

  and null i = Null <$ string "null" $ i


  fun parse_json s = 
    Option.map #1 $ List.find (List.null o #2) $ json % s

end    

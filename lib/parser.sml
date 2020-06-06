functor MkParser (M : BASE) : PARSER =
struct
  type 'a m = 'a M.t
  type 'a t = char list -> ('a * char list) m
  type 'a parser = 'a t
  
  infix 2 >>= <|> <$> <$ <*> <* *> %

  fun $ (f,x) = f x
  infixr $

  fun return x i = M.return (x,i)

  fun bind p f i = 
    M.bind (p i)  (Fn.uncurry f)

  fun fail _ = M.fail

  fun choice p q i = M.choice (p i) (q i)

  val op>>= = fn x => Fn.uncurry bind x
  val op<|> : ('a parser * 'a parser) -> 'a parser = fn x => Fn.uncurry choice x

  fun map (f : 'a -> 'b) (p : 'a parser) : 'b parser = p >>= return o f

  val op<$> = fn x => Fn.uncurry map x

  val op<$ = fn (x,p) => Fn.const x <$> p

  fun apply p q =
    p >>= (fn f =>
    q >>= (fn x =>
    return $ f x))

  val op<*> = fn x => Fn.uncurry apply x

  fun p *> q = p >>= Fn.const q
  
  fun p <* q = p >>= (fn x => q *> return x)

  fun lift2 f (p,q) = Fn.curry f <$> p <*> q

  val multi_choice = fn x => foldr op<|> fail x

  val item = fn
    []    => fail []
  | x::xs => return x xs


  fun sat p =
    item >>= (fn c =>
    if p c then return c else fail)

  fun char c = sat (fn x => x = c)

  fun in_range (x,y) (c:char) = x<=c andalso c<=y

  val digit = sat $ in_range (#"0",#"9")

  val lower = sat $ in_range (#"a",#"z")

  val upper = sat $ in_range (#"A",#"Z")

  val letter = lower <|> upper

  val alphanum = letter <|> digit

  val rec char_list = fn
    []    => return []
  | x::xs => lift2 op:: (char x, char_list xs)

  fun string s = String.implode <$> (char_list $ String.explode s)

  fun many p = lift2 op:: (p, fn x => many p x) <|> return []
  fun many1 p = lift2 op:: (p, many p)

  val nat : int parser = 
    let
      fun toNum c = Char.ord c - Char.ord #"0"
      val eval = foldl (fn (c,i) => toNum c + 10*i) 0
    in
      eval <$> many1 digit
    end
  
  val int = ((op~ <$ char #"-") <|> return Fn.id) <*> nat
   
  fun sepby1 p sep = lift2 op:: (p, many $ sep *> p) 
  fun sepby p sep = sepby1 p sep <|> return []

  fun between l r p = l *> p <* r

  fun chainl1 ts os =
    let
      fun rest x =
        os >>= (fn f =>
        ts >>= (fn y =>
        return $ f (x,y))) 
        <|> return x
    in
      ts >>= rest
    end

  fun chainr1 ts os =
    ts >>= (fn x =>
    os >>= (fn f =>
    chainr1 ts os >>= (fn y =>
    return $ f (x,y)))
    <|> return x)
  
 fun p % s = p $ String.explode s
end

structure ListBase : BASE =
struct
  type 'a t = 'a list
  fun return x = [x]
  fun bind x f = List.concatMap f x
  val fail = []
  val choice = fn x => Fn.curry op@ x
end

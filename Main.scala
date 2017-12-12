object Main extends App {
    import scala.language.implicitConversions
    import scala.language.reflectiveCalls

    abstract class Regexp 
    case object ZERO                        extends Regexp
    case object ONE                         extends Regexp
    case class  CHAR(c: Char)               extends Regexp
    case class  SEQ(r1: Regexp, r2: Regexp) extends Regexp
    case class  ALT(r1: Regexp, r2: Regexp) extends Regexp
    case class  STAR(r: Regexp)             extends Regexp
    case class  C_SET(s: Set[Char])         extends Regexp
    case class  PLUS(r: Regexp)             extends Regexp
    case class  OPT(r: Regexp)              extends Regexp
    case class  EXACT_N(r: Regexp, n: Int)  extends Regexp
    case class  NOT(r: Regexp)              extends Regexp
    case class  CFUN(f: (Char) => Boolean)  extends Regexp
    case class  REC(x: String, r: Regexp)   extends Regexp

    abstract class Val
    case object Empty                  extends Val
    case class  Chr(c: Char)           extends Val
    case class  Sequ(v1: Val, v2: Val) extends Val
    case class  Left(v: Val)           extends Val
    case class  Right(v: Val)          extends Val
    case class  Stars(vs: List[Val])   extends Val
    case class  Recd(x: String, v: Val) extends Val

    def charlist2rexp(s: List[Char]): Regexp = s match {
        case Nil => ONE
        case c::Nil => CHAR(c)
        case c::s => SEQ(CHAR(c), charlist2rexp(s))
    }

    implicit def string2rexp(s: String): Regexp = charlist2rexp(s.toList)

    implicit def RexpOps(r: Regexp) = new {
        def | (s: Regexp) = ALT(r, s)
        def ~ (s: Regexp) = SEQ(r, s)
        def % = STAR(r)
    }
    implicit def stringOps(s: String) = new {
        def | (r: Regexp) = ALT(s, r)
        def | (r: String) = ALT(s, r)
        def ~ (r: Regexp) = SEQ(s, r)
        def ~ (r: String) = SEQ(s, r)
        def % = STAR(s)
    }

// matching w/ regex
    def nullable(reg: Regexp): Boolean = { 
        reg match {
            case ZERO            => false
            case ONE             => true
            case CHAR(_)         => false
            case SEQ(r1,r2)      => nullable(r1) && nullable(r2)
            case ALT(r1,r2)      => nullable(r1) || nullable(r2)
            case STAR(r)         => true
            case C_SET(s)        => false
            case PLUS(r)         => false
            case OPT(r)          => true
            case EXACT_N(r,n)    => if(n == 0) true else nullable(r)
            case CFUN(f)         => false 
            case NOT(r)          => !nullable(r)
            case _               => {
                println("Error occured in Nullable!")
                false
            }
        }
    }

    def der_c(reg: Regexp, c: Char): Regexp = {
        // println(reg)
        reg match {
            case ZERO            => ZERO
            case ONE             => ZERO
            case CHAR(h)         => if(h==c) ONE else ZERO
            case SEQ(r1,r2)      => {
                if(nullable(r1))
                    ALT(SEQ(der_c(r1,c),r2), der_c(r2,c))
                else
                    SEQ(der_c(r1,c), r2)
            }
            case ALT(r1,r2)      => ALT(der_c(r1,c), der_c(r2,c))
            case STAR(r)         => SEQ(der_c(r,c), STAR(r))
            case C_SET(s)        => if(s.contains(c)) ONE else ZERO
            case EXACT_N(r,n)    => {
                if(n == 0) 
                    ZERO
                else 
                    SEQ(der_c(r,c), EXACT_N(r,n-1))
            }
            case OPT(r)          => der_c(r,c)
            case PLUS(r)         => SEQ(der_c(r,c), STAR(r))
            case CFUN(f)         => {
                if( f(c) )
                    ONE
                else 
                    ZERO
            }
            case NOT(r)   => NOT(der_c(r,c))
            case REC(_,r) => der_c(r,c)
            case _          => {
                println("Error occured in Der!")
                ZERO
            }

        }
    }

    def der_s(reg: Regexp, s: List[Char]): Regexp = {
        println(reg)
        s match {
            case Nil => reg
            case c::s => der_s(der_c(reg,c), s)
            // case c::s => der_s(simplify(der_c(reg,c)), s)
        }
    }

    def simplify(r: Regexp) : Regexp = r match {
        case ALT(r1, r2) => (simplify(r1), simplify(r2)) match {
            case (ZERO, r2s) => r2s
            case (r1s, ZERO) => r1s
            case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
        }
            case SEQ(r1, r2) =>  (simplify(r1), simplify(r2)) match {
                case (ZERO, _) => ZERO
                case (_, ZERO) => ZERO
                case (ONE, r2s) => r2s
                case (r1s, ONE) => r1s
                case (r1s, r2s) => SEQ(r1s, r2s)
            }
                case r => r
    }

    def matches(reg: Regexp, s: String): Boolean = {
        nullable(der_s(reg, s.toList))
    }

// ****************************************************************************************************
// lexing
    def mkeps(reg: Regexp): Val = {
        // println(reg)
        reg match { 
            case ONE          => Empty
            case ALT(r1,r2)   => {
                if(nullable(r1)) 
                    Left(mkeps(r1))
                else 
                    Right(mkeps(r2))
            }
            case SEQ(r1,r2)   => Sequ(mkeps(r1),mkeps(r2))
            case STAR(r)      => Stars(Nil)
            case REC(x,r)     => Recd(x,mkeps(r))
            case OPT(r)       => Empty
            case EXACT_N(r,0) => {
                Empty
            }
            case EXACT_N(r,n) => {
                mkeps(r)
            }
            case _            => {
                println(s"MKEPS: passed reg w/no case: $reg")
                Empty
            }
            /*
            // plus and c_set are not nullable, thus mkeps will never see them. 
            case PLUS(r)    => 
            case C_SET(s)   => 
            */
        }
    }

    def inj(to_inj: Char, reg: Regexp, v: Val): Val = (reg, v) match {
        case (STAR(r), Sequ(v1, Stars(vs)))    => Stars(inj(to_inj, r, v1)::vs)
        case (SEQ(r1, r2), Sequ(v1, v2))       => Sequ(inj(to_inj, r1, v1), v2)
        case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(to_inj, r1, v1), v2)
        case (SEQ(r1, r2), Right(v2))          => Sequ(mkeps(r1), inj(to_inj, r2, v2))
        case (ALT(r1, r2), Left(v1))           => Left(inj(to_inj, r1, v1))
        case (ALT(r1, r2), Right(v2))          => Right(inj(to_inj, r2, v2))
        case (CHAR(d), Empty)                  => Chr(to_inj) 
        case (REC(x, r1), _)                   => Recd(x, inj(to_inj, r1, v))
        case (EXACT_N(r,1), Sequ(v1,v2))       => inj(to_inj, r, v2)
        case (EXACT_N(r,n), Sequ(v1,v2))       => Sequ(inj(to_inj, r, v1), v2)
        case (C_SET(cs), _)                    => Chr(to_inj)
        case (OPT(r), _)                       => inj(to_inj, r, v)
        case (PLUS(r), Sequ(v1,Stars(vs)))     => Stars(inj(to_inj, r, v1)::vs)
        case (_,_)                             => {
            throw new Exception(s"inj: no match: ($reg, $v)")
        }
    }   

    def flatten(v: Val): String = v match {
        case Empty       => ""
        case Chr(c)      => c.toString
        case Sequ(v1,v2) => flatten(v1) + flatten(v2)
        case Left(v)     => flatten(v)
        case Right(v)    => flatten(v)
        case Stars(vs)   => vs.map(flatten).mkString
        case Recd(_, v)  => flatten(v)
    }

    def env(v: Val): List[(String,String)] = v match {
        case Empty       => Nil
        case Chr(c)      => Nil
        case Left(v1)    => env(v1)
        case Right(v1)   => env(v1)
        case Sequ(v1,v2) => env(v1) ::: env(v2)
        case Stars(vs)   => vs.flatMap(env)
        case Recd(x, v1) => (x, flatten(v1))::env(v1)
    }

    def lex(reg: Regexp, s: List[Char]): Val = s match {
        case Nil   => if(nullable(reg)) {
                          val m = mkeps(reg)
                          m
                      }
                      else  
                          throw new Exception("Not matched")
        case c::cs => {
            val (r_simp, f_simp) = simp(der_c(reg, c))
            inj(c, reg, f_simp(lex(r_simp, cs)))
         }
    }

    def lexing(reg: Regexp, s: String): Val = {
        lex(reg, s.toList)
    }

// simp functions for lexing
    def F_ID(v: Val): Val = v

    def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))

    def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))

    def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
        case Right(v) => Right(f2(v))
        case Left(v) => Left(f1(v))
    }

    def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
        case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
    }

    def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
        (v:Val) => Sequ(f1(Empty), f2(v))

    def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
        (v:Val) => Sequ(f1(v), f2(Empty))

    def F_ERROR(v: Val): Val = throw new Exception("error")

    def simp(r: Regexp): (Regexp, Val => Val) = r match {
        case ALT(r1, r2) => {
            val (r1s, f1s) = simp(r1)
            val (r2s, f2s) = simp(r2)
            (r1s, r2s) match {
                case (ZERO, _) => (r2s, F_RIGHT(f2s))
                case (_, ZERO) => (r1s, F_LEFT(f1s))
                case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s))
            }
        }
                case SEQ(r1, r2) => {
                    val (r1s, f1s) = simp(r1)
                    val (r2s, f2s) = simp(r2)
                    (r1s, r2s) match {
                        case (ZERO, _) => (ZERO, F_ERROR)
                        case (_, ZERO) => (ZERO, F_ERROR)
                        case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
                        case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
                        case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
                    }
                }
                        case r => (r, F_ID)
    }


// regex definitions for WHILE lexer
    val alpha_set: Regexp   = C_SET("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toSet)
    val num_set: Regexp           = C_SET("0123456789".toSet)
    val num_set_without_0: Regexp = C_SET("123456789".toSet)

    val key_r: Regexp = REC("keyword", "while" | "if" | "then" | "else" | "do" | "for" | "to" | "true" | "false" | "read" | "write" | "skip")
    val ops_r: Regexp = REC("operation", "+" | "-" | "*" | "%" | "/" | "==" | "!=" | "<" | ">" | ":=" | "&&" | "||")
    val str_r: Regexp = REC("string", "\"" ~ STAR(alpha_set) ~ "\"")
    val paren_r: Regexp = REC("parenthesis", "(" | ")" | "{" | "}" )
    val semi_r: Regexp = REC("semicolon", ";")
    val white_r: Regexp = REC("whitespace", PLUS("\n" | "\t" | " "))
    val ident_r: Regexp = REC("identifier", (alpha_set) ~ STAR(alpha_set | num_set | "_"))
    val num_r: Regexp = REC("number", "0" | (num_set_without_0 ~ STAR(num_set)))

    val lexing_reg: Regexp = STAR(str_r | white_r | paren_r | ops_r | semi_r | key_r | ident_r | num_r)

// ****************************************************************************************************
// parsing 
//          note: T as a type refers to the return type of the parser.
    
    abstract class Parser[Inp <% Seq[_], T] {
        def parse(token_list: Inp): Set[(T, Inp)]

        def parse_all(token_list: Inp): Set[T] = 
            for ( (head, tail) <- parse(token_list);
                  if (tail.isEmpty)) yield head
    }

    class AltParser[Inp <% Seq[_], T](p: => Parser[Inp, T],
                                      q: => Parser[Inp, T])
                                            extends Parser[Inp, T] {
        def parse(token_list: Inp) = {
            p.parse(token_list) ++ q.parse(token_list)
        }
    }

    class SeqParser[Inp <% Seq[_], T, S](p: => Parser[Inp, T],
                                         q: => Parser[Inp, S])
                                            extends Parser[Inp, (T, S)] {
        def parse(token_list: Inp) = {
            for ((out1, unparsed1) <- p.parse(token_list);
                 (out2, unparsed2) <- q.parse(unparsed1))
                    yield ((out1, out2), unparsed2)
        }
    }
    class FuncParser[Inp <% Seq[_], T, S](p: => Parser[Inp, T],
                                          func: T => S)
                                            extends Parser[Inp, S] {
        def parse(token_list: Inp) = {
            for ((out, unparsed) <- p.parse(token_list))
                 yield (func(out), unparsed)
        }
    }

    case class KeyParser(s: String) extends Parser[List[(String,String)], List[(String,String)]] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head == ("keyword", s)))
                Set((List(token_list.head), token_list.drop(1)))
            else
                Set()
        }
    }

    case class OpParser(s: String) extends Parser[List[(String,String)], List[(String,String)]] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head == ("operation", s) ))
                Set((List(token_list.head), token_list.drop(1)))
            else
                Set()
        }
    }

    case class ParParser(s: String) extends Parser[List[(String,String)], List[(String,String)]] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head == ("parenthesis", s) ))
                Set((List(token_list.head), token_list.drop(1)))
            else
                Set()
        }
    }

    case object IdParser extends Parser[List[(String,String)], (String,String)] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head._1 == "identifier"))
                Set(((token_list.head), token_list.drop(1)))
            else 
                Set()
        }
    }

    case object NumParser extends Parser[List[(String,String)], (String,String)] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head._1 == "number"))
                Set(((token_list.head), token_list.drop(1)))
            else 
                Set()
        }
    }

    case class TypeParser(typ: String) extends Parser[List[(String,String)], List[(String,String)]] {
        def parse(token_list: List[(String,String)] ) = {
            if ((!token_list.isEmpty) && (token_list.head._1 == typ))
                Set((List(token_list.head), token_list.drop(1)))
            else
                Set()
        }
    }

    implicit def ParserOps[Inp <% Seq[_], T](p: Parser[Inp, T]) = new {
        def || (q : => Parser[Inp, T]) = new AltParser[Inp, T](p, q)
        def ==>[S] (f: => T => S) =  new FuncParser[Inp, T, S](p, f)
        def ~[S] (q: => Parser[Inp, S]) = new SeqParser[Inp, T, S](p, q)
    }

    val while_p = new KeyParser("while")
    val if_p    = new KeyParser("if")
    val then_p  = new KeyParser("then")
    val else_p  = new KeyParser("else")
    val do_p    = new KeyParser("do")
    val to_p    = new KeyParser("to")
    val true_p  = new KeyParser("true")
    val false_p = new KeyParser("false")
    val read_p  = new KeyParser("read")
    val write_p = new KeyParser("write")
    val skip_p  = new KeyParser("skip")

    val plus_p   = new OpParser("+")
    val minus_p  = new OpParser("-")
    val times_p  = new OpParser("*")
    val div_p    = new OpParser("/")
    val mod_p    = new OpParser("%")
    val eq_p     = new OpParser("=")
    val nq_p     = new OpParser("!=")
    val lt_p     = new OpParser("<")
    val gt_p     = new OpParser(">")
    val assign_p = new OpParser(":=")
    val and_p    = new OpParser("&&")
    val or_p     = new OpParser("||")

    val lparen_p = new ParParser("(")
    val rparen_p = new ParParser(")")
    val lbrack_p = new ParParser("{")
    val rbrack_p = new ParParser("}")

    val whitespace_p = new TypeParser("whitespace")
    val str_p        = new TypeParser("string")
    val num_p        = new TypeParser("number")
    val semicolon_p  = new TypeParser("semicolon")

    val test_one = List( ("keyword", "while"), ("keyword", "true"), ("keyword", "do"), ("string", "foogle") )
    val test_parser = while_p ~ true_p ~ do_p ~ str_p
//    println(test_parser.parse(test_one))

    abstract class Stmt
    abstract class AExp
    abstract class BExp

    type Block = List[Stmt]

    case object Skip                                 extends Stmt
    case class  If(bo: BExp, bl1: Block, bl2: Block) extends Stmt
    case class  While(bo: BExp, bl: Block)           extends Stmt
    case class  Assign(id: (String,String), a: AExp)          extends Stmt
    case class  Read(id: (String,String))                     extends Stmt
    case class  Write(id: (String,String))                    extends Stmt

    case class Var(id: (String,String))                    extends AExp
    case class Num(i: (String, String))                        extends AExp
    case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

    case object True                               extends BExp
    case object False                              extends BExp
    case class  Bop(o: String, a1: AExp, a2: AExp) extends BExp

    lazy val AExp: Parser[List[(String,String)], AExp] = 
        ((Med ~ plus_p  ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z): AExp } ||
         (Med ~ minus_p ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z): AExp } || 
          Med)
    lazy val Med: Parser[List[(String,String)], AExp] = 
        ((Hi ~ times_p ~ Med) ==> { case ((x, y), z) => Aop("*", x, z): AExp } ||
         (Hi ~ div_p   ~ Med) ==> { case ((x, y), z) => Aop("/", x, z): AExp } ||
         (Hi ~ mod_p   ~ Med) ==> { case ((x, y), z) => Aop("%", x, z): AExp } || 
          Hi)
    lazy val Hi: Parser[List[(String,String)], AExp] = 
        ((lparen_p ~ AExp ~ rparen_p) ==> { case ((x, y), z) => y } ||
          IdParser  ==> Var ||
          NumParser ==> Num)

    lazy val BExp: Parser[List[(String,String)], BExp] = 
        ((AExp ~ eq_p ~ AExp) ==> { case ((x, y), z) => Bop("=", x, z):BExp } || 
         (AExp ~ nq_p ~ AExp) ==> { case ((x, y), z) => Bop("!=", x, z):BExp } || 
         (AExp ~ lt_p ~ AExp) ==> { case ((x, y), z) => Bop("<", x, z):BExp } || 
         (AExp ~ gt_p ~ AExp) ==> { case ((x, y), z) => Bop(">", x, z):BExp } || 
         (true_p  ==> ((_) => True:BExp )) || 
         (false_p ==> ((_) => False:BExp )) ||
         (lparen_p ~ BExp ~ rparen_p) ==> { case ((x, y), z) => y})

    lazy val Stmt: Parser[List[(String,String)], Stmt] = 
        ((skip_p ==> ((_) => Skip: Stmt)) ||
         (IdParser ~ assign_p ~ AExp) ==> { case ((x, y), z) => Assign(x, z): Stmt } ||
         (if_p ~ BExp ~ then_p ~ Block ~ else_p ~ Block) ==>
         { case (((((x,y),z),u),v),w) => If(y, u, w): Stmt } ||
         (while_p ~ BExp ~ do_p ~ Block) ==> { case (((x, y), z), w) => While(y, w) } ||
         (read_p  ~ IdParser) ==> { case (x, y) => Read(y)  } ||
         (write_p ~ IdParser) ==> { case (x, y) => Write(y) }) 

    lazy val Stmts: Parser[List[(String,String)], Block] =
        (Stmt ~ semicolon_p ~ Stmts) ==> { case ((x, y), z) => x :: z : Block } ||
        (Stmt ==> ((s) => List(s) : Block))

    lazy val Block: Parser[List[(String,String)], Block] = 
        ((lbrack_p ~ Stmts ~ rbrack_p) ==> { case ((x, y), z) => y} || 
         (Stmt ==> ((s) => List(s))))

    def isNotWhitespaceOrComment(element: (String,String)): Boolean = {
        return !((element._1 == "whitespace") || (element._1 == "comment"))
    }

    def parse_block(s: String): Set[List[Stmt]] = Block.parse_all(env(lexing(lexing_reg, s)).filter(isNotWhitespaceOrComment))


// ****************************************************************************************************
    // interpretation 

    type Env = Map[String, Int]

    def eval_aexp(a: AExp, env : Env) : Int = {
        a match {
            case Num(i) => i._2.toInt
            case Var(s) => env(s._2)
            case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
            case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
            case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
            case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
            case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
        }
    }

    def eval_bexp(b: BExp, env: Env) : Boolean = b match {
        case True => true
        case False => false
        case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
        case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
        case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
        case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    }

    def eval_stmt(s: Stmt, env: Env) : Env = s match {
        case Skip => env
        case Assign(x, a) => env + (x._2 -> eval_aexp(a, env))
        case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
        case While(b, bl) => 
            if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
            else env
        case Read(n) =>
            env + (n._2 -> scala.io.StdIn.readLine().toInt)
        case Write(n) =>
            println(env(n._2))
            env

    }

    def eval_bl(bl: Block, env: Env) : Env = bl match {
        case Nil => env
        case s::bl => eval_bl(bl, eval_stmt(s, env))
    }

    def eval(bl: Block) : Env = {
        eval_bl(bl, Map())
    }

    val timing_test = """{start := 100;
        x := start;
        y := start;
        z := start;
        while 0 < x do {
            while 0 < y do {
                while 0 < z do { z := z - 1 };
                z := start;
                y := y - 1
            };
            y := start;
            x := x - 1
        }
    }
"""

    val fib = """{read n;
    minus1:=0;
    minus2:= 1;
    temp :=0;
    while(n>0)do   {temp:=minus2;
    minus2:=minus1+ minus2;
    
    minus1:=temp;n:=n-1};result:=minus2}"""

    def time_function(fun: => Map[String, Int]): Long = {
        val start_time = System.currentTimeMillis
        val my_fun = fun
        System.currentTimeMillis - start_time
    }

    
    println(time_function(eval(parse_block(timing_test).head)))
}


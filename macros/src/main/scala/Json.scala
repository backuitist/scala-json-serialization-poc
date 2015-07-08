package ms.webmaster.macroserialization

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context // scala 2.11: {BlackboxContext => Context}
import scala.annotation.tailrec
import java.io._

// Copied from https://bitbucket.org/jaroslav/scala-macro-serialization

object Json {
  private def interleave[T](lst: List[T], x: T) = lst.flatMap(List(x, _)).tail

  class Exception(s: String) extends RuntimeException(s)

  /******************************************************************************************************************
   *  Packer                                                                                                        *
   ******************************************************************************************************************/
  trait AuxPacker[T] {
    def packto(os: Appendable, t: T)
  }

  final def pack[A](a: A): String = macro packImpl[A]

  final def packImpl[A : c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[String] = {
    import c.universe._
    // todo: avoid StringWriter creation for simple types
    reify {
      val sw = new StringWriter
      packtoImpl(c)(c.Expr[Appendable](Ident(newTermName("sw"))), a).splice
      sw.toString
    }
  }


  final def packCharSequence(out: Appendable, s: CharSequence) {
    if (s == null) {
      out append "null"
    } else {
      out append '"'
      for(i <- 0 until s.length) s.charAt(i) match {
        case '\"'                                           => out append "\\\""
        case '\\'                                           => out append "\\\\"
        case '\b'                                           => out append "\\b"
        case '\f'                                           => out append "\\f"
        case '\n'                                           => out append "\\n"
        case '\r'                                           => out append "\\r"
        case '\t'                                           => out append "\\t"
        case c if Character.getType(c) == Character.CONTROL => out append "\\u%04X".format(c.toInt)
        case c                                              => out append c
      }
      out append '"'
    }
  }

  // type not known at compile time
  final def packAny(out: Appendable, value: Any) {
    value match {
    case null => out append "null"

    case x: collection.GenMapLike[_,_,_] =>
      var needcomma = false
      out append '{'
      x foreach { case (k,v) =>
        if (needcomma) out.append(',') else needcomma=true
        packAny(out, k)
        out append ':'
        packAny(out, v)
      }
      out append '}'

    case x: Array[_] =>
      var needcomma = false
      out append '['
      for(v <- x) {
        if (needcomma) out.append(',') else needcomma=true
        packAny(out, v)
      }
      out append ']'

    case x: TraversableOnce[_] => // also iterator?
      var needcomma = false
      out append '['
      for(v <- x) {
        if (needcomma) out.append(',') else needcomma=true
        packAny(out, v)
      }
      out append ']'

    case x: Product       => packAny(out, x.productIterator)

    case x: CharSequence  => packCharSequence(out, x)

    case x: Byte          => out append x.toString
    case x: Short         => out append x.toString
    case x: Char          => out append x.toInt.toString
    case x: Int           => out append x.toString
    case x: Long          => out append x.toString
    case x: Float         => out append x.toString
    case x: Double        => out append x.toString
    case x: Boolean       => out append x.toString
    // todo: class _ extends AnyVal
    case _ =>
      println("x=" + value + ": " + value.getClass)
      ???
    }
  }


  final def packto[A](output: Appendable, value: A): Unit = macro packtoImpl[A]

  final def packtoImpl[A : c.WeakTypeTag](c: Context)(output: c.Expr[Appendable], value: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    import definitions._

    val JSON = q"_root_.ms.webmaster.macroserialization.Json"

    val out = newTermName(c.fresh("out$"))

    def packcode(tpe: c.Type, w: c.Tree): c.Tree = {
      if (tpe =:= NullTpe)                                                   { q"""$out.append("null")"""
      } else if(tpe =:= AnyTpe     || tpe =:= AnyValTpe
             || tpe =:= AnyValTpe  || tpe =:= c.typeOf[java.lang.Object]   ) { q"$JSON.packAny($out, $w)"
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character]) { q"$out.append($w.toInt.toString)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
             || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
             || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
             || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
             || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]
             || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]
             || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) { q"$out.append($w.toString)"
      } else if (tpe <:< c.typeOf[CharSequence])                             { q"$JSON.packCharSequence($out, $w)"

      } else if (tpe.baseClasses.exists(_.fullName.toString == "scala.collection.GenMapLike")) {
        val MethodType(List(arg), tpeValue) = tpe.member(newTermName("apply")).typeSignatureIn(tpe)
        val tpeKey = arg.typeSignatureIn(tpe)
        q"""$out.append('{')
            var needComma = false
            $w foreach { x =>
              if (needComma) $out.append(',')
              needComma = true
              ${ packcode(tpeKey, q"x._1") }
              $out.append(':')
              ${ packcode(tpeValue, q"x._2") }
            }
            $out.append('}')"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.Array") { // Array is final class
        val TypeRef(_, _, List(tpeElement)) = tpe
        val tmp = newTermName(c.fresh("x$"))
        q"""val $tmp = $w /* eval once */
            $out.append('[')
            0 until $tmp.length foreach { i =>
              if (i != 0) $out.append(',')
              ${ packcode(tpeElement, q"$tmp(i)") }
            }
            $out.append(']')"""

      } else if (tpe.baseClasses.exists(_.fullName.toString == "scala.collection.TraversableOnce")) {
        val NullaryMethodType(tpeElement) = tpe.member(newTermName("head")).typeSignatureIn(tpe)
        q"""var needComma = false
            $out.append('[')
            $w foreach { x =>
              if (needComma) $out.append(',')
              needComma = true
              ${ packcode(tpeElement, q"x") }
            }
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.None") { // todo: pack as null
        q"""$out.append('[')
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.Some") {
        val TypeRef(_, _, List(tpeElement)) = tpe
        q"""$out.append('[')
            ${ packcode(tpeElement, q"$w.get") }
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.Option") { // todo: pack None as null
        val TypeRef(_, _, List(tpeElement)) = tpe
        q"""$out.append('[')
            $w foreach { x =>
              ${ packcode(tpeElement, q"x") }
            }
            $out.append(']')"""

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass) { // case class or tuple
        val getters: List[MethodSymbol] = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)

        val tmp = newTermName(c.fresh("x$"))
        val packers = for((sym, i) <- getters.zipWithIndex; NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe)) yield packcode(tpeElement, q"$tmp.${sym.name}")
        q"""val $tmp = $w  /* eval  once */
            $out.append('[')
            ..${ interleave(packers, q"$out.append(',')") }
            $out.append(']')"""

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isDerivedValueClass) { // value class; pack as element type
        val List(getter) = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
        val NullaryMethodType(tpeElement) = getter.typeSignatureIn(tpe)
        packcode(tpeElement, q"$w.${getter.name}")

      } else {
        // can refer to local Json
        q"implicitly[Json.AuxPacker[$tpe]].packto($out, $w)"
      }
    }
    val rc = c.Expr[Unit](q"""val $out = ${output.tree} /* eval once */
                              ${packcode(c.weakTypeOf[A], value.tree)}""")
    //println(s"// packer ${c.weakTypeOf[A]}\n" + rc)
    rc
  }


  /******************************************************************************************************************
   *  Unpacker                                                                                                      *
   ******************************************************************************************************************/
  trait AuxUnpacker[T] {
    def unpackStream(in: Reader): T
  }

  @tailrec
  final def getcNoSpace(in: Reader): Int = in.read() match {
    case ' ' | '\t' | '\r' | '\n' => getcNoSpace(in)
    case c => c
  }

  final def expect(in: Reader, x: Int) {
    val c = getcNoSpace(in)
    if (c != x)
      throw new Json.Exception(printAsChar(x) + " expected, got " + printAsChar(c))
  }

  private[this] def unhex(i: Int): Int = i match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => i - '0'
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F'                         => i - 'A' + 10
    case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'                         => i - 'a' + 10
  }

  final def printAsChar(x: Int) = x match {
    case -1                          => "EOF"
    case c if 0x0021<=c && c<=0x007E => "'%c'" format c
    case c if 0x0000<=c && c<=0xFFFF => "'\\u%04X'" format c
  }

  final def unpackString(in: PushbackReader): String = getcNoSpace(in) match {
    case '"' =>
      val sb = new java.lang.StringBuilder
      while(true) {
        in.read() match {
        case -1 =>
          throw new EOFException
        case '"' =>
          return sb.toString
        case '\\' =>
          sb append (in.read() match {
            case -1     => throw new EOFException
            case 'u'    => (unhex(in.read())*4096 + unhex(in.read())*256 + unhex(in.read())*16 + unhex(in.read())).toChar
            case 'b'    => '\b'
            case 'f'    => '\f'
            case 'n'    => '\n'
            case 'r'    => '\r'
            case 't'    => '\t'
            case c      => c.toChar
          })
        case c   =>
          sb append c.toChar
        }
      }
      ???
    case 'n' =>
      in.unread('n')
      unpackNull(in)

    case c => throw new Json.Exception("'\"' or 'null' expected, got " + printAsChar(c))
  }

  final def unpackLong(in: PushbackReader): Long = getcNoSpace(in) match {
    case '-' =>
      -unpackLong(in)
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
      var rc = (c - '0').toLong
      while(true) {
        in.read() match {
          case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
            rc = rc * 10 + (c - '0').toLong
          case c =>
            if (c != -1) in.unread(c)
            return rc
        }
      }
      ???

    case c => throw new Json.Exception("digit or '-' expected, got " + printAsChar(c))
  }

  final def unpackDouble(in: PushbackReader): Double = getcNoSpace(in) match {
    case '-' =>
      -unpackDouble(in)
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
      var s = c.toChar.toString
      while(true) {
        in.read() match {
          case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'e' | 'E' | '.' | '+' | '-') =>
            s += c.toChar
          case c =>
            if (c != -1) in.unread(c)
            return java.lang.Double.parseDouble(s)
        }
      }
      ???

    case c => throw new Json.Exception("digit or '-' expected, got " + printAsChar(c))
  }


  final def unpackNull(in: Reader): Null = getcNoSpace(in) match {
    case 'n' =>
      in.read() match { case 'u' => case c => throw new Json.Exception("'u' expected, got " + printAsChar(c)) }
      in.read() match { case 'l' => case c => throw new Json.Exception("'l' expected, got " + printAsChar(c)) }
      in.read() match { case 'l' => case c => throw new Json.Exception("'l' expected, got " + printAsChar(c)) }
      null

    case c => throw new Json.Exception("'n' expected, got " + printAsChar(c))
  }

  final def unpackBoolean(in: Reader): Boolean = getcNoSpace(in) match {
    case 't' =>
      in.read() match { case 'r' => case c => throw new Json.Exception("'r' expected, got " + printAsChar(c)) }
      in.read() match { case 'u' => case c => throw new Json.Exception("'u' expected, got " + printAsChar(c)) }
      in.read() match { case 'e' => case c => throw new Json.Exception("'e' expected, got " + printAsChar(c)) }
      true

    case 'f' =>
      in.read() match { case 'a' => case c => throw new Json.Exception("'a' expected, got " + printAsChar(c)) }
      in.read() match { case 'l' => case c => throw new Json.Exception("'l' expected, got " + printAsChar(c)) }
      in.read() match { case 's' => case c => throw new Json.Exception("'s' expected, got " + printAsChar(c)) }
      in.read() match { case 'e' => case c => throw new Json.Exception("'e' expected, got " + printAsChar(c)) }
      false

    case c => throw new Json.Exception("'t' or 'f' expected, got " + printAsChar(c))
  }


  final def unpackAnyVal(in: PushbackReader): AnyVal = getcNoSpace(in) match {
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-') =>
      in.unread(c)
      unpackDouble(in)

    case c@('t' | 'f') =>
      in.unread(c)
      unpackBoolean(in)

    case c => throw new Json.Exception("digit or 't' or 'f' expected, got " + printAsChar(c))
  }

  final def unpackAnyRef(in: PushbackReader): AnyRef = {
    unpackAny(in).asInstanceOf[AnyRef]
  }

  final def unpackAny(in: PushbackReader): Any = getcNoSpace(in) match {
    case '{' =>
      val b = Map.newBuilder[Any, Any]
      getcNoSpace(in) match {
        case -1  => throw new Json.Exception("map content expected, got EOF")
        case '}' =>
        case c   => in.unread(c)
                    var n: Int = 0
                    do {
                      val k = unpackAny(in)
                      expect(in, ':')
                      b += k -> unpackAny(in)
                      n = getcNoSpace(in)
                    } while (n == ',')
                    if (n != '}') throw new Json.Exception("'}' or ',' expected, got " + printAsChar(n))
      }
      b.result

    case '[' => //array
      val b = Seq.newBuilder[Any]
      getcNoSpace(in) match {
        case -1  => throw new Json.Exception("array content expected, got EOF")
        case ']' =>
        case c   => in.unread(c)
                    var n: Int = 0
                    do {
                      b += unpackAny(in)
                      n = getcNoSpace(in)
                    } while (n == ',')
                    if (n != ']') throw new Json.Exception("']' or ',' expected, got " + printAsChar(n))
      }
      b.result

    case c@('"' | '\'') =>
      in.unread(c)
      unpackString(in)

    case 'n' =>
      in.unread('n')
      unpackNull(in)

    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-') =>
      in.unread(c)
      unpackDouble(in)

    case c@('t' | 'f') =>
      in.unread(c)
      unpackBoolean(in)

    case c => throw new Json.Exception("unexpected " + printAsChar(c))
  }


  final def unpack[A](input: String): A = macro unpackImpl[A]

  final def unpackImpl[A : c.WeakTypeTag](c: Context)(input: c.Expr[String]): c.Expr[A] = {
    import c.universe._
    // todo: avoid StringReader creation for simple types
    unpackStreamImpl(c)(reify {
      new StringReader(input.splice)
    })
  }

  final def unpackStream[A](input: Reader): A = macro unpackStreamImpl[A]

  final def unpackStreamImpl[A: c.WeakTypeTag](c: Context)(input: c.Expr[Reader]): c.Expr[A] = {
    import c.universe._
    import definitions._

    val JSON = q"_root_.ms.webmaster.macroserialization.Json"

    val in = newTermName(c.fresh("in$"))

    def distinctWith[T](lst: List[T], equ: (T, T) => Boolean): List[T] = lst match {
      case Nil                                   => Nil
      case hd::tl if tl exists (x => equ(x, hd)) => distinctWith(tl, equ)
      case hd::tl                                => hd :: distinctWith(tl, equ)
    }

    // "a.b.c" -> Select(Select(Ident("a"), "b"), "c")
    def nameAsTree(m: String): Tree =
      m.split("\\.this\\.") match {
        case Array(t, n) => n.split('.').foldLeft[Tree](This(newTypeName(t))) { Select(_, _) }
        case Array(n)    => n.split('.').foldLeft[Tree](null) {
          case (null, part  ) => Ident(newTermName(part))
          case (tre,  part  ) => Select(tre, newTermName(part))
        }
      }

    def getCompanionSymbol(s: Symbol): Symbol = s.companionSymbol match {
      case NoSymbol if s.owner.isMethod || s.owner.isTerm =>
        val tn = s.name.toTermName
        def findInContext(ctx: scala.tools.nsc.typechecker.Contexts#Context): Symbol =
          ctx.scope.asInstanceOf[Scope] find { sym => sym.isModule && sym.owner == s.owner && sym.name.toTermName == tn } match {
            case Some(sym)                => assert(!sym.isMethod); sym
            case None if ctx.outer != ctx => findInContext(ctx.outer)
            case None                     => NoSymbol
          }
        findInContext(c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.context)

      case sym: Symbol => sym
    }


    def unpackOption(readElt: c.Tree) = {
      q"""$JSON.expect($in, '[')
          $JSON.getcNoSpace($in) match {
            case -1  => throw new Json.Exception("array content expected, got EOF")
            case ']' => None
            case c   => $in.unread(c)
                        val x = Some($readElt)
                        $JSON.expect($in, ']')
                        x
          }"""
    }

    def unpackcode(tpe: c.Type): c.Tree = {
             if (tpe =:= NullTpe                                            ) { q"$JSON.unpackNull($in)"
      } else if (tpe =:= AnyTpe                                             ) { q"$JSON.unpackAny($in)"
      } else if (tpe =:= AnyValTpe                                          ) { q"$JSON.unpackAnyVal($in)"
      } else if (tpe =:= AnyRefTpe  || tpe =:= c.typeOf[java.lang.Object   ]) { q"$JSON.unpackAnyRef($in)"
    // todo: null is ok when unpacking java.lang.Byte
      } else if (tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]) { q"$JSON.unpackLong($in).toByte"
      } else if (tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]) { q"$JSON.unpackLong($in).toShort"
      } else if (tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character]) { q"$JSON.unpackLong($in).toChar"
      } else if (tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]) { q"$JSON.unpackLong($in).toInt"
      } else if (tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]) { q"$JSON.unpackLong($in)"
      } else if (tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]) { q"$JSON.unpackDouble($in).toFloat"
      } else if (tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]) { q"$JSON.unpackDouble($in)"
      } else if (tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) { q"$JSON.unpackBoolean($in)"
      } else if (tpe =:= c.typeOf[String]                                   ) { q"$JSON.unpackString($in)"
      } else if (tpe.baseClasses.exists(_.fullName.toString == "scala.collection.GenMapLike")) {
        val MethodType(List(arg), tpeValue) = tpe.member(newTermName("apply")).typeSignatureIn(tpe)
        val tpeKey = arg.typeSignatureIn(tpe)
        val TypeRef(_, _, typeArgs) = tpe
        q"""$JSON.expect($in, '{')
            val bldr = ${tpe.typeSymbol.companionSymbol}.newBuilder[..$typeArgs]
            $JSON.getcNoSpace($in) match {
              case -1  => throw new $JSON.Exception("map content expected, got EOF")
              case '}' =>
              case c   => $in.unread(c)
                          var n: Int = 0
                          do {
                            bldr += Tuple2[$tpeKey, $tpeValue](${unpackcode(tpeKey)}, {$JSON.expect($in, ':'); ${unpackcode(tpeValue)}})
                            n = $JSON.getcNoSpace($in)
                          } while (n == ',')
                          if (n != '}')
                            throw new $JSON.Exception("'}' or ',' expected, got " + $JSON.printAsChar(n))
            }
            bldr.result"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.Array") { // Array is final class
        val TypeRef(_, _, List(tpeElement)) = tpe
        q"""$JSON.expect($in, '[')
            val bldr = _root_.scala.Array.newBuilder[$tpeElement]
            $JSON.getcNoSpace($in) match {
              case -1  => throw new $JSON.Exception("array content expected, got EOF")
              case ']' =>
              case c   => $in.unread(c)
                          var n: Int = 0
                          do {
                            bldr += ${unpackcode(tpeElement)}
                            n = $JSON.getcNoSpace($in)
                          } while (n == ',')
                          if (n != ']')
                            throw new $JSON.Exception("']' or ',' expected, got " + $JSON.printAsChar(n))
            }
            bldr.result"""

      } else if (tpe.baseClasses.exists(_.fullName.toString == "scala.collection.Traversable")) {
        val NullaryMethodType(tpeElement) = tpe.member(newTermName("head")).typeSignatureIn(tpe)
        val TypeRef(_, _, typeArgs) = tpe
        q"""$JSON.expect($in, '[')
            val bldr = ${tpe.typeSymbol.companionSymbol}.newBuilder[..$typeArgs]
            $JSON.getcNoSpace($in) match {
              case -1  => throw new $JSON.Exception("array content expected, got EOF")
              case ']' =>
              case c   => $in.unread(c)
                          var n: Int = 0
                          do {
                            bldr += ${unpackcode(tpeElement)}
                            n = $JSON.getcNoSpace($in)
                          } while (n == ',')
                          if (n != ']')
                            throw new $JSON.Exception("']' or ',' expected, got " + $JSON.printAsChar(n))
            }
            bldr.result"""

      } else if (tpe.typeSymbol.fullName.toString == "scala.Option") { // Option is sealed class
        val TypeRef(_, _, List(tpeElement)) = tpe
        unpackOption(unpackcode(tpeElement))

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass) { // case class or tuple
        val getters: List[MethodSymbol] = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
        if (getters.zipWithIndex.forall{ case(sym, i) => sym.name.decoded == "_"+(i+1) }) { // tuple
          val unpackers = for((sym, i) <- getters.zipWithIndex; NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe))
                          yield i match { case 0 => unpackcode(tpeElement)
                                          case _ => q"$JSON.expect($in, ','); ${unpackcode(tpeElement)}" }
          q"""$JSON.expect($in, '[')
              val tmp = new $tpe(..$unpackers)
              $JSON.expect($in, ']')
              tmp"""

        } else { // case class
          val symCompanion: Symbol = getCompanionSymbol(tpe.typeSymbol)
          val tpeCompanion: Type   = symCompanion.typeSignature

          case class Param(name: Name, tpe: Type, default: Symbol) {
            val isOption = tpe.typeSymbol.fullName.toString == "scala.Option"
            val tpeElement = if (isOption) {
                               val TypeRef(_, _, List(tpeElement)) = tpe
                               tpeElement
                             } else tpe
            val canBeNull = !tpeElement.typeSymbol.asClass.isPrimitive && !tpeElement.typeSymbol.asClass.isDerivedValueClass
            val varname  = newTermName(name.encoded+"$var")
          }

          val params: List[Param] =
            for((sym, i) <- getters.zipWithIndex) yield {
              val default: Symbol = tpeCompanion.members find { member => member.isMethod &&
                                                                          member.name.decoded=="apply$default$" + (i+1) && // sometimes there is no "<init>$default$1"
                                                                          member.asMethod.paramss==Nil &&
                                                                          member.asMethod.returnType=:=sym.asMethod.returnType } getOrElse NoSymbol
              val NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe)
              Param(sym.name, tpeElement, default)
            }


          val paramTypes: List[Type] = distinctWith[Type](params.map(_.tpeElement), _ =:= _)

          q"""..${ paramTypes.zipWithIndex map { case (t,i) => q"def ${newTermName("unpack$"+i)}: $t = ${unpackcode(t)}" } : List[Tree] /* SI-6840 */ }

              val c = $JSON.getcNoSpace($in)
              if (c=='[') {
                val tmp = new ${tpe}(..${ params.zipWithIndex map { case (p,i) =>
                                            val readElt = Ident(newTermName("unpack$"+paramTypes.indexWhere(_ =:= p.tpeElement))) /*unpackcode(p.tpeElement)*/
                                            val tree = if (p.isOption) unpackOption(readElt) else readElt
                                            if (i==0) tree
                                            else q"$JSON.expect($in, ','); $tree" } : List[Tree] /* SI-6840 */ })
                $JSON.expect($in, ']')
                tmp
              } else if (c=='{') {
                ..${ params map { case p if p.canBeNull => q"var ${p.varname} :        ${p.tpeElement}  = null"
                                  case p                => q"var ${p.varname} : Option[${p.tpeElement}] = None"
                                } : List[Tree] /* SI-6840 */ }
                $JSON.getcNoSpace($in) match {
                  case -1  => throw new $JSON.Exception("map content expected, got EOF")
                  case '}' =>
                  case c   => $in.unread(c)
                              var r: Int = 0
                              def recur {
                                val s = $JSON.unpackString($in)
                                $JSON.expect($in, ':')
                                ${ params.foldLeft[Tree](q"$JSON.unpackAny($in)" /* skip unknown field */) {
                                   case (tree, p) if p.canBeNull =>
                                     q"""if (s == ${p.name.decoded})
                                           ${p.varname} = ${ newTermName("unpack$"+paramTypes.indexWhere(_ =:= p.tpeElement)) /*unpackcode(p.tpeElement)*/ }
                                         else
                                           $tree"""
                                   case (tree, p) =>
                                     q"""if (s == ${p.name.decoded}) {
                                           ${p.varname} = Some(${ newTermName("unpack$"+paramTypes.indexWhere(_ =:= p.tpeElement)) /*unpackcode(p.tpeElement)*/ })
                                         } else
                                           $tree"""
                                 }}
                                r = $JSON.getcNoSpace($in)
                                if (r == ',') recur
                              }
                              recur
                              if (r != '}')
                                throw new $JSON.Exception("'}' or ',' expected, got " + $JSON.printAsChar(r))
                }
                new ${tpe}(
                  ..${ params map { case p if p.canBeNull && p.isOption          => q"""if (${p.varname}!=null) Some(${p.varname}) else None"""
                                    case p if p.canBeNull && p.default==NoSymbol => q"""if (${p.varname}!=null) ${p.varname}       else throw new $JSON.Exception(${s"missing \042${p.name}\042"})"""
                                    case p if p.canBeNull                        => q"""if (${p.varname}!=null) ${p.varname}       else ${nameAsTree(tpe.toString)}.${p.default.asTerm.name}"""
                                    case p if                p.isOption          => q"""                        ${p.varname}"""
                                    case p if                p.default==NoSymbol => q"""                        ${p.varname} .getOrElse(throw new $JSON.Exception(${s"missing \042${p.name}\042"}))"""
                                    case p                                       => q"""                        ${p.varname} .getOrElse(${nameAsTree(tpe.toString)}.${p.default.asTerm.name})"""
                                  } : List[Tree] /* SI-6840 */ }
                )
              } else if (c == -1) {
                throw new $JSON.Exception("'{' or '[' expected, got EOF")
              } else {
                ${ (if (params.length==1) {  // constructor has one and only param (it is known at compile time)
                      q"""$in.unread(c)
                          new ${tpe}( ${ { val readElt = Ident(newTermName("unpack$"+paramTypes.indexWhere(_ =:= params(0).tpeElement)))
                                           if (params(0).isOption) unpackOption(readElt) else readElt } : Tree /* SI-6840 */
                                       } )"""
                     } else {
                       q"""throw new $JSON.Exception(${"cannot unpack "+tpe})"""
                     }) : Tree /* SI-6840 */
                 }
              }"""
        }

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isDerivedValueClass) { // value class; unpack as element type
        val List(getter) = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
        val NullaryMethodType(tpeElement) = getter.typeSignatureIn(tpe)
        q"""new $tpe(${unpackcode(tpeElement)})"""

      } else {
        // can refer to local Json
        q"implicitly[Json.AuxUnpacker[$tpe]].unpackStream($in)"

      }

    }

    val rc = c.Expr[A](q"""val $in = ${input.tree} match {
                                        case w: _root_.java.io.PushbackReader => w
                                        case w                                => new _root_.java.io.PushbackReader(w, 1)
                                     }
                           ${unpackcode(c.weakTypeOf[A])}""")

    //println(s"// unpacker ${c.weakTypeOf[A]}\n" + rc)
    rc
  }
}

package autoconfig

import scala.reflect._
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

object EitherHelper {
  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = s collectFirst { case Left(l) => Left(l) } getOrElse Right(s collect { case Right(x) => x })
}
import EitherHelper._

class ConfigTranslator[C <: Context](val c: C) {
  import c.universe._

  case class ValueInfo(name: TermName, tpe: Type)

  case class Options(section: Option[String])

  private val configVar = TermName(c.fresh("config"))

  private val options: Options = c.prefix.tree match {
    case Apply(_, args) => args match {
      case Ident(TermName(section)) :: Nil => Options(section = Option(section))
      case Nil => Options(section = None)
      case _ => c.abort(c.enclosingPosition, "invalid argument, expected: section")
    }
    case _ => c.abort(c.enclosingPosition, "unexpected invocation")
  }

  def constructorValues(tpe: Type): List[List[ValueInfo]] = {
    val symbol = tpe.declaration(nme.CONSTRUCTOR)
    symbol.typeSignature.paramLists.map(_.map(param => ValueInfo(param.name.toString, param.typeSignature))).toList
  }

  //TODO: check for null|NoType
  def treeToType(tree: Tree): Type = {
    c.typecheck(tq"$tree", c.TYPEmode).tpe
  }

  def generateImplementation(section: Option[String], value: ValueInfo, optional: Boolean): Either[String, Tree] = {
    val key = section.map(_ + ".").getOrElse("") + value.name
    def maybeImpl(tree: Tree) = if (optional) {
      q"""
        $configVar.hasPath($key) match {
          case true => Option($tree)
          case false => None
        }
      """
    } else tree

    value.tpe match {
      case tpe if tpe =:= typeOf[Int] => Right(maybeImpl(q"$configVar.getInt($key)"))
      case tpe if tpe =:= typeOf[Long] => Right(maybeImpl(q"$configVar.getLong($key)"))
      case tpe if tpe =:= typeOf[Boolean] => Right(maybeImpl(q"$configVar.getBoolean($key)"))
      case tpe if tpe =:= typeOf[String] => Right(maybeImpl(q"$configVar.getString($key)"))
      case tpe if tpe =:= typeOf[List[Int]] => Right(maybeImpl(q"$configVar.getIntList($key).asScala.map(_.intValue).toList"))
      case tpe if tpe =:= typeOf[List[Long]] => Right(maybeImpl(q"$configVar.getLongList($key).asScala.map(_.longValue).toList"))
      case tpe if tpe =:= typeOf[List[Boolean]] => Right(maybeImpl(q"$configVar.getBooleanList($key).asScala.map(_.booleanValue).toList"))
      case tpe if tpe =:= typeOf[List[String]] => Right(maybeImpl(q"$configVar.getStringList($key).asScala.toList"))
      //TODO case duration? eg 10ms, 4h
      //TODO case bytes?
      //TODO case memorylimit?
      case tpe if tpe <:< typeOf[Option[Any]] =>
        val tpeArg = value.tpe.typeArgs.head
        generateImplementation(section, ValueInfo(value.name, tpeArg), true).right.map(maybeImpl _)
      case tpe if tpe.typeSymbol.isClass =>
        sequence(constructorValues(value.tpe).map(list => sequence(list.map { value =>
          generateImplementation(Option(key), value, optional)
        }))).right.map { args =>
          if (optional) {
            val argNames = args.zipWithIndex.map { case (args, i) =>
              args.zipWithIndex.map { case (arg, j) =>
                TermName("arg" + i + "_" + j)
              }
            }
            val assocs = args.zip(argNames).flatMap { case (args, names) =>
              args.zip(names).map { case (arg, name) =>
                fq"$name <- $arg"
              }
            }
            q"for(..$assocs) yield new ${value.tpe}(...$argNames)"
          } else q"new ${value.tpe}(...$args)"
        }
      case t => Left(s"cannot read type '${value.tpe}' from config")
    }
  }

  def generateMember(section: Option[String], value: ValueInfo): Either[String, Tree] =
    generateImplementation(section, value, false).right.map { getter =>
      println(getter)
      q"val ${value.name}: ${value.tpe} = $getter"
    }

  def generateMembers(body: List[Tree]): Either[String, List[Tree]] = {
    val values = body collect {
      case q"val $name: $tpt" => ValueInfo(name, treeToType(tpt))
    }

    sequence(values.map(generateMember(options.section, _))).right.map { members =>
      val init = options.section match {
        case Some(section) =>
          q"val $configVar = com.typesafe.config.ConfigFactory.load.getConfig($section)"
        case None =>
          q"val $configVar = com.typesafe.config.ConfigFactory.load"
      }

      val imports =
        q"import scala.collection.JavaConverters._" ::
        Nil

      init :: (imports ++ members)
    }
  }

  def translate(obj: ModuleDef): Either[String, Tree] = {
    val members = generateMembers(obj.impl.body)
    members.right.map { members =>
      q"object ${obj.name} { ..$members }"
    }
  }
}
object ConfigTranslator {
  def apply(c: Context): ConfigTranslator[c.type] = new ConfigTranslator(c)
}

object ConfigMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val translator = ConfigTranslator(c)
    annottees.map(_.tree) match {
      case (obj: ModuleDef) :: Nil =>
        translator.translate(obj) match {
          case Left(err) => c.abort(c.enclosingPosition, s"error in translation: $err")
          case Right(translated) => c.Expr[Any](translated)
        }
      case _ =>
        c.abort(c.enclosingPosition, "config can only annotate objects")
    }
  }
}

@compileTimeOnly("only for compile time expansion")
class config extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ConfigMacro.impl
}

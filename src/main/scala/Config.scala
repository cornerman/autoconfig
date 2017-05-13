package autoconfig

import scala.reflect._
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import java.time.Duration
import com.typesafe.config.{Config, ConfigMemorySize}

object EitherHelper {
  def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] = s collectFirst { case Left(l) => Left(l) } getOrElse Right(s collect { case Right(x) => x })
}
import EitherHelper._

class ConfigTranslator[C <: Context](val c: C) {
  import c.universe._

  case class ValueInfo(name: TermName, tpe: Type)

  case class Options(section: Option[Tree], flatTypes: Set[Type])

  private val configVar = TermName(c.freshName("config"))

  private val options: Options = c.prefix.tree match {
    case Apply(_, args) =>
      val section: Seq[Tree] = args.collect {
        case q"section = $section" => section
      }
      val flatTypes: Seq[Set[Type]] = args.collect {
        case q"flatTypes = Set(..$names)" =>
          val typeNames = names.map(name => tq"${TypeName(name.toString)}")
          typeNames.map(treeToType).toSet
      }

      val validArgs = section.size + flatTypes.size
      if (validArgs > 2 || validArgs < args.size)
        c.abort(c.enclosingPosition, "invalid argument, expected: section = <name>, flatTypes = <set>")

      Options(section.lastOption, flatTypes.lastOption.toSet.flatten)
    case _ => c.abort(c.enclosingPosition, "unexpected invocation")
  }

  def constructorValues(tpe: Type): List[List[ValueInfo]] = {
    val symbol = tpe.decl(termNames.CONSTRUCTOR)
    symbol.typeSignatureIn(tpe).paramLists.map(_.map { param =>
      ValueInfo(TermName(param.name.toString), param.typeSignature)
    }).toList
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
      case tpe if tpe =:= typeOf[Double] => Right(maybeImpl(q"$configVar.getDouble($key)"))
      case tpe if tpe =:= typeOf[Boolean] => Right(maybeImpl(q"$configVar.getBoolean($key)"))
      case tpe if tpe =:= typeOf[String] => Right(maybeImpl(q"$configVar.getString($key)"))
      case tpe if tpe =:= typeOf[Config] => Right(maybeImpl(q"$configVar.getConfig($key)"))
      case tpe if tpe =:= typeOf[ConfigMemorySize] => Right(maybeImpl(q"$configVar.getMemorySize($key)"))
      case tpe if tpe =:= typeOf[Duration] => Right(maybeImpl(q"$configVar.getDuration($key)"))
      case tpe if tpe =:= typeOf[List[Int]] => Right(maybeImpl(q"$configVar.getIntList($key).asScala.map(_.intValue).toList"))
      case tpe if tpe =:= typeOf[List[Long]] => Right(maybeImpl(q"$configVar.getLongList($key).asScala.map(_.longValue).toList"))
      case tpe if tpe =:= typeOf[List[Double]] => Right(maybeImpl(q"$configVar.getDoubleList($key).asScala.map(_.doubleValue).toList"))
      case tpe if tpe =:= typeOf[List[Boolean]] => Right(maybeImpl(q"$configVar.getBooleanList($key).asScala.map(_.booleanValue).toList"))
      case tpe if tpe =:= typeOf[List[String]] => Right(maybeImpl(q"$configVar.getStringList($key).asScala.toList"))
      case tpe if tpe =:= typeOf[List[Config]] => Right(maybeImpl(q"$configVar.getConfigList($key).asScala.toList"))
      case tpe if tpe =:= typeOf[List[ConfigMemorySize]] => Right(maybeImpl(q"$configVar.getMemorySizeList($key).asScala.toList"))
      case tpe if tpe =:= typeOf[List[Duration]] => Right(maybeImpl(q"$configVar.getDurationList($key).asScala.toList"))
      case tpe if tpe <:< typeOf[Option[Any]] =>
        val tpeArg = value.tpe.typeArgs.head
        generateImplementation(section, ValueInfo(value.name, tpeArg), true).right.map(maybeImpl _)
      case tpe if tpe.typeSymbol.isClass =>
        val ctorValues = constructorValues(value.tpe)
        val isFlat = options.flatTypes.find(_ =:= value.tpe).isDefined || (tpe <:< typeOf[AnyVal])
        val impl = isFlat match {
          case true if ctorValues.size == 1 && ctorValues.head.size == 1 =>
            val innerValue = ctorValues.head.head.copy(name = value.name)
            generateImplementation(section, innerValue, optional).right.map { impl => Seq(Seq(impl)) }
          case true =>
            Left("flat types need to have exactly one constructor")
          case false =>
            val valueImpls = ctorValues.map(_.map { value =>
              generateImplementation(Option(key), value, optional)
            })
            sequence(valueImpls.map(sequence _))
        }

        impl.right.map { args =>
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
      q"val ${value.name}: ${value.tpe} = $getter"
    }

  def generateMembers(name: TermName, body: List[Tree]): Either[String, List[Tree]] = {
    val values = body collect {
      case q"val $name: $tpt" => ValueInfo(name, treeToType(tpt))
    }

    sequence(values.map(generateMember(None, _))).right.map { members =>
      val rootConfigVar = TermName(c.freshName("config"))
      val args = values.map(_.name)

      q"val $rootConfigVar = com.typesafe.config.ConfigFactory.load" ::
      (options.section match {
        case Some(section) => q"val $configVar = $rootConfigVar.getConfig($section)"
        case None => q"val $configVar = $rootConfigVar"
      }) ::
      // q"val $configVar = com.typesafe.config.ConfigFactory.load" ::
      q"""override def toString = ${name.toString}.+((..$args))""" ::
      q"import scala.collection.JavaConverters._" ::
      members.toList
    }
  }

  def translate(obj: ModuleDef): Either[String, Tree] = {
    val members = generateMembers(obj.name, obj.impl.body)
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

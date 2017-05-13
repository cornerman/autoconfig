package autoconfig

import macroni._

class ConfigTranslatorSpec extends TreeSpec with ContextMock {
  import scala.reflect.macros.whitebox

  val context = mockContext[whitebox.Context]
  import context.universe._

  context.freshName("config") returns "config"

  context.prefix returns {
    val exprMock = mock[context.Expr[context.PrefixType]]
    exprMock.tree returns q"new config(section = bier)"
    exprMock
  }

  val translator = ConfigTranslator(context)
  import translator._

  "constructor values" >> {
    constructorValues(typeOf[Some[Int]]) must beEqualTo(List(List(ValueInfo(TermName("x"), typeOf[Int]))))
  }
}

class ConfigSpec extends CompileSpec with ContextMock {
  import scala.reflect.runtime.universe._

  "section" >> {
    q"""
      @autoconfig.config(section = urmel) object Config {
        val id: Long
      }""" must compile.to(containTree(
        q"""Config.this.config$$macro$$1.getLong("urmel.id")"""
      ))
  }

  "only primitives" >> {
    q"""
      @autoconfig.config object Config {
        val id: Long
        val str: String
        val boo: Boolean
        val i: Int
        val d: Double
      }""" must compile.to(containTree(
        q"""Config.this.config$$macro$$1.getLong("id")""",
        q"""Config.this.config$$macro$$1.getString("str")""",
        q"""Config.this.config$$macro$$1.getBoolean("boo")""",
        q"""Config.this.config$$macro$$1.getInt("i")""",
        q"""Config.this.config$$macro$$1.getDouble("d")"""
      ))
  }

  "class with constructor" >> {
    q"""
      @autoconfig.config object Config {
        val obj: (Int, String)
      }""" must compile.to(containTree(
        q"""Config.this.config$$macro$$1.getInt("obj._1")""",
        q"""Config.this.config$$macro$$1.getString("obj._2")"""
      ))
  }
}

import macroni.CompileSpec
import scala.reflect.runtime.universe._

class ConfigSpec extends CompileSpec {
  // "only primitives" >> {
  //   q"""
  //     import configobjects.config
  //     @config object Config {
  //       val id: Long
  //       val str: String
  //       val boo: Boolean
  //       val i: Int
  //     }""" must compile.to(
  //       q"""
  //         class A(val inner: Tret) extends Tret {
  //           override def a(value: Int) = A.this.inner.a(value)
  //         }"""
  //     )
  // }

  // "method with one parameter" >> {
  //   q"""
  //     import configobjects.config
  //     case class Something(i: Int, b: Boolean)
  //     @config object Config {
  //       val id: Long
  //       val str: String
  //       val obj: Something
  //     }""" must compile.to(
  //       q"""
  //         class A(val inner: Tret) extends Tret {
  //           override def a(value: Int) = A.this.inner.a(value)
  //         }"""
  //     )
  // }
}

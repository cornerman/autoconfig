package autoconfig

import org.specs2.mutable.Specification

case class SmtpConfig(endpoint: String)//, username: String, password: String)
case class UsergroupConfig(publicId: Long)
case class AuthConfig(enableImplicit: Boolean, tokenLifetime: Long, secret: String)
case class EmailConfig(fromAddress: String, smtp: SmtpConfig)

class ExampleSpec extends Specification {
  @config(wust) object Config {
    val id: Long
    val str: String
    val boo: Boolean
    val i: Int
    val ido: Option[Long]
    val stro: Option[String]
    val booo: Option[Boolean]
    val io: Option[Int]
    val ids: List[Long]
    val strs: List[String]
    val boos: List[Boolean]
    val is: List[Int]
    val usergroup: UsergroupConfig
    val auth: AuthConfig
    val email: Option[EmailConfig]
  }
}

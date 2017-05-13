package autoconfig

import org.specs2.mutable.Specification
import com.typesafe.config.Config

class Id(val id: Long) extends AnyVal
case class SmtpConfig(endpoint: String)//, username: String, password: String)
case class UsergroupConfig(publicId: Id)
case class AuthConfig(enableImplicit: Boolean, tokenLifetime: Long, secret: String)
case class EmailConfig(fromAddress: String, smtp: SmtpConfig)

class ExampleSpec extends Specification {
  @config(section = "wust") object Config {
    val conf: Config
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

  @config object MinConfig {
    val v: Option[Long]
    val w: Option[Long]
  }

  "load config value" >> {
    MinConfig.v mustEqual None
  }

  "config toString" >> {
    MinConfig.toString mustEqual "MinConfig(None,None)"
  }
}

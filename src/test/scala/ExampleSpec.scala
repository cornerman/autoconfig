package autoconfig

import org.specs2.mutable.Specification
import com.typesafe.config
import java.time.Duration

case class UserGroup(id: Long) extends AnyVal
case class SmtpConfig(endpoint: String, username: String, password: String)
case class AuthConfig(tokenLifetime: Duration, secret: String)
class EmailConfig(fromAddress: String, smtp: SmtpConfig)

@config(section = "wust") object Config {
  val usergroup: UserGroup
  val auth: AuthConfig
  val email: Option[EmailConfig]

  def hasMail = email.isDefined
}

@config object SomeConfig {
  val conf: config.Config
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
}

class ExampleSpec extends Specification {

  "load config value" >> {
    Config.usergroup mustEqual UserGroup(1)
    Config.auth mustEqual AuthConfig(Duration.ofHours(24), "secret")
    Config.email mustEqual None
  }

  "existing methods" >> {
    Config.hasMail mustEqual false
  }

  "config toString" >> {
    Config.toString mustEqual "Config(section = wust)(usergroup = UserGroup(1),auth = AuthConfig(PT24H,secret),email = None)"
  }
}

# autoconfig

generate config objects for typesafe.config in scala

# usage

Dependency in build.sbt:
```
resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies += "com.github.cornerman" %% "autoconfig" % "0.1.0-SNAPSHOT"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

# example

Code:

```scala
import autoconfig.config
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
```

application.conf:

```
wust {
    usergroup = 1
    auth {
        tokenLifetime = 24h
        secret = ${SECRET}
    }

    email {
        fromAddress = ${?EMAIL_ADDRESS}
        smtp {
            username = ${?SMTP_USER}
            password = ${?SMTP_PASS}
            endpoint = ${?SMTP_ENDPOINT}
        }
    }
}
```

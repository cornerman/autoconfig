# config-objects

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

case class SmtpConfig(endpoint: String, username: String, password: String)
case class AuthConfig(tokenLifetime: Long, secret: String)
case class EmailConfig(fromAddress: String, smtp: SmtpConfig)

@config(wust) object Config {
  val usergroup: Long
  val auth: AuthConfig
  val email: Option[EmailConfig]
}
```

application.conf:

```
wust {
    usergroup = 1
    auth {
        tokenLifetimeSeconds = 86400
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

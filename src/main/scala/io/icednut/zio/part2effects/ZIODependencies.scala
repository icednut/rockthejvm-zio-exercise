package io.icednut.zio.part2effects

import io.icednut.zio.part2effects.ZIODependencies.{EmailService, UserDatabase}
import zio.*

object ZIODependencies extends ZIOAppDefault {

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] =
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase): UserSubscription = {
      new UserSubscription(emailService, userDatabase)
    }
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}").unit
  }

  object EmailService {
    def create(): EmailService = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscriber(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool): UserDatabase =
      new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int): ConnectionPool =
      new ConnectionPool(nConnections)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] =
      ZIO.succeed(println(s"Executing query: $query"))
  }

  val subscriptionService: ZIO[Any, Nothing, UserSubscription] = ZIO.succeed( // Dependency Injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  )

  /*
    "clean DI" has drawbacks
    - does not scale for many services
    - DI can be 100x worse
      - pass dependencies partially
      - not having all deps in the same place
      - passing dependencies multiple times
   */

  def subscribe(user: User) = for {
    sub <- subscriptionService
    _ <- sub.subscribeUser(user)
  } yield ()

  def run = subscribe(User("Will", "will.lee@example.com"))
}

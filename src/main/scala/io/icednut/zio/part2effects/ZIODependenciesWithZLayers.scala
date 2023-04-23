package io.icednut.zio.part2effects

import io.icednut.zio.part2effects.ZIODependencies.{EmailService, UserDatabase}
import zio.*

object ZIODependenciesWithZLayers extends ZIOAppDefault {

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

    val live: ZLayer[EmailService & UserDatabase, Nothing, UserSubscription] =
      ZLayer.fromFunction(create _)
  }

  class EmailService {
    def email(user: User): Task[Unit] =
      ZIO.succeed(println(s"You've just been subscribed to Rock the JVM. Welcome, ${user.name}"))
  }

  object EmailService {
    def create(): EmailService = new EmailService

    val live: ZLayer[Any, Nothing, EmailService] =
      ZLayer.succeed(create())
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

    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] =
      ZLayer.fromFunction(create _)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnections: Int): ConnectionPool =
      new ConnectionPool(nConnections)

    def live(nConnection: Int): ZLayer[Any, Nothing, ConnectionPool] =
      ZLayer.succeed(create(nConnection))
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

  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call
    _ <- sub.subscribeUser(user)
  } yield ()

  // risk leaking resources if you subscribe multiple users in the same program
  val program = for {
    // you might lose track of the effect which created the servcies in the first place
    // and you might oblivious to the fact that the subscribe API actually spins up a new subscription service in the first place
    _ <- subscribe(User("Tom", "tom@example.com"))
    _ <- subscribe(User("John", "john@example.com"))
  } yield ()

  // If you wanna mitigate those you can alternatively pass this subscription service as an argument to subscribe to force the user to pass
  // the right effect but that becomes annoying
  // Because you subscribe with a user and the subscription service and you have to pass that argument over and over again.
  // If you have to insert multiple users so pretty much all options are bad now.

  // alternative
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Tom", "tom@example.com"))
    _ <- subscribe_v2(User("John", "john@example.com"))
  } yield ()

  //  def run = subscribe(User("Will", "will.lee@example.com"))


  /*
    - we don't need to care about dependencies until the end of the world
    - all ZIOs requiring this dependency will use the same instance
    - can use different instances of the same type for different needs (e.g. testing)
    - layers can be created and composed much like regular ZIOs + rich API
   */

  /**
   * ZLayer
   */
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  // a layer that requires a dependency (higher layer) can be built with ZLayer.fromFunction
  // (and automatically fetch the function arguments and place them into the ZLayer's dependency/environment type argument)
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] =
  ZLayer.fromFunction(UserDatabase.create(_))
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] =
    ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[UserDatabase & EmailService, Nothing, UserSubscription] =
    ZLayer.fromFunction(UserSubscription.create)

  // composing layers
  // this is a code vertical composition >>>
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer
  // horizontal composition: combines the dependencies of both layers AND the values of both layers
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase & EmailService] = databaseLayerFull ++ emailServiceLayer
  // mix & match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  // best practice: write "factory" methods exposing layers in the companion objects of the services
  val runnableProgram = program_v2.provide(userSubscriptionLayer)

  // magic: dependency auto wire magically
  val runnableProgram_v2 = program_v2.provide(
    UserDatabase.live,
    UserSubscription.live,
    EmailService.live,
    ConnectionPool.live(10),

    // ZIO will tell you if you're missing a layer
    // and if you have multiple layers of the same type

//    ZLayer.Debug.tree
    ZLayer.Debug.mermaid
  )

  // magic_v2
  val userSubscription_v2 = ZLayer.make[UserSubscription](
    UserDatabase.live,
    UserSubscription.live,
    EmailService.live,
    ConnectionPool.live(10),
    ZLayer.Debug.mermaid
  )

  // passthrough
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool & UserDatabase] =
    UserDatabase.live.passthrough
  // service = take a dep and expose it as a value to further layers
  val dbService: ZLayer[UserDatabase, Nothing, UserDatabase] = ZLayer.service[UserDatabase]
  // launch = creates a ZIO that uses the services and never finishes (e.g. web server)
  val subscriptionLaunch: ZIO[EmailService & UserDatabase, Nothing, Nothing] = UserSubscription.live.launch

  def run = runnableProgram_v2
}

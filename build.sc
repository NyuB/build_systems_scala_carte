import mill._, scalalib._

object Versions {
    val scala = "3.5.0"
    val munit = "1.0.0"
}

trait SharedConfiguration extends ScalaModule {
    override def scalaVersion: T[String] = Versions.scala
    override def scalacOptions: T[Seq[String]] =
        Seq(
          "-deprecation",
          "-Werror",
          "-Wimplausible-patterns",
          "-Wnonunit-statement",
          "-WunstableInlineAccessors",
          "-Wunused:all",
          "-Wvalue-discard"
        )

    trait Tests extends ScalaTests with TestModule.Munit {
        override def ivyDeps = super.ivyDeps() ++ Agg(
          ivy"org.scalameta::munit:${Versions.munit}",
          ivy"org.scalameta::munit-scalacheck:${Versions.munit}"
        )

    }

}

object build_systems_a_la_carte extends ScalaModule with SharedConfiguration {
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.scalameta::munit:${Versions.munit}"
    )

    object test extends Tests
}

object makette extends ScalaModule with SharedConfiguration {
    override def moduleDeps = Seq(build_systems_a_la_carte, hashette)
    object test extends Tests
}

object hashette extends ScalaModule with SharedConfiguration {
    override def moduleDeps = Seq(build_systems_a_la_carte)
    object test extends Tests
}

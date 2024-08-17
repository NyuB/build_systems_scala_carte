package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object Example_3_5:
    val sprsh2: Tasks[Monad, String, Int] =
        case "B1" =>
            {
                [F[_]] =>
                    (_: Monad[F]) ?=>
                        (fetch: String => F[Int]) =>
                            fetch("C1").flatMap: c1 =>
                                if c1 == 1 then fetch("B2") else fetch("A2")
            }.some
        case "B2" =>
            {
                [F[_]] =>
                    (_: Monad[F]) ?=>
                        (fetch: String => F[Int]) =>
                            fetch("C1").flatMap: c1 =>
                                if c1 != 1 then fetch("B1") else fetch("A1")
            }.some
        case _ => None

    extension [T](t: T) private def some: Option[T] = Some(t)
end Example_3_5

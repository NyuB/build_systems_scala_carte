package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object Example_3_2:
    val sprsh1: Tasks[Applicative, String, Int] =
        case "B1" =>
            {
                [F[_]] =>
                    (_: Applicative[F]) ?=>
                        (fetch: String => F[Int]) =>
                            (+) `<$>` fetch("A1") <*> (fetch("A2"))
            }.some
        case "B2" =>
            {
                [F[_]] =>
                    (_: Applicative[F]) ?=>
                        (fetch: String => F[Int]) => *(2) `<$>` fetch("B1")
            }.some
        case _ => None

    private def +(a: Int)(b: Int) = a + b
    private def *(a: Int)(b: Int) = a * b
    extension [T](t: T) private def some: Option[T] = Some(t)

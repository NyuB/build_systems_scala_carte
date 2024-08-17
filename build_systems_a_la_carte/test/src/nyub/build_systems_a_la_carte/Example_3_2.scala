package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object Example_3_2:
    val sprsh1: Tasks[Applicative, String, Int] =
        case "B1" => Some(taskB1)
        case "B2" => Some(taskB2)
        case _    => None

    val taskB1 =
        [F[_]] =>
            (_: Applicative[F]) ?=>
                (fetch: String => F[Int]) =>
                    (+) `<$>` fetch("A1") <*> fetch("A2")

    val taskB2 =
        [F[_]] =>
            (_: Applicative[F]) ?=>
                (fetch: String => F[Int]) => *(2) `<$>` fetch("B1")

    private def +(a: Int)(b: Int) = a + b
    private def *(a: Int)(b: Int) = a * b
end Example_3_2

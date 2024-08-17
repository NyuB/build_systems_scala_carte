package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object Example_3_5:
    val sprsh2: Tasks[Monad, String, Int] =
        case "B1" => Some(taskB1)
        case "B2" => Some(taskB2)
        case _    => None

    val taskB1 =
        [F[_]] =>
            (_: Monad[F]) ?=>
                (fetch: String => F[Int]) =>
                    fetch("C1").flatMap: c1 =>
                        if c1 == 1 then fetch("B2") else fetch("A2")

    val taskB2 =
        [F[_]] =>
            (_: Monad[F]) ?=>
                (fetch: String => F[Int]) =>
                    fetch("C1").flatMap: c1 =>
                        if c1 != 1 then fetch("B1") else fetch("A1")

end Example_3_5

package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

object Example_3_5:
    val sprsh2: Tasks[Monad, String, Int] =
        case "B1" => Some(taskB1)
        case "B2" => Some(taskB2)
        case _    => None

    /** if C1 then B2 else A2 */
    val taskB1: Task[Monad, String, Int] =
        [F[_]] =>
            monad ?=>
                fetch =>
                    fetch("C1").flatMap: c1 =>
                        if c1 == 1 then fetch("B2") else fetch("A2")

    /** if C1 then A1 else B1 */
    val taskB2: Task[Monad, String, Int] =
        [F[_]] =>
            monad ?=>
                fetch =>
                    fetch("C1").flatMap: c1 =>
                        if c1 != 1 then fetch("B1") else fetch("A1")

end Example_3_5

package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task

object Example_3_2:
    val sprsh1: Tasks[Applicative, String, Int] =
        case "B1" => Some(taskB1)
        case "B2" => Some(taskB2)
        case _    => None

    val taskB1: Task[Applicative, String, Int] =
        [F[_]] => applicative ?=> fetch => (+) `<$>` fetch("A1") <*> fetch("A2")

    val taskB2: Task[Applicative, String, Int] =
        [F[_]] => applicative ?=> fetch => *(2) `<$>` fetch("B1")

    /** B2 expressed as B1 + B1 instead of B1 * 2 to exhibit non-minimal build
      * systems
      */
    val taskB2_nonMinimal: Task[Applicative, String, Int] =
        [F[_]] => applicative ?=> fetch => (+) `<$>` fetch("B1") <*> fetch("B1")

    private def +(a: Int)(b: Int) = a + b
    private def *(a: Int)(b: Int) = a * b
end Example_3_2

package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

class TaskSuite extends munit.FunSuite:

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

        def fetch(key: String): Option[Int] = key match
            // A1 and A2 are 'excel inputs'
            case "A1" => Some(1)
            case "A2" => Some(2)
            // If a cell is not an input, use tasks definitions to find the relevant task and run it to compute the cell
            case notAnInput =>
                sprsh1(notAnInput).flatMap(task => task(fetch))

        private def +(a: Int)(b: Int) = a + b
        private def *(a: Int)(b: Int) = a * b

    test("Example 3.2 - A1"):
        assertEquals(Example_3_2.fetch("A1"), Some(1))

    test("Example 3.2 - A2"):
        assertEquals(Example_3_2.fetch("A2"), Some(2))

    test("Example 3.2 - B1"):
        assertEquals(Example_3_2.fetch("B1"), Some(1 + 2))

    test("Example 3.2 - B2"):
        assertEquals(Example_3_2.fetch("B2"), Some(2 * (1 + 2)))

    given Applicative[Option] with
        override def pure[A](a: A): Option[A] = Some(a)
        extension [A, B](ff: Option[A => B])
            override def ap(fa: Option[A]): Option[B] = ff.flatMap(fa.map(_))

    extension [T](t: T) private def some: Option[T] = Some(t)

end TaskSuite

package nyub.build_systems_a_la_carte

class TaskSuite extends munit.FunSuite:
    test("Example 3.2 - A1"):
        assertEquals(fetch_3_2("A1"), Some(A1))

    test("Example 3.2 - A2"):
        assertEquals(fetch_3_2("A2"), Some(A2))

    test("Example 3.2 - B1"):
        assertEquals(fetch_3_2("B1"), Some(A1 + A2))

    test("Example 3.2 - B2"):
        assertEquals(fetch_3_2("B2"), Some(2 * (A1 + A2)))

    test("Example 3.5 - B1"):
        assertEquals(
          fetch_3_5(1, "B1"),
          Some(A1)
        ) // (C1  =  1) => (B1 = B2 = A1 = 1)
        assertEquals(
          fetch_3_5(0, "B1"),
          Some(A2)
        ) // (C1 =/= 1) => (B1 = A2 = 2)

    test("Example 3.5 - B2"):
        assertEquals(
          fetch_3_5(1, "B2"),
          Some(A1)
        ) // (C1  =  1) => (B2 = A1 = 1)
        assertEquals(
          fetch_3_5(0, "B2"),
          Some(A2)
        ) // (C1 =/= 1) => (B2 = B1 = A2 = 2)

    def fetch_3_2(key: String): Option[Int] = key match
        // A1 and A2 are 'excel inputs'
        case "A1" => Some(A1)
        case "A2" => Some(A2)
        // If a cell is not an input, use tasks definitions to find the relevant task and run it to compute the cell
        case notAnInput =>
            Example_3_2.sprsh1(notAnInput).flatMap(task => task(fetch_3_2))

    def fetch_3_5(c1: Int, key: String): Option[Int] = key match
        case "A1" => Some(1)
        case "A2" => Some(2)
        case "C1" => Some(c1)
        case notAnInput =>
            Example_3_5
                .sprsh2(notAnInput)
                .flatMap(task => task(k => fetch_3_5(c1, k)))

    given Monad[Option] with
        extension [A](a: A) override def ret: Option[A] = Some(a)
        extension [A](
            fa: Option[A]
        ) override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

end TaskSuite

private val A1 = 1
private val A2 = 2

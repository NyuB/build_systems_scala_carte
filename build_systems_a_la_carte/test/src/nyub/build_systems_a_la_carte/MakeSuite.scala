package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Applicative
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.StoreModule
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

class MakeSuite extends munit.FunSuite:
    type Target = String
    type Content = String
    type MakeTask = Task[Applicative, Target, Content]
    type MakeTasks = Tasks[Applicative, Target, Content]

    import FileBuild.once
    import FileBuild.onceMore
    import FileBuild.times

    test("Nominal"):
        val setup = TestSetup()
        given setup.storeModule.type = setup.storeModule
        val buildSystem = make[Target, Content]
        val (info, result) =
            buildSystem.build(setup.tasks, "main.exe", setup.inputStore)

        setup.util_o `was called` once
        setup.main_o `was called` once
        setup.main_exe `was called` once
        assertEquals(result("util.h"), "UTIL_HEADER")
        assertEquals(
          result("main.exe"),
          "Built main.exe from util.o [Built util.o from util.h [UTIL_HEADER] and util.c [UTIL_SOURCE]] and main.o [Built main.o from util.h [UTIL_HEADER] and main.c [MAIN_SOURCE]]"
        )

        val (infoAgain, resultAgain) =
            buildSystem.build(setup.tasks, "main.exe", setup.inputStore.putInfo(info))
        
        // Not called any more
        setup.util_o `was called` once
        setup.main_o `was called` once
        setup.main_exe `was called` once
        assertEquals(
          result("main.exe"),
          "Built main.exe from util.o [Built util.o from util.h [UTIL_HEADER] and util.c [UTIL_SOURCE]] and main.o [Built main.o from util.h [UTIL_HEADER] and main.c [MAIN_SOURCE]]"
        )
        // No info updated
        assertEquals(info, infoAgain)

    class TestSetup:
        val util_o = UtilObject()
        val util_h = "UTIL_HEADER"
        val util_c = "UTIL_SOURCE"

        val main_o = MainObject()
        val main_exe = MainExe()
        val main_c = "MAIN_SOURCE"

        val storeModule = FunctionalStoreModule[MakeInfo[Target], Target, Content]
        given FunctionalStoreModule[MakeInfo[Target], Target, Content] = storeModule
        def inputs(k: Target): Content = k match
            case "util.h" => util_h
            case "util.c" => util_c
            case "main.c" => main_c
            case _        => "EMPTY"

        val inputStore = StoreModule.initialise(MakeInfo(0, Map.empty.withDefault(_ => 0)), inputs)
        def tasks: MakeTasks = k =>
            k match
                case "util.o"   => Some(util_o)
                case "main.o"   => Some(main_o)
                case "main.exe" => Some(main_exe)
                case _          => None

    class UtilObject extends MakeTask with FileBuild:
        override def run[F[_]](using constraints: Applicative[F])(fetch: Target => F[Content]): F[Content] =
            val dotH = fetch("util.h")
            val dotC = fetch("util.c")
            (dotH, dotC).map2(buildFromFiles("util.o")("util.h", "util.c"))

    class MainObject extends MakeTask with FileBuild:
        override def run[F[_]](using constraints: Applicative[F])(fetch: Target => F[Content]): F[Content] =
            val dotH = fetch("util.h")
            val dotC = fetch("main.c")
            (dotH, dotC).map2(buildFromFiles("main.o")("util.h", "main.c"))

    class MainExe extends MakeTask with FileBuild:
        override def run[F[_]](using constraints: Applicative[F])(fetch: Target => F[Content]): F[Content] =
            val util = fetch("util.o")
            val main = fetch("main.o")
            (util, main).map2(buildFromFiles("main.exe")("util.o", "main.o"))

    trait FileBuild:
        private def called: Unit = calledCount = calledCount.onceMore
        private var calledCount: FileBuild.Times = 0.times

        infix def `was called`(expected: FileBuild.Times) = assertEquals(calledCount, expected)
        def buildFromFiles(resultName: String)(firstName: String, secondName: String): (String, String) => String =
            (firstContent, secondContent) =>
                called
                s"Built $resultName from $firstName [$firstContent] and $secondName [$secondContent]"

    object FileBuild:
        opaque type Times = Int
        extension (i: Int)
            def times: Times =
                require(i >= 0)
                i

        extension (t: Times)
            def onceMore: Times =
                t + 1

        val once: Times = 1
        val never: Times = 0

end MakeSuite

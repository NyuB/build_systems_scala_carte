package nyub.makette

import java.nio.file.Files
import java.nio.file.Paths
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.monads.State.Monad.M
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.State
import nyub.build_systems_a_la_carte.monads.Applicative
import nyub.build_systems_a_la_carte.schedulers.TopologicalScheduler
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.BuildSystem
import nyub.build_systems_a_la_carte.stores.FunctionalStoreModule
import nyub.makette.TaskResult.Ok
import nyub.makette.TaskResult.Ko
import nyub.build_systems_a_la_carte.StoreModule
import nyub.build_systems_a_la_carte.monads.Monad
import java.nio.file.Path
import nyub.build_systems_a_la_carte.HashModule
import nyub.hashette.Hashette
import nyub.hashette.Hashette.Hash
import nyub.build_systems_a_la_carte.schedulers.FixOrderScheduler
import nyub.build_systems_a_la_carte.rebuilders.VTRebuilder
import nyub.build_systems_a_la_carte.rebuilders.VerifyingTrace

class MaketteTaskSuite extends munit.FunSuite:
    test("From java to jar, with class"):

        // GIVEN
        given FunctionalStoreModule[Unit, Key, TaskResult[ResultFolder]] = FunctionalStoreModule()
        val workspace = testWorkspace()

        val tasks = Map[Key, MaketteTask[ResultFolder]](
          "java-task" -> sources(Seq(Paths.get("makette/test/resources", "single/A.java"))),
          "class-task" -> javac("java-task"),
          "jar-task" -> jar("class-task", "makette.jar")
        ).toTasks(workspace)

        // WHEN
        val store = StoreModule.initialise((), k => Ko(s"Task $k did not run yet"))
        val ((), result) =
            topologicalAlwaysRebuild[Key, TaskResult[ResultFolder]].build(tasks, "jar-task", store)

        // THEN
        assertEquals(
          result("jar-task").cast[ResultFolder].map(_.filePaths),
          Ok(Set(workspace("jar-task").resolve("out/makette.jar")))
        )
        assertEquals(
          result("class-task").cast[ResultFolder].map(_.filePaths),
          Ok(Set(workspace("class-task").resolve("out/single/A.class")))
        )

    test("Shortcut compilation when nothing changed"):
        type VT = VerifyingTrace[Key, TaskResult[ResultFolder], Hashette.Hash]
        given FunctionalStoreModule[VT, Key, TaskResult[ResultFolder]] = FunctionalStoreModule()

        val workspace = testWorkspace()

        val a_workspace = workspace("a-input")
        val a_out = a_workspace.workdir("sources")
        val a_input = a_workspace
            .stash(Paths.get("makette/test/resources/deepdeps", "a/A.java"), a_out)
            .let(_ => ResultFolder(using a_workspace)(a_out))

        val observedB = TaskObserver(javac("b", Seq("a-class")))
        val observedC = TaskObserver(javac("c", Seq("b-class")))
        val tasks = Map[Key, MaketteTask[ResultFolder]](
          "b" -> sources(Seq(Paths.get("makette/test/resources/deepdeps", "b/B.java"))),
          "c" -> sources(Seq(Paths.get("makette/test/resources/deepdeps", "c/C.java"))),
          "a-class" -> javac("a"),
          "b-class" -> observedB,
          "c-class" -> observedC
        )

        val store =
            StoreModule.initialise(VerifyingTrace.InMemory.init, (k => Ko("Oops"))).putValue("a", Ok(a_input))
        val order = FixOrderScheduler[Applicative, VT, Key, TaskResult[ResultFolder]](
          Seq("a", "b", "c", "a-class", "b-class", "c-class")
        )
        val rebuilder = VTRebuilder[Key, TaskResult[ResultFolder], Hashette.Hash]
        val buildSystem = order.buildSystem(rebuilder)
        val (vt, result) =
            buildSystem.build(
              tasks.let(MaketteTasks(_, workspace)),
              "c-class",
              store
            )
        assertEquals(observedB.callCount, 1)
        assertEquals(observedC.callCount, 1)

        val a_alter_workspace = workspace("a-alter-input")
        val a_alter_out = a_alter_workspace.workdir("sources")
        val a_alter_input = a_alter_workspace
            .stash(Paths.get("makette/test/resources/deepdeps", "a_alter/A.java"), a_alter_out)
            .let(_ => ResultFolder(using a_alter_workspace)(a_alter_out))

        val aChanged = (vt, result).putValue("a", Ok(a_alter_input))
        val (vtBis, resultBis) =
            buildSystem.build(
              tasks.let(MaketteTasks(_, workspace)),
              "c-class",
              aChanged
            )
        assertEquals(
          result("c-class").cast[ResultFolder].map(_.filePaths),
          Ok(Set(workspace("c-class").resolve("out/C.class")))
        )
        assertEquals(observedB.callCount, 2) // B was rebuilt because A.class changed
        assertEquals(observedC.callCount, 1) // C was not rebuilt because B.class did not change

    private object HashetteModule extends HashModule[TaskResult[ResultFolder], Hashette.Hash]:
        override def hash(value: TaskResult[ResultFolder]): Hash =
            value match
                case Ok(v)      => Hashette.hashPath(v.folderPath, Hashette.Method.MD_5)
                case Ko(reason) => Hashette.hashString(s"KO($reason)", Hashette.Method.MD_5)

    private given HashModule[TaskResult[ResultFolder], Hashette.Hash] = HashetteModule

    private def topologicalAlwaysRebuild[K, V](using Ordering[K]): BuildSystem[Applicative, Unit, K, V] =
        TopologicalScheduler().buildSystem(alwaysRebuild)

    private def alwaysRebuild[K, V]: Rebuilder[Monad, Unit, K, V] = new:
        override def rebuild(key: K, task: Task[Monad, K, V], lastValue: V): Task[M[Unit], K, V] = task

    private def testWorkspace() = TestWorkspace(Files.createTempDirectory("makette"))
    extension [V](mt: Map[Key, MaketteTask[V]])
        private def toTasks(workspace: Key => TestWorkspace) =
            MaketteTasks(mt, workspace)

end MaketteTaskSuite

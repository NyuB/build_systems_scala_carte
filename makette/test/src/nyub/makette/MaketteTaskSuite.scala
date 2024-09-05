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

class MaketteTaskSuite extends munit.FunSuite:
    test("From java to jar, with class"):
        // GIVEN
        val tempDir = Files.createTempDirectory("makette")
        val tasks = Map[Key, MaketteTask[ResultFiles | ResultFolder]](
          "java-task" -> sources(Seq(Paths.get("makette/test/resources", "com/A.java"))),
          "class-task" -> javac("java-task"),
          "jar-task" -> jar("class-task", "makette.jar")
        ).let(MaketteTasks(_, testWorkspace(tempDir)))

        // WHEN
        given FunctionalStoreModule[Unit, Key, TaskResult[ResultFiles | ResultFolder]] = FunctionalStoreModule()
        val store = StoreModule.initialise((), k => Ko(s"Task $k did not run yet"))
        val ((), result) =
            topologicalAlwaysRebuild[Key, TaskResult[ResultFiles | ResultFolder]].build(tasks, "jar-task", store)

        // THEN
        assertEquals(
          result("jar-task").cast[ResultFiles].map(_.filePaths),
          Ok(Set(tempDir.resolve("jar-task/out/makette.jar")))
        )
        assertEquals(
          result("class-task").cast[ResultFolder].map(_.filePaths),
          Ok(Set(tempDir.resolve("class-task/out/com/A.class")))
        )

    private def topologicalAlwaysRebuild[K, V](using Ordering[K]): BuildSystem[Applicative, Unit, K, V] =
        TopologicalScheduler().buildSystem(alwaysRebuild)

    private def alwaysRebuild[K, V]: Rebuilder[Monad, Unit, K, V] = new:
        override def rebuild(key: K, task: Task[Monad, K, V], lastValue: V): Task[M[Unit], K, V] = task

end MaketteTaskSuite

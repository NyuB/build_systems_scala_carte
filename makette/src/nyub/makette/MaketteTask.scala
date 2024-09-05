package nyub.makette

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Applicative
import java.nio.file.Path
import nyub.makette.TaskResult.Ok
import nyub.makette.TaskResult.Ko
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

trait MaketteTask[V]:
    def within(workspace: Workspace): Task[Applicative, Key, TaskResult[V]]

object MaketteTask:
    def apply[V](
        f: [F[_]] => Applicative[F] ?=> Workspace ?=> (Key => F[TaskResult[V]]) => F[TaskResult[V]]
    ): MaketteTask[V] =
        new:
            override def within(workspace: Workspace): Task[Applicative, Key, TaskResult[V]] = new:
                given Workspace = workspace
                override def run[F[_]](using constraints: Applicative[F])(
                    fetch: Key => F[TaskResult[V]]
                ): F[TaskResult[V]] =
                    f(fetch)

class MaketteTasks[V](val makettes: Map[Key, MaketteTask[V]], val workspaceFactory: Key => Workspace)
    extends Tasks[Applicative, Key, TaskResult[V]]:
    override def get(taskKey: Key): Option[Task[Applicative, Key, TaskResult[V]]] =
        makettes.get(taskKey).map(_.within(workspaceFactory(taskKey)))

def sources(files: Seq[Path]) = MaketteTask[ResultFolder | ResultFiles]:
    [F[_]] =>
        applicative ?=>
            workspace ?=>
                fetch =>
                    applicative.pure:
                        workspace.clear()
                        val dir = workspace.workdir("out")
                        files.foreach(workspace.stash(_, dir))
                        Ok(ResultFolder(dir))

def javac(sourcesTask: Key) = MaketteTask[ResultFolder | ResultFiles]:
    [F[_]] =>
        applicative ?=>
            workspace ?=>
                fetch =>
                    fetch(sourcesTask).map:
                        workspace.clear()
                        val dir = workspace.workdir("out")
                        _.cast[ResultFolder].flatMap: sources =>
                            val eachSource = sources.filePaths.map(_.toString()).toSeq
                            val command = Seq(
                              "javac",
                              "-d",
                              dir.toString(),
                              "-sourcepath",
                              sources.folderPath.toString()
                            ) ++ eachSource
                            val exitCode = ProcessBuilder().command(command*).start().waitFor()
                            if exitCode == 0 then Ok(ResultFolder(dir))
                            else Ko(s"javac exited with non-zero code $exitCode")

def jar(classesTask: Key, jarName: String) = MaketteTask[ResultFiles | ResultFolder]:
    [F[_]] =>
        applicative ?=>
            workspace ?=>
                fetch =>
                    fetch(classesTask).map:
                        workspace.clear()
                        val dir = workspace.workdir("out")
                        val jar = workspace.workfile(jarName, dir)
                        _.cast[ResultFolder].flatMap: classes =>
                            val eachClass = classes.filePaths.flatMap: classFile =>
                                Seq(
                                  "-C",
                                  classes.folderPath.toString(),
                                  classes.folderPath.relativize(classFile).toString()
                                )
                            val command = Seq("jar", "--create", "--file", jar.filePath.toString()) ++ eachClass
                            val exitCode = ProcessBuilder().command(command*).start().waitFor()
                            if exitCode == 0 then Ok(ResultFiles(Set(jar)))
                            else Ko(s"jar exited with non-zero code $exitCode")

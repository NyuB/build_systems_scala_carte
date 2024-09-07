package nyub.makette

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.monads.Applicative
import java.nio.file.Path
import nyub.makette.TaskResult.Ok
import nyub.makette.TaskResult.Ko
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks
import java.lang.ProcessBuilder.Redirect

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

def sources(files: Seq[Path]) = MaketteTask[ResultFolder]:
    [F[_]] =>
        applicative ?=>
            workspace ?=>
                fetch =>
                    applicative.pure:
                        workspace.clear()
                        val dir = workspace.workdir("out")
                        files.foreach(workspace.stash(_, dir))
                        Ok(ResultFolder(dir))

def javac(sourcesTask: Key, classesTasks: Seq[Key] = Seq.empty) = MaketteTask[ResultFolder]:
    [F[_]] =>
        applicative ?=>
            workspace ?=>
                fetch =>
                    val classPathCommand = classesTasksToClassPath(fetch)(classesTasks).map: r =>
                        r.map: cp =>
                            if cp.isEmpty then Seq.empty
                            else
                                Seq(
                                  "-classpath",
                                  cp.map(_.folderPath.toString()).mkString(";")
                                )

                    val sourcesCommand = fetch(sourcesTask).map:
                        _.cast[ResultFolder].map: sources =>
                            val eachSource = sources.filePaths.map(_.toString()).toSeq
                            Seq(
                              "-sourcepath",
                              sources.folderPath.toString()
                            ) ++ eachSource

                    (sourcesCommand, classPathCommand).map2: (src, cp) =>
                        src.flatMap: src =>
                            cp.flatMap: cp =>
                                workspace.clear()
                                val dir = workspace.workdir("out")
                                val command = Seq("javac", "-d", dir.toString()) ++ cp ++ src
                                val exitCode = ProcessBuilder()
                                    .command(command*)
                                    .redirectOutput(Redirect.INHERIT)
                                    .redirectError(Redirect.INHERIT)
                                    .start()
                                    .waitFor()
                                if exitCode == 0 then Ok(ResultFolder(dir))
                                else Ko(s"javac exited with non-zero code $exitCode")

private def classesTasksToClassPath[F[_]](using
    app: Applicative[F]
)(fetch: Key => F[TaskResult[ResultFolder]])(classesTasks: Seq[Key]): F[TaskResult[Seq[ResultFolder]]] =
    classesTasks
        .map(fetch)
        .foldLeft(app.pure(Ok(Seq.empty))): (acc, item) =>
            (acc, item).map2: (cps, cp) =>
                cps.flatMap(cps => cp.map(cps.appended(_)))

def jar(classesTask: Key, jarName: String) = MaketteTask[ResultFolder]:
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
                            if exitCode == 0 then Ok(ResultFolder(dir))
                            else Ko(s"jar exited with non-zero code $exitCode")

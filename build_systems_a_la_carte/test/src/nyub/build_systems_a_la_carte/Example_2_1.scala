package nyub.build_systems_a_la_carte

import nyub.build_systems_a_la_carte.monads.Applicative
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Tasks

object Example_2_1:
    val storeModule = FunctionalStoreModule[Unit, String, Artifact]()
    val inputs = storeModule.initialise(
      (),
      {
          case "util.h" => Artifact.HeaderFile("[HEADER - util]")
          case "util.c" => Artifact.HeaderFile("[SOURCE - util]")
          case "main.c" => Artifact.HeaderFile("[SOURCE - main]")
      }
    )

    val tasks: Tasks[Applicative, String, Artifact] = s =>
        s match
            case s"${prefix}.o"   => Some(objectFileTask(s"${prefix}.c", List("util.h")))
            case s"${prefix}.exe" => Some(executableFileTask(List(s"${prefix}.o", "util.o")))
            case _                => None

    sealed trait Artifact:
        def content: String

    object Artifact:
        object Missing extends Artifact:
            override val content = ""

        case class CSourceFile(override val content: String) extends Artifact
        case class HeaderFile(override val content: String) extends Artifact
        case class ObjectFile(override val content: String) extends Artifact
        case class Executable(override val content: String) extends Artifact

    private def objectFileTask(sourceFile: String, headers: List[String]): Task[Applicative, String, Artifact] =
        [F[_]] =>
            app ?=>
                fetch =>
                    val headersContent = headers
                        .map(fetch)
                        .foldLeft(app.pure("")): (acc, header) =>
                            concatContents `<$>` acc <*> (header.map(_.content))
                    val sourceContent = fetch(sourceFile).map(_.content)
                    val totalContent = concatContents `<$>` headersContent <*> sourceContent
                    totalContent.map(c => Artifact.ObjectFile(s"[OBJ - $c]"))

    private def executableFileTask(objectFiles: List[String]): Task[Applicative, String, Artifact] =
        [F[_]] =>
            app ?=>
                fetch =>
                    objectFiles
                        .map(fetch)
                        .foldLeft(app.pure("")): (acc, obj) =>
                            concatContents `<$>` acc <*> (obj.map(_.content))
                        .map(c => Artifact.Executable(s"[EXE - $c]"))

    private def concatContents(a1: String)(a2: String) = s"$a1$a2"
end Example_2_1

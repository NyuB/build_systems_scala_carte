package nyub.makette

import java.nio.file.Path
import java.io.File
import java.nio.file.Files

class TestWorkspace(val tempDir: Path, val key: Key) extends Workspace:
    override type Workdir = Path
    override type Workfile = Path

    def resolve(path: String): Path =
        root.resolve(path)

    private val root = tempDir.resolve(key)
    override def stash(actualFile: Path, workdir: Workdir): Workfile =
        Files.copy(actualFile, workdir.resolve(actualFile.getFileName()))

    override def workdir(name: String): Workdir =
        Files.createDirectories(root.resolve(name))

    override def workfile(name: String, workdir: Path): Workfile = workdir.resolve(name)

    override def clear(): Unit =
        if root.toFile().exists() then root.toFile().listFiles().map(_.toPath()).foreach(rmRf)

    extension (w: Workdir)
        override def files: Seq[Workfile] = listFiles(List.empty, w.toFile()).map(_.toPath())
        override def dirPath: Path = w.toAbsolutePath()

    extension (w: Workfile) override def filePath: Path = w.toAbsolutePath()

    private def listFiles(acc: List[File], file: File): List[File] =
        if file.isFile() then file :: acc
        else file.listFiles().foldLeft(acc)(listFiles)

object TestWorkspace:
    def apply(tempDir: Path)(key: Key): TestWorkspace = new TestWorkspace(tempDir, key)

private def rmRf(path: Path): Unit =
    val f = path.toFile()
    if f.isDirectory() then f.listFiles().map(_.toPath()).foreach(rmRf)
    Files.delete(path)

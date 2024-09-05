package nyub.makette

import java.nio.file.Path

final class ResultFiles(using val workspace: Workspace)(val files: Set[workspace.Workfile]):
    val filePaths: Set[Path] = files.map(_.filePath)
    override def equals(x: Any): Boolean =
        x.isInstanceOf[ResultFiles] && x.asInstanceOf[ResultFiles].filePaths == filePaths

final class ResultFolder(using val workspace: Workspace)(val folder: workspace.Workdir):
    val folderPath: Path = folder.dirPath
    val files = folder.files
    val filePaths: Set[Path] = folder.files.map(_.filePath).toSet
    override def equals(x: Any): Boolean =
        x.isInstanceOf[ResultFolder] && x.asInstanceOf[ResultFolder].filePaths == filePaths

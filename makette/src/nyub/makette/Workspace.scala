package nyub.makette

import java.nio.file.Path

trait Workspace:
    type Workdir
    type Workfile
    def workdir(name: String): Workdir
    def workfile(name: String, workdir: Workdir): Workfile
    def stash(actualFile: Path, workdir: Workdir): Workfile
    def clear(): Unit

    extension (w: Workdir)
        def files: Seq[Workfile]
        def dirPath: Path

    extension (w: Workfile) def filePath: Path

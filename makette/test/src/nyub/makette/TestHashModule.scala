package nyub.makette

import nyub.build_systems_a_la_carte.HashModule
import nyub.makette.TaskResult.Ok
import nyub.makette.TaskResult.Ko
import java.security.MessageDigest
import java.nio.file.Path
import java.nio.file.Files
import java.util.HexFormat

object TestHashModule extends HashModule[TaskResult[ResultFolder], String]:
    private val hexFormat = HexFormat.of()
    override def hash(value: TaskResult[ResultFolder]): String = value match
        case Ok(v)      => hexFormat.formatHex(hashPath(v.folderPath))
        case Ko(reason) => hexFormat.formatHex(hashString(s"KO($reason)"))

    private def initMd() = MessageDigest.getInstance("SHA1")
    private def hashFile(p: Path): Array[Byte] =
        initMd().digest(Files.readAllBytes(p))

    private def hashPath(p: Path): Array[Byte] =
        if p.toFile().isFile() then hashFile(p)
        else if p.toFile().isDirectory() then
            val md = initMd()
            p.toFile()
                .listFiles()
                .sortBy(_.getName())
                .foreach: f =>
                    md.update(f.getName().getBytes())
                    md.update(hashPath(f.toPath()).toArray)
            md.digest()
        else throw IllegalArgumentException(s"$p is not a file nor a directory")

    private def hashString(s: String) = initMd().digest(s.getBytes())

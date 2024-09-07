package nyub.hashette

import java.nio.file.Path
import java.security.MessageDigest
import java.nio.file.Files
import scala.collection.immutable.ArraySeq

object Hashette:
    opaque type Hash = ByteWrap
    private case class ByteWrap(val bytes: Seq[Byte]):
        override def toString(): String = bytes.map(byteToHex).mkString

    extension (h: Hash)
        def toHex: String =
            h.bytes.map(byteToHex).mkString

    enum Method(private[Hashette] val id: String):
        case SHA_1 extends Method("SHA-1")
        case SHA_256 extends Method("SHA-256")
        case MD_5 extends Method("MD5")

    def hashPath(path: Path, method: Method): Hash =
        if path.toFile().isFile() then hashSingleFile(path, method)
        else if path.toFile().isDirectory() then
            val md = initMd(method)()
            path.toFile()
                .listFiles()
                .sortBy(_.getName())
                .foreach: f =>
                    md.update(f.getName().getBytes())
                    md.update(hashPath(f.toPath(), method).bytes.toArray)
            md.digest().wrapped
        else throw IllegalArgumentException(s"$path is not a file or directory")

    def hashPaths(paths: Seq[Path], method: Method): Hash =
        val md = initMd(method)()
        paths.foreach(p => md.update(hashPath(p, method).bytes.toArray))
        md.digest().wrapped

    def hashString(s: String, method: Method): Hash = initMd(method)().digest(s.getBytes()).wrapped

    private def byteToHex(num: Byte) =
        val left = Character.forDigit((num >> 4) & 0xf, 16);
        val right = Character.forDigit((num & 0xf), 16);
        s"$left$right"

    private def initMd(method: Method)(): MessageDigest = MessageDigest.getInstance(method.id)

    extension (bytes: Array[Byte]) private def wrapped = ByteWrap(ArraySeq.unsafeWrapArray(bytes))
    private def hashSingleFile(path: Path, method: Method): Hash =
        initMd(method)().digest(Files.readAllBytes(path)).wrapped

end Hashette

package nyub.hashette

import java.nio.file.Path
import java.security.MessageDigest
import java.nio.file.Files
import scala.collection.immutable.ArraySeq
import java.util.HexFormat

object Hashette:
    opaque type Hash = ByteWrap
    trait Cache:
        def get(p: Path): Option[Hash]
        def put(p: Path, h: Hash): Unit

    object NoCache extends Cache:
        override def get(p: Path): Option[Hash] = None
        override def put(p: Path, h: Hash): Unit = ()

    trait Listener:
        def send(p: Path, h: Hash): Unit

    object NoListener extends Listener:
        override def send(p: Path, h: Hash): Unit = ()

    private case class ByteWrap(val bytes: Seq[Byte]):
        override def toString(): String =
            hexFormat.formatHex(bytes.toArray)

    extension (h: Hash) def toHex: String = hexFormat.formatHex(h.bytes.toArray)
    def fromHex(hex: String): Hash = hexFormat.parseHex(hex).wrapped

    enum Method(private[Hashette] val id: String):
        case SHA_1 extends Method("SHA-1")
        case SHA_256 extends Method("SHA-256")
        case MD_5 extends Method("MD5")

    def hashPath(path: Path, method: Method, cache: Cache = NoCache, listener: Listener = NoListener): Hash =
        cache
            .get(path)
            .getOrElse:
                val res =
                    if path.toFile().isFile() then hashSingleFile(path, method)
                    else if path.toFile().isDirectory() then
                        val md = initMd(method)()
                        path.toFile()
                            .listFiles()
                            .sortBy(_.getName())
                            .foreach: f =>
                                md.update(f.getName().getBytes())
                                md.update(hashPath(f.toPath(), method, cache, listener).bytes.toArray)
                        md.digest().wrapped
                    else throw IllegalArgumentException(s"$path is not a file or directory")
                cache.put(path, res)
                listener.send(path, res)
                res

    def hashString(s: String, method: Method): Hash = initMd(method)().digest(s.getBytes()).wrapped

    private def initMd(method: Method)(): MessageDigest = MessageDigest.getInstance(method.id)

    extension (bytes: Array[Byte]) private def wrapped = ByteWrap(ArraySeq.unsafeWrapArray(bytes))
    private def hashSingleFile(path: Path, method: Method): Hash =
        initMd(method)().digest(Files.readAllBytes(path)).wrapped

    private val hexFormat = HexFormat.of()

end Hashette

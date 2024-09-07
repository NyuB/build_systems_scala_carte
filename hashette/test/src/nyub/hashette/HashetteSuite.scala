package nyub.hashette

import nyub.hashette.Hashette.Hash
import java.nio.file.Path
import java.nio.file.Paths

class HashetteSuite extends munit.FunSuite with AssertExtensions:
    private val aFile = Paths.get("hashette/test/resources/a.txt")
    private val (someFolder, someFolderCopy) =
        (Paths.get("hashette/test/resources/folder_a"), Paths.get("hashette/test/resources/folder_a_copy"))

    private val (trickyFolder, trickyFile) =
        (Paths.get("hashette/test/resources/tricky_folder"), Paths.get("hashette/test/resources/tricky.txt"))

    test("Single file (SHA256)"):
        Hashette
            .hashPath(aFile, Hashette.Method.SHA_256)
            .toHex `is equal to` "cb1ad2119d8fafb69566510ee712661f9f14b83385006ef92aec47f523a38358"

    test("Single file (SHA1)"):
        Hashette
            .hashPath(aFile, Hashette.Method.SHA_1)
            .toHex `is equal to` "606ec6e9bd8a8ff2ad14e5fade3f264471e82251"

    test("Single file (MD5)"):
        Hashette.hashPath(aFile, Hashette.Method.MD_5).toHex `is equal to` "e1faffb3e614e6c2fba74296962386b7"

    test("Folders with the same contents have the same hash"):
        Hashette.Method.values.foreach: method =>
            Hashette.hashPath(someFolder, method) `is equal to` Hashette.hashPath(someFolderCopy, method)

    test("Folders' children names are treated differently from their content"):
        Hashette.hashPath(trickyFile, Hashette.Method.MD_5) `is not equal to` Hashette.hashPath(
          trickyFolder,
          Hashette.Method.MD_5
        )

    test("hashPaths uses cache when present"):
        Hashette.Method.values.foreach: method =>
            val spy = HashSpy()
            val cache = ReadOnlyCache(Map(Paths.get("hashette/test/resources/folder_a") -> Hashette.fromHex("AA")))
            Hashette.hashPath(Paths.get("hashette/test/resources/folder_a"), method, cache, spy) `is equal to` Hashette
                .fromHex("AA")
            spy.called = 0

    private class HashSpy extends Hashette.Listener:
        var called = 0
        override def send(p: Path, h: Hash): Unit =
            called += 1

    private class ReadOnlyCache(val map: Map[Path, Hashette.Hash]) extends Hashette.Cache:
        override def get(p: Path): Option[Hash] = map.get(p)
        override def put(p: Path, h: Hash): Unit = ???

end HashetteSuite

package nyub.hashette

import munit.internal.io.PlatformIO.Paths

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

    test("hashPaths is order-sensitive"):
        Hashette.Method.values.foreach: method =>
            Hashette.hashPaths(Seq(aFile, someFolder), method) `is not equal to` Hashette.hashPaths(
              Seq(someFolder, aFile),
              method
            )

end HashetteSuite

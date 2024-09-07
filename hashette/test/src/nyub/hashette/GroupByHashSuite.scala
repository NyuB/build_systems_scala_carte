package nyub.hashette

import munit.internal.io.PlatformIO.Paths

class GroupByHashSuite extends munit.FunSuite with AssertExtensions:
    private val method = Hashette.Method.SHA_256

    test("group same files"):
        val hashA = Hashette.hashPath(Paths.get("hashette/test/resources/a.txt"), method)
        val groupByHash = GroupByHash()
        val _ = Hashette.hashPath(Paths.get("hashette/test/resources/groups"), method, listener = groupByHash)
        groupByHash.groups(hashA) `is equal to` Set(
          "hashette/test/resources/groups/a.txt",
          "hashette/test/resources/groups/b_with_a.txt",
          "hashette/test/resources/groups/folder_one/a.txt",
          "hashette/test/resources/groups/folder_two/b_with_a.txt",
          "hashette/test/resources/groups/folder_three/a.txt"
        ).map(Paths.get(_))

    test("group same folders with same files AND same names"):
        val groupByHash = GroupByHash()
        val hashFolderOne = Hashette.hashPath(Paths.get("hashette/test/resources/groups/folder_one"), method)
        val hashFolderTwo = Hashette.hashPath(Paths.get("hashette/test/resources/groups/folder_two"), method)
        val _ = Hashette.hashPath(Paths.get("hashette/test/resources/groups"), method, listener = groupByHash)
        groupByHash.groups(hashFolderOne) `is equal to` Set(
          "hashette/test/resources/groups/folder_one",
          "hashette/test/resources/groups/folder_three"
        ).map(Paths.get(_))
        groupByHash.groups(hashFolderTwo) `is equal to` Set(
          "hashette/test/resources/groups/folder_two"
        ).map(Paths.get(_))

end GroupByHashSuite

package nyub.build_systems_a_la_carte.monads

import org.scalacheck.Arbitrary
import org.scalacheck.Gen

class OptionMonadProperties extends MonadProperties[Option]:
    override def scalaCheckInitialSeed = "K3HFZb7sCk0GL2cE2HDoFbbhMBh5_ZTsEu25FnMAYGN="
    override given associativityTestCases: Arbitrary[AssociativityTestCase[?, ?, ?]] =
        val gen = longOptions.map: l =>
            AssociativityTestCase(l, someIsMultipleOfThree, ifTrueNoneElseSomeOk)
        Arbitrary(gen)

    override given leftIdentityTestCases: Arbitrary[LeftIdentityTestCase[?, ?]] =
        val gen = Gen.long.map: l =>
            LeftIdentityTestCase(l, someIsMultipleOfThree)
        Arbitrary(gen)

    override given rightIdentityTestCases: Arbitrary[RightIdentityTestCase[?]] =
        val gen = longOptions.map: l =>
            RightIdentityTestCase(l)
        Arbitrary(gen)

    override def eq[A](a: Option[A], b: Option[A]): Boolean = a == b

    private def longOptions = Gen.option(Gen.long)
    private def someIsMultipleOfThree(n: Long): Option[Boolean] = Some(n % 3 == 0)
    private def ifTrueNoneElseSomeOk(b: Boolean): Option[String] = if b then None else Some("OK")
end OptionMonadProperties

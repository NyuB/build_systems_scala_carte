package nyub.build_systems_a_la_carte.monads

import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen

type StringToLong = Map[String, Long]
type StringToLongState[A] = State[StringToLong, A]
given Monad[StringToLongState] = State.Monad.stateMonad[StringToLong]

class StringStateMonadPropertiesSuite extends MonadProperties[StringToLongState]:
    override given associativityTestCases: Arbitrary[AssociativityTestCase[?, ?, ?]] =
        val gen = summon[Arbitrary[StringToLong]].arbitrary.flatMap: maps =>
            AssociativityTestCase(
              maps.ret,
              m => State.gets(s => s.size),
              n => State.modify(s => s.updated(n.toHexString, n))
            )
        Arbitrary(gen)

    override given leftIdentityTestCases: Arbitrary[LeftIdentityTestCase[?, ?]] =
        val gen = summon[Arbitrary[StringToLong]].arbitrary.flatMap: maps =>
            Gen.long.map: l =>
                LeftIdentityTestCase(l, n => maps.ret >> State.modify(s => s.updated(n.toHexString, n)))
        Arbitrary(gen)

    override given rightIdentityTestCases: Arbitrary[RightIdentityTestCase[?]] =
        val gen = summon[Arbitrary[StringToLong]].arbitrary.flatMap: maps =>
            Gen.long.map: l =>
                RightIdentityTestCase(maps.ret)
        Arbitrary(gen)

    override def eq[A](a: StringToLongState[A], b: StringToLongState[A]): Prop =
        forAll: (s: StringToLong) =>
            assertEquals(a.runState(s), b.runState(s))

end StringStateMonadPropertiesSuite

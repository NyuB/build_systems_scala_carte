package nyub.build_systems_a_la_carte

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary

trait MonadProperties[M[_]](using monad: Monad[M]) extends munit.ScalaCheckSuite:

    property("Left identity"):
        forAll: (rp: LeftIdentityTestCase[?, ?]) =>
            (rp.a.ret >>= rp.f) =:= rp.f(rp.a)

    property("Right identity"):
        forAll: (rp: RightIdentityTestCase[?]) =>
            rp.m =:= (rp.m >>= monad.ret)

    property("Associativity"):
        forAll: (fp: AssociativityTestCase[?, ?, ?]) =>
            (fp.m >>= (x => fp.f(x) >>= fp.g)) =:= ((fp.m >>= fp.f) >>= fp.g)

    def eq[A](a: M[A], b: M[A]): Boolean
    extension [A](a: M[A]) final infix def =:=(b: M[A]): Boolean = eq(a, b)

    given leftIdentityTestCases: Arbitrary[LeftIdentityTestCase[?, ?]]
    given rightIdentityTestCases: Arbitrary[RightIdentityTestCase[?]]
    given associativityTestCases: Arbitrary[AssociativityTestCase[?, ?, ?]]
    class LeftIdentityTestCase[A, B](val a: A, val f: A => M[B])
    class RightIdentityTestCase[A](val m: M[A])
    class AssociativityTestCase[X, A, B](val m: M[X], val f: X => M[A], val g: A => M[B])
end MonadProperties

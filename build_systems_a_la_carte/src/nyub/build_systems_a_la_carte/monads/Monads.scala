package nyub.build_systems_a_la_carte.monads

/** Constraints over a given environment, requiring the capability to apply an arbitrary function to a value within the
  * environment
  * @tparam F
  *   an environment (or e**ff**ect) over arbitrary values
  */
trait Functor[F[_]]:
    extension [A](fa: F[A])
        /** @tparam B
          *   `f` return type
          * @param f
          *   a function from `A` to `B`
          * @return
          *   the result within `F` of the application of `f` to the value wrapped by `fa`
          */
        def map[B](f: A => B): F[B]

        /** Haskell-like operator for [[map]]
          */
        final infix def `<&>`[B](f: A => B): F[B] = fa.map(f)

    extension [A, B](f: A => B)
        /** Haskell-like operator for `fa`.[[map]](`f`)
          */
        final infix def `<$>`(fa: F[A]): F[B] = fa.map(f)

end Functor

/** Constraints over a given environment, requiring [[Functor]] capabilities and two additional ones:
  *   - lift any value to the environment via [[pure]]
  *   - combine any pair of values already within the environments via [[map2]]
  * @tparam F
  *   an environment (or e**ff**ect) over arbitrary values
  */
trait Applicative[F[_]] extends Functor[F]:
    /** @param a
      *   an arbitrary expression. Passed **by-name**, to allow lazy evaluation implementations
      * @return
      *   the value `a` lifted in the environment `F`
      */
    def pure[A](a: => A): F[A]
    extension [A](fa: F[A])
        final override def map[B](f: A => B): F[B] =
            pure(f).ap(fa)

    extension [A, B](ff: F[A => B])
        /** @param fa
          *   a value within `F`
          * @return
          *   the result of applying the function wrapped by `ff` to the value wrapped by `fa`
          */
        def ap(fa: F[A]): F[B]

        /** Haskell-like operator for [[ap]]
          */
        final infix def <*>(fa: F[A]): F[B] = ap(fa)

    extension [A, B](pair: (F[A], F[B]))
        /** @tparam C
          *   `f` return type
          * @param f
          *   the function to apply
          * @return
          *   the result within `F` of the function `f` applied to this `pair` of values within `F`
          */
        def map2[C](f: (A, B) => C): F[C] =
            pure((a: A) => (b: B) => f(a, b)).ap(pair._1).ap(pair._2)

end Applicative

/** Constraints over a given environment, requiring [[Applicative]] capabilities and the capability to 'flatten' nested
  * environments, allowing to chain computations returning values within the environment via [[flatMap]]
  * @tparam F
  *   an environment (or e**ff**ect) over arbitrary values
  */
trait Monad[F[_]] extends Applicative[F]:
    extension [A](a: A)
        /** @return
          *   this value lifted to the environement `F`
          */
        final def ret: F[A] = pure(a)

    extension [A](fa: F[A])
        /** @tparam B
          *   the type of the value wrapped by the return type of `f`
          * @param f
          *   a function going to an **arbitrary** value to another value **within F**
          * @return
          *   the result within `F` of applying the function f to the value wrapped by `fa`, and then removing the
          *   resulting additional level of nesting within `F`
          */
        def flatMap[B](f: A => F[B]): F[B]

        /** Haskell-like operator for [[flatMap]]
          */
        final infix def >>=[B](f: A => F[B]): F[B] = flatMap(f)

        /** Haskell-like operator for [[flatMap]]`(_ => fb)`
          */
        final infix def >>[B](fb: F[B]): F[B] = flatMap(_ => fb)

    extension [A, B](fab: F[A => B])
        final override def ap(fa: F[A]): F[B] = fab.flatMap: ab =>
            fa.flatMap(a => ab(a).ret)

end Monad

object Monad:
    /** [[Monad]] implementation for [[Option]] by delegation
      */
    given Monad[Option] with
        override def pure[A](a: => A): Option[A] = Some(a)
        extension [A](
            fa: Option[A]
        ) override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

end Monad

/** Simplest monad, wrapping a value and passing it around without doing anything else
  * @param expression
  *   the value wrapped by this identity. Passed by-name and lazy evaluated once to yield [[value]]
  */
class Identity[A](expression: => A):
    lazy val value = expression
    override def equals(x: Any): Boolean =
        x.isInstanceOf[Identity[?]] && x.asInstanceOf[Identity[?]].value == value

object Identity:
    /** Trivial [[Monad]] implementation for [[Identity]]
      */
    given Monad[Identity] with
        override def pure[A](a: => A): Identity[A] = Identity(a)
        extension [A](fa: Identity[A]) override def flatMap[B](f: A => Identity[B]): Identity[B] = f(fa.value)

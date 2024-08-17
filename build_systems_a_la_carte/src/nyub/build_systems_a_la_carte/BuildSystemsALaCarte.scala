package nyub.build_systems_a_la_carte

object BuildSystemsALaCarte:
    trait StoreModule[I, K, V]:
        type Store
        def initialise(i: I, kv: (K => V)): Store
        def getInfo(store: Store): I
        def putInfo(info: I, store: Store): Store
        def getValue(key: K, store: Store): V
        def putValue(using CanEqual[K, K])(key: K, v: V, store: Store): Store
    end StoreModule

    trait HashModule[V]:
        type Hash
        def hash[T <: V](value: T): Hash
        def getHash[K](key: K, storeModule: StoreModule[?, K, V]): Hash
    end HashModule

    trait Task[C[_[_]], K, V]:
        def run[F[_]](using C[F])(kfv: K => F[V]): F[V]
    end Task

    type Tasks[C[_[_]], K, V] = K => Option[Task[C, K, V]]

    trait BuildModule[C[_[_]], I, K, V]:
        val s: StoreModule[I, K, V]
        type Build = (Tasks[C, K, V], K, s.Store) => s.Store
    end BuildModule

end BuildSystemsALaCarte

trait Functor[F[_]]:
    extension [A](fa: F[A]) def map[B](f: A => B): F[B]
    extension [A, B](f: A => B)
        final infix def `<$>`(fa: F[A]): F[B] = fa.map(f)

end Functor

trait Applicative[F[_]] extends Functor[F]:
    def pure[A](a: A): F[A]
    extension [A](fa: F[A])
        final override def map[B](f: A => B): F[B] =
            pure(f).ap(fa)

    extension [A, B](ff: F[A => B])
        def ap(fa: F[A]): F[B]
        final infix def <*>(fa: F[A]): F[B] = ap(fa)

end Applicative

trait Monad[F[_]]:
    def ret[A](a: A): F[A]

    def flatMap[A, B](f: A => F[B], fa: F[A]): F[B]
    final def >>=[A, B](fa: F[A], f: A => F[B]): F[B] = flatMap(f, fa)

    final def map[A, B](f: A => B, fa: F[A]): F[B] =
        flatMap((a: A) => ret(f(a)), fa)

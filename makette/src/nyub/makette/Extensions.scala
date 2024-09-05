package nyub.makette

extension [A](any: A) def let[B](f: A => B): B = f(any)

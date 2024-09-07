package nyub.build_systems_a_la_carte.hashes

import nyub.build_systems_a_la_carte.HashModule

/** [[HashModule]] implementation relying on JVM [[hashCode()]]
  */
object HashCodeModule extends HashModule[Any, Int]:
    override def hash(value: Any): Int = value.hashCode()

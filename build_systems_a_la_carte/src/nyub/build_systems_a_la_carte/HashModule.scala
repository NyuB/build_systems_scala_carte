package nyub.build_systems_a_la_carte

/** Hashing operations on objects
  * @tparam V
  *   the hashable values
  * @tparam H
  *   the hashing return type
  */
trait HashModule[-V, H]:
    /** @param value
      *   the object to hash
      * @return
      *   the hash of `value`
      */
    def hash(value: V): H
end HashModule

package nyub.build_systems_a_la_carte.rebuilders

/** Modification times tracker
  *
  * @param K
  *   the tracked keys type
  * @param now
  *   the current epoch
  * @param modificationTimes
  *   last modification of a given key
  */
case class ModificationTimes[K](now: ModificationTimes.Time, modificationTimes: Map[K, ModificationTimes.Time]):
    def touch(key: K): ModificationTimes[K] =
        ModificationTimes(now + 1, modificationTimes.updated(key, now))

object ModificationTimes:
    type Time = Int

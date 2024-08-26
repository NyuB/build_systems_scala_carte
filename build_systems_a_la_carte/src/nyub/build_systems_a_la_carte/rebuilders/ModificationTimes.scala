package nyub.build_systems_a_la_carte.rebuilders

case class ModificationTimes[K](now: ModificationTimes.Time, modificationTimes: Map[K, ModificationTimes.Time]):
    def touch(key: K): ModificationTimes[K] =
        ModificationTimes(now + 1, modificationTimes.updated(key, now))

object ModificationTimes:
    type Time = Int

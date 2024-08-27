package nyub.build_systems_a_la_carte.hashes

trait HashModule[V, H]:
    def hash[T <: V](value: T): H
end HashModule

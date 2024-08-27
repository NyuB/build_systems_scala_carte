package nyub.build_systems_a_la_carte.rebuilders

import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Rebuilder
import nyub.build_systems_a_la_carte.monads.Monad
import nyub.build_systems_a_la_carte.monads.State
import nyub.build_systems_a_la_carte.BuildSystemsALaCarte.Task
import nyub.build_systems_a_la_carte.DynamicDependencies

class VTRebuilder[K, V] extends Rebuilder[Monad, VerifyingTrace[K, V], K, V]:
    override def rebuild(
        key: K,
        task: Task[Monad, K, V],
        lastValue: V
    ): Task[State.Monad.M[VerifyingTrace[K, V]], K, V] = Task:
        [F[_]] =>
            monad ?=>
                fetch =>
                    monad.get.flatMap: vt =>
                        val valueHash = vt.hashModule.hash(lastValue)
                        def getHash(k: K): F[vt.Hash] =
                            fetch(k).map(vt.hashModule.hash)
                        vt.traceChanged(key, valueHash, getHash)
                            .flatMap: upToDate =>
                                if upToDate then lastValue.ret
                                else
                                    DynamicDependencies
                                        .directDependencies(fetch)(task)
                                        .flatMap: (newValue, dependencies) =>
                                            monad
                                                .modify: vtState =>
                                                    vtState.recordTrace(
                                                      key,
                                                      vtState.hashModule.hash(newValue),
                                                      dependencies.map((k, v) => k -> vtState.hashModule.hash(v))
                                                    )
                                                .flatMap: _ =>
                                                    monad.ret(newValue)

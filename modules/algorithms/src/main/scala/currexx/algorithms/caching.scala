package currexx.algorithms

import cats.effect.{Concurrent, Deferred, Ref}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

/** Runs `compute` at most once per key, with concurrent callers for the same key waiting on the first rather than repeating it.
  *
  * A search asks the same questions over and over - elites survive unchanged, crossover rediscovers its own parents, and a converged
  * population is mostly copies of a handful of individuals - so this is the difference between paying per candidate scored and paying per
  * *distinct* candidate scored. A `Deferred` per key rather than a plain map of results is what makes that true under the parallel
  * evaluation in `Op.EvaluatePopulation`: with a plain map the whole population starts the same computation before any of them finishes,
  * and the cache only begins working on the generation after.
  *
  * A failure is raised in every caller waiting on it and then evicted, so a transient error costs one round of work rather than poisoning
  * the key for the rest of the run.
  */
def memoize[F[_], K, V](compute: K => F[V])(using F: Concurrent[F]): F[K => F[V]] =
  Ref.of(Map.empty[K, Deferred[F, Either[Throwable, V]]]).map { cache =>
    def read(slot: Deferred[F, Either[Throwable, V]]): F[V] =
      slot.get.flatMap(F.fromEither)

    def fill(key: K, slot: Deferred[F, Either[Throwable, V]]): F[V] =
      compute(key).attempt
        .flatTap(result => slot.complete(result) >> F.whenA(result.isLeft)(cache.update(_ - key)))
        .flatMap(F.fromEither)

    // Claiming the key and finding it already claimed have to be one atomic step, or two callers both see an empty slot and both compute.
    def claim(key: K): F[V] =
      Deferred[F, Either[Throwable, V]].flatMap { slot =>
        cache
          .modify(entries => entries.get(key).fold((entries + (key -> slot), slot))(existing => (entries, existing)))
          .flatMap(winner => if (winner eq slot) fill(key, slot) else read(winner))
      }

    key => cache.get.flatMap(_.get(key).fold(claim(key))(read))
  }

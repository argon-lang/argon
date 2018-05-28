package com.mi3software.argon.util

import scalaz._
import Scalaz._

import scala.collection.immutable._

object MonadHelpers {

  def findFirst[A, B, F[_] : Monad, S[_]: Foldable](items: S[A])(f: A => F[Option[B]]): F[Option[B]] =
    items.foldLeftM(None : Option[B]) {
      case (result @ Some(_), _) => (result : Option[B]).point[F]
      case (None, item) => f(item)
    }

  def loadDependencyTree[F[_]: Monad, A, K : Equal, V]
  (item: A)
  (allDeps: Vector[A])
  (itemKey: A => Option[K])
  (itemDeps: A => Vector[K])
  (load: (A, Vector[Option[V]]) => F[V])
  : F[Option[V]] =
    loadDependencyTreeImpl(item)(allDeps)(itemKey)(itemDeps)(load).eval((Map.empty, Vector.empty))

  private def loadDependencyTreeImpl[F[_]: Monad, A, K : Equal, V]
  (item: A)
  (allDeps: Vector[A])
  (itemKey: A => Option[K])
  (itemDeps: A => Vector[K])
  (load: (A, Vector[Option[V]]) => F[V])
  : StateT[F, (Map[K, V], Vector[K]), Option[V]] =
    itemKey(item)
      .traverseM[StateT[F, (Map[K, V], Vector[K]), ?], V] { key =>
        handleDependencyCache(key) {
          itemDeps(item)
            .traverse[StateT[F, (Map[K, V], Vector[K]), ?], Option[V]] { dep =>
              allDeps.iterator
                .map { depItem => (itemKey(depItem), depItem) }
                .collectFirst {
                  case (Some(depKey), depItem) if depKey === dep => depItem
                }
                .traverseM[StateT[F, (Map[K, V], Vector[K]), ?], V] { depItem =>
                  loadDependencyTreeImpl(depItem)(allDeps)(itemKey)(itemDeps)(load)
                }
            }
            .flatMap { depItems =>
              implicitly[MonadTrans[StateT[?[_], (Map[K, V], Vector[K]), ?]]].liftM(load(item, depItems))
            }
        }
      }

  private def handleDependencyCache[F[_]: Monad, A, K : Equal, V]
  (key: K)
  (load: => StateT[F, (Map[K, V], Vector[K]), V])
  : StateT[F, (Map[K, V], Vector[K]), Option[V]] =
    State.get[(Map[K, V], Vector[K])].lift[F].flatMap { case (cache, loadingItems) =>
      cache.get(key) match {
        case Some(loadedDep) => (Some(loadedDep) : Option[V]).point[StateT[F, (Map[K, V], Vector[K]), ?]]
        case None =>
          if(loadingItems.any(key === _))
            (None : Option[V]).point[StateT[F, (Map[K, V], Vector[K]), ?]]
          else
            for {
              _ <- State.modify[(Map[K, V], Vector[K])] { case (cache, loadingItems) => (cache, loadingItems :+ key) }.lift[F]
              result <- load
              _ <- State.modify[(Map[K, V], Vector[K])] { case (cache, _) => (cache + (key -> result), loadingItems) }.lift[F]
            } yield Some(result)
      }
    }

}

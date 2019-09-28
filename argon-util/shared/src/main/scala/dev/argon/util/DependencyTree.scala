package dev.argon.util

import cats._
import cats.data._
import cats.implicits._

import scala.collection.immutable._

object DependencyTree {

  trait DependencyTreeOperations[LoadAction[_], Item, ItemKey, LoadedValue, LoadResult[_]] {
    def getItemKey(item: Item): ItemKey
    def getItemDependencies(item: Item): Vector[ItemKey]
    def loadItem(item: Item, dependencies: Vector[LoadedValue]): LoadAction[LoadedValue]
    def circularReferenceHandler(item: Item): LoadResult[LoadedValue]
    def missingDependencyHandler(item: Item, missingDepKey: ItemKey): LoadResult[LoadedValue]
  }



  def loadDependencyTree[LoadAction[_]: Monad, Item, ItemKey : Eq, LoadedValue, LoadResult[_] : Monad : Traverse]
  (ops: DependencyTreeOperations[LoadAction, Item, ItemKey, LoadedValue, LoadResult])
  (item: Item)
  (allDeps: Vector[Item])
  : LoadAction[LoadResult[LoadedValue]] =
    loadDependencyTreeImpl(item)(allDeps)(ops).runA((Map.empty, Vector.empty))

  def loadDependencies[LoadAction[_]: Monad, Item, ItemKey : Eq, LoadedValue, LoadResult[_] : Monad : Traverse]
  (ops: DependencyTreeOperations[LoadAction, Item, ItemKey, LoadedValue, LoadResult])
  (allDeps: Vector[Item])
  : LoadAction[Vector[LoadResult[LoadedValue]]] =
    allDeps
      .traverse { item =>
        loadDependencyTreeImpl(item)(allDeps)(ops)
      }
      .runA((Map.empty, Vector.empty))

  private def loadDependencyTreeImpl[LoadAction[_]: Monad, Item, ItemKey : Eq, LoadedValue, LoadResult[_] : Monad : Traverse]
  (item: Item)
  (allDeps: Vector[Item])
  (ops: DependencyTreeOperations[LoadAction, Item, ItemKey, LoadedValue, LoadResult])
  : StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), LoadResult[LoadedValue]] =
    handleDependencyCache(ops)(item)(ops.getItemKey(item)) {
      ops.getItemDependencies(item)
        .traverse { dep =>
          allDeps.iterator
            .map { depItem => (ops.getItemKey(depItem), depItem) }
            .collectFirst {
              case (depKey, depItem) if depKey === dep => depItem
            } match {
              case Some(depItem) =>
                loadDependencyTreeImpl(depItem)(allDeps)(ops)

              case None =>
                ops.missingDependencyHandler(item, dep).pure[StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), ?]]
            }
        }
        .flatMap { depItemResults =>
          StateT.liftF(
            depItemResults.sequence.traverse { depItems =>
              ops.loadItem(item, depItems)
            }
          )
        }
    }

  private def handleDependencyCache[LoadAction[_]: Monad, Item, ItemKey : Eq, LoadedValue, LoadResult[_] : Monad]
  (ops: DependencyTreeOperations[LoadAction, Item, ItemKey, LoadedValue, LoadResult])
  (item: Item)
  (key: ItemKey)
  (load: => StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), LoadResult[LoadedValue]])
  : StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), LoadResult[LoadedValue]] =
    StateT.get[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey])].flatMap { case (cache, loadingItems) =>
      cache.get(key) match {
        case Some(loadedDep) => loadedDep.pure[StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), ?]]
        case None =>
          if(loadingItems.exists(key === _))
            ops.circularReferenceHandler(item).pure[StateT[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey]), ?]]
          else
            for {
              _ <- StateT.modify[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey])] { case (cache, loadingItems) => (cache, loadingItems :+ key) }
              result <- load
              _ <- StateT.modify[LoadAction, (Map[ItemKey, LoadResult[LoadedValue]], Vector[ItemKey])] { case (cache, _) => (cache + (key -> result), loadingItems) }
            } yield result
      }
    }

}

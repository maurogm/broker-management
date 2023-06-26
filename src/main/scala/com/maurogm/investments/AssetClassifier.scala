package com.maurogm.investments

import com.maurogm.investments.currency.Money

type AssetCategory = String

trait AssetClassifier {
  def getCategory(asset: Asset): AssetCategory

}

class AssetClassifierFromMap(table: Map[Asset, AssetCategory])
    extends AssetClassifier {
  override def getCategory(asset: Asset): AssetCategory = table.getOrElse(asset, "UNKNOWN")
}

/** Since it is common to have maps with assets as keys, this TypeClass is
  * provided to (A) add it's category to each asset (turning the Map's key into
  * a tuple), and (B) aggregating grouping+reducing the map by AssetCategory; as
  * usual, a given instance must be provided for each new type of Value that
  * wants to be reduced.
  */
object AssetGrouperTypeClass {
  trait AssetGrouper[T] {
    def addAssetCategory(m: Map[Asset, T])(using
        ac: AssetClassifier
    ): Map[(AssetCategory, Asset), T] = m
      .groupBy(x => ac.getCategory(x._1))
      .map { case (cat, m) => m.map { case (asset, x) => (cat, asset) -> x } }
      .fold(Map.empty)(_ ++ _)
  }

  object AssetGrouperSyntax {
    extension [T](m: Map[Asset, T])
      def addAssetCategory(using
          instance: AssetGrouper[T],
          ac: AssetClassifier
      ): Map[(AssetCategory, Asset), T] =
        instance.addAssetCategory(m)
  }

  // step 1: TC definition
  trait AssetCategoryAggregator[T] extends AssetGrouper[T] {
    def aggByCategory(m: Map[Asset, T])(using
        ac: AssetClassifier
    ): Map[AssetCategory, T]
  }

  // step 2: TC instances
  given moneyGrouper: AssetCategoryAggregator[Money] with {
    override def aggByCategory(m: Map[Asset, Money])(using
        ac: AssetClassifier
    ): Map[AssetCategory, Money] =
      addAssetCategory(m).groupMapReduce(_._1._1)(_._2)(_ + _)
  }

  // step 3: "user-facing" API
  object AssetCategoryAggregator {
    // often similar to what the type class definition offers
    def aggByCategory[T](m: Map[Asset, T])(using
        instance: AssetCategoryAggregator[T],
        ac: AssetClassifier
    ): Map[AssetCategory, T] = instance.aggByCategory(m)

    def apply[T](using
        instance: AssetCategoryAggregator[T]
    ): AssetCategoryAggregator[T] = instance
  }

  // step 4: extension methods
  object AssetCategoryAggregatorSyntax {
    extension [T](m: Map[Asset, T])
      def aggByCategory(using
          instance: AssetCategoryAggregator[T],
          ac: AssetClassifier
      ): Map[AssetCategory, T] =
        instance.aggByCategory(m)
  }
}

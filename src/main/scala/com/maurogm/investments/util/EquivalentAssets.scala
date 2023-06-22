package com.maurogm.investments.util

import com.maurogm.investments.Asset

trait EquivalentAssets {
  val equivalentAssetsDict = Map(
    Asset("BCBA", "MGCBC") -> Asset("BCBA", "MGCBO"),
    Asset("BCBA", "AL29D") -> Asset("BCBA", "AL29"),
    Asset("BCBA", "AL30D") -> Asset("BCBA", "AL30"),
    Asset("BCBA", "AE38D") -> Asset("BCBA", "AE38"),
    Asset("BCBA", "AL41D") -> Asset("BCBA", "AL41"),
    Asset("BCBA", "AL35D") -> Asset("BCBA", "AL35"),
    Asset("BCBA", "GD29D") -> Asset("BCBA", "GD29"),
    Asset("BCBA", "GD30D") -> Asset("BCBA", "GD30"),
    Asset("BCBA", "GD38D") -> Asset("BCBA", "GD38"),
    Asset("BCBA", "GD46D") -> Asset("BCBA", "GD46"),
    Asset("BCBA", "GD41D") -> Asset("BCBA", "GD41"),
    Asset("BCBA", "GD35D") -> Asset("BCBA", "GD35"),
    Asset("BCBA", "AL30C") -> Asset("BCBA", "AL30"),
    Asset("BCBA", "AL29C") -> Asset("BCBA", "AL29"),
    Asset("BCBA", "AE38C") -> Asset("BCBA", "AE38"),
    Asset("BCBA", "AL41C") -> Asset("BCBA", "AL41"),
    Asset("BCBA", "AL35C") -> Asset("BCBA", "AL35"),
    Asset("BCBA", "GD29C") -> Asset("BCBA", "GD29"),
    Asset("BCBA", "GD30C") -> Asset("BCBA", "GD30"),
    Asset("BCBA", "GD38C") -> Asset("BCBA", "GD38"),
    Asset("BCBA", "GD46C") -> Asset("BCBA", "GD46"),
    Asset("BCBA", "GD35C") -> Asset("BCBA", "GD35"),
    Asset("BCBA", "GD41C") -> Asset("BCBA", "GD41")
  )
}

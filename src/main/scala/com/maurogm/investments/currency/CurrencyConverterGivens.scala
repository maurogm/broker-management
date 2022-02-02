package com.maurogm.investments.currency

import com.maurogm.investments.etl.GDRReader
import com.maurogm.investments.{Asset, GDR}

import java.time.LocalDate

object CurrencyConverterGivens {

  given mepGD30: CurrencyConverter = CurrencyFactory.fromAssetRatio(
    Asset("BCBA", "GD30"),
    Asset("BCBA", "GD30D"),
    "USD-MEP",
    Some(Set("USD", "MEP-GD30"))
  )

  given mepAL30: CurrencyConverter = CurrencyFactory.fromAssetRatio(
    Asset("BCBA", "AL30"),
    Asset("BCBA", "AL30D"),
    "USD-MEP",
    Some(Set("USD", "MEP-AL30"))
  )

  given cclGD30: CurrencyConverter = CurrencyFactory.fromAssetRatio(
    Asset("BCBA", "GD30"),
    Asset("BCBA", "GD30C"),
    "USD-CCL",
    Some(Set("USD", "CCL-GD30"))
  )

  given cclAL30: CurrencyConverter = CurrencyFactory.fromAssetRatio(
    Asset("BCBA", "AL30"),
    Asset("BCBA", "AL30C"),
    "USD-CCL",
    Some(Set("USD", "CCL-AL30"))
  )

  given cclGGAL: CurrencyConverter = CurrencyFactory.fromGDR(
    GDRReader.getByAsset(Asset("NYSE", "GGAL")),
    "USD-CCL",
    toGDRsCurrency = false,
    Some(Set("USD", "CCL-GGAL"))
  )

  given customCCL: CurrencyConverter = cclGGAL
    .addOrUpdate(
      cclAL30
        .trimDates(
          minDate = LocalDate.parse("2020-09-14"),
          maxDate = LocalDate.parse("2021-11-13")
        )
    )
    .addOrUpdate(cclGD30.trimDates(minDate = LocalDate.parse("2020-09-14")), newSymbol = Some("CCL"))
}

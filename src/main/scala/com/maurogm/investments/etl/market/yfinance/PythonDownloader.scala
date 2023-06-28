package com.maurogm.investments.etl.market.yfinance

import com.maurogm.investments.Asset

import scala.sys.process.*

object PythonDownloader {
  val OUTPUT_DIR = "src/main/resources/market"
  val PATH_TO_DOWNLOAD_SCRIPT = "src/main/python/downloadTickerHistory.py"

  def downloadAssets(assetsToDownload: Iterable[Asset]) = {
    val commands = assetsToDownload.map { case Asset(exchange, ticker) =>
      s"${executionCommand(exchange, ticker)}"
    }
    for {
      asset <- assetsToDownload
      command = executionCommand(asset.exchange, asset.ticker)
      _ = println(s"Downloading $asset")
    } yield {

      command.!!
    }
  }

  private def executionCommand(exchange: String, ticker: String): String =
    s"src/main/python/venv/bin/python3 $PATH_TO_DOWNLOAD_SCRIPT --ticker $ticker --output_path ./$OUTPUT_DIR/$exchange/$ticker.csv"
}

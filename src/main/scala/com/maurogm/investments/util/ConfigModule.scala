package com.maurogm.investments.util

import com.typesafe.config.{Config, ConfigFactory}

trait ConfigModule {
  val config: Config = ConfigFactory.load()
  val secrets: Config = ConfigFactory.load("secrets.conf")
  //val assetsOfInterest: Config = ConfigFactory.load("assetsOfInterest.conf") //is this used anywhere?
}

package com.maurogm.brokers.util

import com.typesafe.config.{Config, ConfigFactory}

trait ConfigModule {
  val config: Config = ConfigFactory.load()
  val secrets: Config = ConfigFactory.load("secrets.conf")

}

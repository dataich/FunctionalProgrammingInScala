package com.dataich.state

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by dataich on 3/28/16.
  */
class StateSpec extends WordSpec with Matchers {

  "RNG" should {
    "EXERCISE 6.1 nonNegativeInt" in {
      RNG.nonNegativeInt(RNG.Simple(1L)) shouldBe(384748, RNG.Simple(25214903928L))
    }
  }
}

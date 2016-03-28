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

    "EXERCISE 6.2 double" in {
      RNG.double(RNG.Simple(1L)) shouldBe(1.7916224896907806E-4, RNG.Simple(25214903928L))
    }

    "EXERCISE 6.3 intDouble doubleInt double3" in {
      RNG.intDouble(RNG.Simple(1L)) shouldBe((384748, -0.5360936461947858), RNG.Simple(206026503483683L))
      RNG.doubleInt(RNG.Simple(1L)) shouldBe((-0.5360936461947858, 384748), RNG.Simple(206026503483683L))
      RNG.double3(RNG.Simple(1L)) shouldBe((1.7916224896907806E-4, -0.5360936461947858, -0.25582678942009807), RNG.Simple(245470556921330L))
    }
  }
}

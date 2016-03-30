package com.dataich.state

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by dataich on 3/28/16.
  */
class StateSpec extends WordSpec with Matchers {

  "EXERCISE 6.1" should {
    "nonNegativeInt" in {
      RNG.nonNegativeInt(RNG.Simple(1L)) shouldBe(384748, RNG.Simple(25214903928L))
    }
  }

  "EXERCISE 6.2" should {
    "double" in {
      RNG.double(RNG.Simple(1L)) shouldBe(1.7916224896907806E-4, RNG.Simple(25214903928L))
    }
  }

  "EXERCISE 6.3" should {
    "intDouble" in {
      RNG.intDouble(RNG.Simple(1L)) shouldBe((384748, -0.5360936461947858), RNG.Simple(206026503483683L))
    }
    "doubleInt" in {
      RNG.doubleInt(RNG.Simple(1L)) shouldBe((-0.5360936461947858, 384748), RNG.Simple(206026503483683L))
    }
    "double3" in {
      RNG.double3(RNG.Simple(1L)) shouldBe((1.7916224896907806E-4, -0.5360936461947858, -0.25582678942009807), RNG.Simple(245470556921330L))
    }
  }

  "EXERCISE 6.4" should {
    "ints" in {
      RNG.ints(3)(RNG.Simple(1L)) shouldBe(List(384748, -1151252339, -549383847), RNG.Simple(245470556921330L))
    }
  }

  "EXERCISE 6.5" should {
    "doubleImproved" in {
      RNG.doubleImproved(RNG.Simple(1L)) shouldBe(1.7916224896907806E-4, RNG.Simple(25214903928L))
    }
  }
}

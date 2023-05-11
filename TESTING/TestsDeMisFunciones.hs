module TestsDeMisFunciones where

import Test.HUnit
import MisFunciones

--casos de tests

run = runTestTT tests

tests = test [
    "doblede3" ~: doble 3 ~?= 6,
    "doblede2" ~: doble 2 ~?= 4
             ]


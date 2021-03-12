
module Main where

import TestParsing ( testParsing )
import TestEquivalences ( testEquivalences )
import TestSynthesis ( testSynthesis )
import TestTypeCheck ( testTypeCheck )
import TestLiveness ( testLiveness )
import TestReconstruction ( testReconstruction )
import TestDriving ( testDriving )
import TestPropagate ( testPropagate )

main :: IO ()
main =
  do
    -- testParsing
    -- testEquivalences
    -- testTypeCheck
    testLiveness
    testSynthesis
    -- testReconstruction
    testDriving
    testPropagate

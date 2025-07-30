{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import SquareRoot (root)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "root" $ for_ cases test
  where

    test Case{..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion   = isLeapYear (fromIntegral input) `shouldBe` expected

data Case = Case { description :: String
                 , input       :: Integer
                 , expected    :: Integer
                 }

cases :: [Case]
cases = [ Case { description = "square root of 1"
               , input       = 1
               , expected    = 1
               }
        , Case { description = "square root of 4"
               , input       = 4
               , expected    = 2
               }
        , Case { description = "square root of 25"
               , input       = 25
               , expected    = 5
               }
        , Case { description = "square root of 81"
               , input       = 81
               , expected    = 9
               }
        , Case { description = "square root of 196"
               , input       = 196
               , expected    = 14
               }
        , Case { description = "square root of 65025"
               , input       = 65025
               , expected    = 255
               }
        ]

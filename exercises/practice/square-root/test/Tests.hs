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
cases = [ Case { description = "year not divisible by 4 in common year"
               , input       = 1
               , expected    = 1
               }
        , Case { description = "year divisible by 2, not divisible by 4 in common year"
               , input       = 4
               , expected    = 2
               }
        , Case { description = "year divisible by 4, not divisible by 100 in leap year"
               , input       = 25
               , expected    = 5
               }
        , Case { description = "year divisible by 4 and 5 is still a leap year"
               , input       = 81
               , expected    = 9
               }
        , Case { description = "year divisible by 100, not divisible by 400 in common year"
               , input       = 196
               , expected    = 14
               }
        , Case { description = "year divisible by 100 but not by 3 is still not a leap year"
               , input       = 65025
               , expected    = 255
               }
        ]
module Test.Gen where

import Test.QuickCheck.Gen ( Gen , elements , vectorOf , chooseInt )
import Test.QuickCheck.Arbitrary ( arbitrary )


genSmallInt :: Gen Int
genSmallInt = chooseInt ( 0 , 6 )

genSmallList :: Gen a -> Gen [ a ]
genSmallList g = genSmallInt >>= flip vectorOf g

genSmallString :: Gen String
genSmallString = genSmallInt >>= flip vectorOf arbitrary

genFunction1 :: Gen ( Int -> Int )
genFunction1 = genSmallInt >>= \ n -> elements
  [ id
  , const n
  , subtract n
  , (+ n)
  , (* n)
  , (n -)
  ]

genFunction2 :: Gen ( Int -> Int -> Int )
genFunction2 = elements
  [ const
  , (+)
  , (*)
  , (-)
  ]

genPredicate :: Gen ( String -> Bool )
genPredicate = elements [ null , (>=3) . length , (<=5) . length ]

genPredicateInt :: Gen ( Int -> Bool )
genPredicateInt = elements [ (>3) , (<3) , (<6) , (==0) , (>0) ]

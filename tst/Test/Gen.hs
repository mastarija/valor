module Test.Gen where
--
import Data.Valor.Internal ( Wrong (..) )
import Test.QuickCheck.Gen ( Gen , oneof , elements , vectorOf , chooseInt )
import Test.QuickCheck.Arbitrary ( arbitrary )
--

genWrong :: Gen a -> Gen ( Wrong a )
genWrong gen = oneof
  [ Inert <$> gen
  , Wrong <$> gen
  ]

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

import Test.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.State 

prop :: Int -> Property
prop x = if x > 42 then property False else False ==> True

cb  = PostFinalFailure NotCounterexample (\MkState{numDiscardedTests = n} _ -> 
                                              print $ "Number Of Discards: " ++ show n)

prop_cb = callback cb prop

main = quickCheck prop_cb

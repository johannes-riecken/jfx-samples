{-# OPTIONS_GHC -Wall #-}
import Test.Hspec
import MaxDataConverter

main :: IO ()
main = hspec $ do
    it "handles ints" $ do
        let got = translate "public class MaxData { public int materials; }"
        got `shouldBe` Right "data MaxData = MaxData { mdMaterials :: Int }"

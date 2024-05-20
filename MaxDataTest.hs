{-# OPTIONS_GHC -Wall #-}
import Test.Hspec
import MaxDataConverter

main :: IO ()
main = hspec $ do
    it "handles ints" $ do
        let got = translate "public class MaxData { public int materials; }"
        got `shouldBe` Right "data MaxData = MaxData { mdMaterials :: Int }"
    it "handles arrays" $ do
        let got = translate "public class InA { public int[] numbers; }"
        got `shouldBe` Right "data InA = InA { iaNumbers :: [Int] }"
    it "handles short class names by taking the first three letters" $ do
        let got = translate "public class Mesh { public String name; }"
        got `shouldBe` Right "data Mesh = Mesh { mesName :: String }"
    it "translates HashMap to Map" $ do
        let got = translate "public class InB { public Map<String, Integer> nodes = new HashMap<>(); }"
        got `shouldBe` Right "data InB = InB { ibNodes :: Map String Int }"

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
    it "translates List to []" $ do
        let got = translate "public class InC { public List<Integer> numbers; }"
        got `shouldBe` Right "data InC = InC { icNumbers :: [Int] }"
    it "translates static classes by individual translation" $ do
        let got = translate "public class InD { public static class Inner { public int foo; } public int bar; }"
        got `shouldBe` Right "data InD = InD { idBar :: Int }\ndata Inner = Inner { innFoo :: Int }"
    it "translates inheritance to composition" $ do
        let got = translate "public class InE { public static class Foo { public int bar; } public static class Baz extends Foo { public int quux; } public int foobar; }"
        got `shouldBe` Right "data InE = InE { ieFoobar :: Int }\ndata Foo = Foo { fooBar :: Int }\ndata Baz = Baz { bazQuux :: Int, bazBaseFoo :: Foo }"

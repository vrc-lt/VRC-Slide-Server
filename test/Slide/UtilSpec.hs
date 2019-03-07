module Slide.UtilSpec where

import Test.Hspec
import Slide.Util
import Slide.Model

slides = [Slide "id1" 13, Slide "id2" 42]

slideNums = map slideCount slides

spec :: Spec
spec = do
  describe "getSlideId" $ do
    it "表示するページが0の時、一つめのスライドIDが返る" $ do
          getSlideId slides 0 `shouldBe` Just "id1"
    it "表示するページが12以下の時、一つめのスライドIDが返る" $ do
      getSlideId slides 12 `shouldBe` Just "id1"
    it "表示するページが13の時、２つめのスライドIDが返る" $ do
      getSlideId slides 13 `shouldBe` Just "id2"
    it "表示するページが14以上42未満の時、２つめのスライドIDが返る" $ do
      getSlideId slides 16 `shouldBe` Just "id2"
  describe "getSlidePage" $ do
    it "表示するページが0の時、0が返る" $ do
          getSlidePage slideNums 0 `shouldBe` 0
    it "表示するページが12以下の時、同じ数が返る" $ do
      getSlidePage slideNums 12 `shouldBe` 12
    it "表示するページが13の時、0が返る" $ do
      getSlidePage slideNums 13 `shouldBe` 0
    it "表示するページが14以上42未満の時、１つ目のスライド枚数分を引いた数が返る" $ do
      getSlidePage slideNums 16 `shouldBe` 3

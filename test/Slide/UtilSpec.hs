module Slide.UtilSpec where

import Test.Hspec
import Slide.Util

slideIds = ["id1", "id2"]
slideNums = [13, 42]

spec :: Spec
spec = do
  describe "getSlideId" $ do
    it "表示するページが0の時、一つめのスライドIDが返る" $ do
          getSlideId slideIds slideNums 0 `shouldBe` Just "id1"
    it "表示するページが12以下の時、一つめのスライドIDが返る" $ do
      getSlideId slideIds slideNums 12 `shouldBe` Just "id1"
    it "表示するページが13の時、２つめのスライドIDが返る" $ do
      getSlideId slideIds slideNums 13 `shouldBe` Just "id2"
    it "表示するページが14以上42未満の時、２つめのスライドIDが返る" $ do
      getSlideId slideIds slideNums 13 `shouldBe` Just "id2"

module DecryptSpec (spec) where

import Test.Hspec ( describe, it, shouldReturn, Spec, shouldBe )
import Decrypt ( isPDF, isEncrypted )


spec :: Spec
spec = do
    describe "isPDF" $ do
        it "pdfの場合" $ do
            isPDF "test.pdf" `shouldBe` True

    describe "isEncrypted" $ do
        it "存在しないパスの場合" $ do
            isEncrypted "invailde/path/to/file.pdf" `shouldReturn` (False, Nothing)

        it "パスでない文字列を渡した場合" $ do
            isEncrypted "test" `shouldReturn` (False, Nothing)

        it "pdfでない場合" $ do
            isEncrypted "test/decryptSpec.hs" `shouldReturn` (False, Nothing)
        
        it "暗号化されたpdfの場合" $ do
            result <- isEncrypted "test/test_data/geka05.pdf"
            fst result `shouldBe` True
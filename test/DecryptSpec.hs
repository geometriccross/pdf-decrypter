module DecryptSpec (spec) where

import Test.Hspec ( describe, it, shouldReturn, Spec )
import Decrypt

spec :: Spec
spec = do
    describe "isEncrypted" $ do
        it "存在しないパスの場合" $ do
            isEncrypted "invailde/path/to/file.pdf" `shouldReturn` (False, Nothing)

        it "パスでない文字列を渡した場合" $ do
            isEncrypted "test" `shouldReturn` (False, Nothing)

        it "pdfでない場合" $ do
            isEncrypted "test/decryptSpec.hs" `shouldReturn` (False, Nothing)
module DecryptSpec (spec) where

import Test.Hspec ( describe, it, shouldReturn, Spec, shouldBe )
import Decrypt ( isPDF, isEncrypted, decrypt )
import Data.Maybe (Maybe(Nothing))
import Data.Bool (Bool(True, False))
import System.Directory (copyFile)


spec :: Spec
spec = do
    describe "isPDF" $ do
        it "存在しないパスの場合" $ do
            isPDF "invailde/path/to/file.pdf" `shouldReturn` False

        it "パスでない文字列を渡した場合" $ do
            isPDF "test" `shouldReturn` False

        it "pdfでない場合" $ do
            isPDF "test/decryptSpec.hs" `shouldReturn` False

        it "pdfの場合" $ do
            isPDF "test/test_data/normal.pdf" `shouldReturn` True

    describe "isEncrypted" $ do
        it "暗号化されていないpdfの場合" $ do
            isEncrypted "test/test_data/normal.pdf" `shouldReturn` False

        it "暗号化されたpdfの場合" $ do
            isEncrypted "test/test_data/encrypted.pdf" `shouldReturn` True
        
    describe "decrypt" $ do
        it "パスワードが間違っている場合" $ do
            decrypt "test/test_data/encrypted.pdf" ["wrong"]
            isEncrypted "test/test_data/encrypted.pdf" `shouldReturn` True

        it "パスワードが正しい場合" $ do
            let copy_path = "test/test_data/encrypted_copy.pdf"
            copyFile "test/test_data/encrypted.pdf" copy_path
            decrypt copy_path ["password"]
            isEncrypted copy_path `shouldReturn` Nothing
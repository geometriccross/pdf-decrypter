module DecryptSpec (spec) where

import           Decrypt          (decrypt, checkFileInfo, isPDF)
import           System.Directory (removeFile)
import           Test.Hspec       (Spec, describe, it, shouldReturn)

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
            checkFileInfo "test/test_data/normal.pdf" "-Encryption" `shouldReturn` False

        it "暗号化されたpdfの場合" $ do
            checkFileInfo "test/test_data/encrypted.pdf" "-Encryption" `shouldReturn` True

    describe "decrypt" $ do
        it "パスワードが間違っている場合" $ do
            decrypt "test/test_data/encrypted.pdf" "test/test_data/out.pdf" ["wrong"]
            checkFileInfo "test/test_data/encrypted.pdf" "-Encryption" `shouldReturn` True

        it "パスワードが正しい場合" $ do
            let out_path = "test/test_data/out.pdf"
            decrypt "test/test_data/encrypted.pdf" out_path ["password"]
            checkFileInfo out_path "-Encryption" `shouldReturn` False
            removeFile out_path

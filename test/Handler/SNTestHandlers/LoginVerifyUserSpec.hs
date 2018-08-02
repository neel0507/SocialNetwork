module Handler.SNTestHandlers.LoginVerifyUserSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "putLoginVerifyUserR" $ do
        it "requests the user name" $ do
          get HomepageR
          statusIs 200


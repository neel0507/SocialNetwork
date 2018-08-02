module Handler.SNTestHandlers.RegisterVerifyUserSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "putRegisterVerifyUserR" $ do
        it "requests the username" $ do
          get MessagesR
          statusIs 200


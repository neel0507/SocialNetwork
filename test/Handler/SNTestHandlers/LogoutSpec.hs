module Handler.SNTestHandlers.LogoutSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getLogoutR" $ do
        it "requests the log out page" $ do
          get SignupR
          statusIs 200


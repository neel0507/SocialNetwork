module Handler.SNTestHandlers.SettingsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getSettingsR" $ do
        it "requests the settings" $ do
          get SettingsR
          statusIs 200


    describe "postSettingsR" $ do
        it "requests the settings" $ do
          get SettingsR
          statusIs 200


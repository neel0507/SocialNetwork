module Handler.SNTestHandlers.HomepageSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
        it "requests the home page" $ do
          get HomepageR
          statusIs 200


module Handler.SNTestHandlers.FriendsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Friends" $ do
        it "requests the friends page" $ do
          get FriendsR
          statusIs 200


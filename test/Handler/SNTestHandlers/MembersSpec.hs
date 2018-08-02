module Handler.SNTestHandlers.MembersSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMembersR" $ do
        it "requests the user name" $ do
          get MembersR
          statusIs 200


    describe "postMembersR" $ do
        it "requests the user name" $ do
          get MembersR
          statusIs 200


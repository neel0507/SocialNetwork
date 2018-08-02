module Handler.SNTestHandlers.SignupSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getSignupR" $ do
        it "requests the sign up" $ do
          get SignupR
          statusIs 200


    describe "postSignupR" $ do
        it "posts the sign up" $ do
          get SignupR
          statusIs 200


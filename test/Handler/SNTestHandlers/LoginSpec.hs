module Handler.SNTestHandlers.LoginSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

     describe "getLoginR" $ do
        it "requests the login page" $ do
          get (LoginpageR LoginR)
          statusIs 200


     describe "postLoginR" $ do
        it "posts the login page" $ do
          get (LoginpageR LoginR)
          statusIs 200


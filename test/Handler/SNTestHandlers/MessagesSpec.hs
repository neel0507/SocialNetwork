module Handler.SNTestHandlers.MessagesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMessagesR" $ do
        it "requests the messages" $ do
          get MessagesR
          statusIs 200


    describe "postMessagesR" $ do
        it "requests the messages" $ do
          get MessagesR
          statusIs 200


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SNTestHandlers.SettingsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "SettingsR" $ do
        it "Accessing settings page when there is no profile message" $ do
          (uname, password) <- doSignup "Neel1" "1234"          
          doLogin uname password
          get SettingsR
          statusIs 200
          bodyContains "Your profile message: No message yet"

        it "Accessing settings page and posting a profile message" $ do
          (uname, password) <- doSignup "Neel1" "1234"         
          doLogin uname password
          get SettingsR
          statusIs 200
          postProfileMessage "My profile looks awesome"
          get SettingsR
          statusIs 200
          bodyContains "Your profile message: My profile looks awesome"

        it "Accessing settings page and posting multiple profile messages" $ do
          (uname, password) <- doSignup "Neel1" "1234"          
          doLogin uname password
          get SettingsR
          statusIs 200
          postProfileMessage "My profile looks awesome"
          get SettingsR
          statusIs 200
          postProfileMessage "My profile looks more awesome"
          get SettingsR
          statusIs 200
          bodyContains "Your profile message: My profile looks more awesome"


{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SNTestHandlers.MessagesSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "getMessagesR" $ do
        it "Accessing messages page when there is no message set" $ do
          (uname, password) <- doSignup "Neel" "1234"          
          doLogin uname password
          get MessagesR
          statusIs 200
          bodyContains "No messages yet"
        
        it "Accessing messages page and posting a message" $ do
          (uname, password) <- doSignup "Neel" "1234"          
          doLogin uname password
          get MessagesR
          statusIs 200
          postPrivateMessage "My message"
          get MessagesR
          statusIs 200
          bodyContains "06/08/2018"
          bodyContains "whispered"
          bodyContains "My message"                   
          postPublicMessage "My another message"
          get MessagesR
          statusIs 200
          bodyContains "06/08/2018"
          bodyContains "wrote"
          bodyContains "My another message"
          htmlCount ".listItem" 2
          request $ do
             setUrl ("http://localhost:3000/messages?erase=1" :: Text) --remove "My message"
          bodyNotContains "My message"
          htmlCount ".listItem" 1

    describe "postMessagesR" $ do
       it "Accessing another user's messages page and posting a message" $ do
          (uname1, password1) <- doSignup "Neel" "1234"
          (uname2, password2) <- doSignup "Neel2" "12345"          
          doLogin uname1 password1
          get MembersR
          statusIs 200
          get (ViewMemberR uname2)
          statusIs 200
          get (ViewMemberMessagesR uname2)
          statusIs 200
          bodyContains "No messages yet"
          postPrivateMessageToMember "Hi Neel2" uname2
          get LogoutpageR
          doLogin uname2 password2
          get MessagesR
          bodyContains "Hi Neel2"   

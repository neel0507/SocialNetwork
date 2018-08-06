{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.SNTestHandlers.HomepageSpec (spec) where

import TestImport
import qualified Data.List as L

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
      it "Accessing homepage without logging in" $ do
        get HomepageR
        statusIs 200
        bodyContains "You must be logged in to view this page."

      it "Accessing homepage with valid credentials" $ do
        (uname, password) <- doSignup "Neel" "12345"               
        doLogin uname password
        get HomepageR
        statusIs 200
        bodyContains "Neel"

      it "Accessing site with invalid username" $ do
        (uname, password) <- doSignup "Neel" "12345"        
        doLogin "Neel1" password --should be Neel        
        statusIs 200
        bodyContains "Invalid User"

      it "Accessing site with invalid password" $ do
        (uname, password) <- doSignup "Neel" "12345"        
        doLogin uname "1234" --should be 12345        
        statusIs 200
        bodyContains "Invalid User"

    describe "verifyUserR" $ do
       it "Accessing json data of sign up page" $ do
         createUser "Neel" "12345"
         get SignupR
         request $ do
           setMethod "GET"        
           setUrl (RegisterVerifyUserR "Neel")
           addRequestHeader ("Content-type", "application/json")
         bodyContains "exists"
         request $ do
           setMethod "GET"        
           setUrl (RegisterVerifyUserR "Neel1")
           addRequestHeader ("Content-type", "application/json")
         bodyContains "is available"

       it "Accessing json data of login page" $ do
         createUser "Neel" "12345"
         get LoginpageR
         request $ do
           setMethod "GET"        
           setUrl (LoginVerifyUserR "Neel")
           addRequestHeader ("Content-type", "application/json")
         bodyContains "a valid username"
         request $ do
           setMethod "GET"        
           setUrl (LoginVerifyUserR "Neel1")
           addRequestHeader ("Content-type", "application/json")
         bodyContains "an invalid username"
         
          


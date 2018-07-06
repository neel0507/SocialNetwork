{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where

import Import
import Yesod.Form (runInputPost, textField, ireq)

getSignupR :: Handler Html
getSignupR = do    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/signup")

postSignupR :: Handler Html
postSignupR = do
    ident <- runInputPost $ ireq textField "ident"
    password <- runInputPost $ ireq textField "password"
    
    existingUser <- runDB $ getBy $ UniqueUser ident

    existingUserId <- case existingUser of
         Nothing -> 
          runDB $ insert $ User 
           { userIdent = ident
           , userPassword = password
           }       
         Just (Entity userId _) -> return userId  

    existingMember <- runDB $ getBy $ UniqueMember existingUserId
    _ <- case existingMember of
         Nothing ->
          runDB $ insert $ Member
           { memberUserId = existingUserId 
           , memberIdent = ident
           }
         Just (Entity userId _) -> return userId
 
    defaultLayout $ do
      [whamlet|
       <ul class="menu">
        <li><a href=@{HomepageR}>Home </a>
        <li><a href=@{SignupR}>Signup </a>
        <li><a href=@{LoginpageR}>Log In </a>
       <br>
       <div class="message"><b>Account for user #{ident} created.</b>
       <br>   
       <div class="message"><b>Please enter your details to login.</b>
      |]


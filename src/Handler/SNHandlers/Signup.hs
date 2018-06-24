{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where

import Import
import Yesod.Form (runInputPost, textField, ireq)
--import Yesod.Core

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
    _ <- case existingUser of
         Nothing -> runDB $ insert $ User 
          { userIdent = ident
          , userPassword = password
          }
         Just (Entity userId _) -> return userId
    defaultLayout $ do
      [whamlet|
       <ul class="menu">
        <li><a href=@{HomepageR}>Home </a>
        <li><a href=@{LoginpageR}>Log In </a>
        <li><a href=@{MembersR}>Members </a>
        <li><a href=@{FriendsR}>Friends </a>
        <li><a href=@{MessagesR}>Messages </a>
        <li><a href=@{SettingsR}>Settings </a>
        <li><a href=@{LogoutpageR}>Log Out </a>
       <br>
       <div class="message"><b>Welcome to Social Network, #{ident}</b>
       <br>   
       <div class="message"><b>Please enter your details to login</b>
      |]
         
     


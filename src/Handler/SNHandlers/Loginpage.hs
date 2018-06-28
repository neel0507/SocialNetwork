{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import
import Database.Persist.Sql

getLoginpageR :: Handler Html
getLoginpageR = do
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/login")

postLoginpageR :: Handler Html
postLoginpageR = do
    user <- runInputPost $ ireq textField "ident"
    password <- runInputPost $ ireq textField "password"
    
    existingUser <- runDB $ getBy $ UniqueUser user    
    let existingUserId = case existingUser of
         Just (Entity userId _) -> fromSqlKey $ userId
         Nothing -> 0 :: Int64      
    let isValid = case existingUser of
         Nothing -> False
         Just (Entity _ sqlUser) -> (unpack password) == (unpack (userPassword sqlUser))
        
    if isValid
        then     do
             runDB $ updateWhere [MemberUserId !=. (mUserId existingUserId)] [MemberLoggedInUser =. False]
             runDB $ updateWhere [MemberUserId ==. (mUserId existingUserId)] [MemberLoggedInUser =. True]              
             defaultLayout $ do
                  [whamlet|
                   <ul class="menu">
                    <li><a href=@{HomepageR}>Home </a>
                    <li><a href=@{MembersR}>Members </a>
                    <li><a href=@{FriendsR}>Friends </a>
                    <li><a href=@{MessagesR}>Messages </a>
                    <li><a href=@{SettingsR}>Settings </a>
                    <li><a href=@{LogoutpageR}>Log Out </a>
                   <br>
                   <div class="message"><b>Welcome to Social Network, #{user}</b>
                  |]
        else     
             defaultLayout $ do
                  [whamlet| <p>Invalid User |]

mUserId :: Int64 -> Key User
mUserId userId = toSqlKey $ userId

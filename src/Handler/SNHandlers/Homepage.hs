{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Homepage where

import Import
import Database.Persist.Sql

getHomepageR :: Handler Html
getHomepageR = do
    uid <- lookupSession "User_Id" --Identify the user
    sessUserId <- getMemberId uid  --Get userid stored in the session

    if sessUserId > 0
        then do            
            memberEntity <- getUniqueMember $ getUserKey sessUserId --Identify the member with the help of session userid
            username <- getMemberName memberEntity "Does not exist" --Identify the username with the help of member entity
            defaultLayout $ do
              $(widgetFile "SNTemplates/validUser") --Display welcome message with userame             
        else
            defaultLayout $ do
              $(widgetFile "SNTemplates/homepage") -- User is not logged in      

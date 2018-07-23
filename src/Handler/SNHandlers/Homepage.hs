{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Homepage where

import Import
import Database.Persist.Sql

getHomepageR :: Handler Html
getHomepageR = do
    uid <- lookupSession "User_Id"
    sessUserId <- getMemberId uid

    if sessUserId > 0
        then do            
            memberEntity <- getUniqueMember $ getUserKey sessUserId
            username <- getMemberName memberEntity "Does not exist"
            defaultLayout $ do
              $(widgetFile "SNTemplates/validUser")              
        else
            defaultLayout $ do
              $(widgetFile "SNTemplates/homepage")        

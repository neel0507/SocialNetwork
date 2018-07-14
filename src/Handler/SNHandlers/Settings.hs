{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Settings where

import Import
import Database.Persist.Sql


getSettingsR :: Handler Html
getSettingsR = do
    uid <- lookupSession "_ID"
    sessUserId <- getMemberId uid

    if sessUserId > 0
       then do
           let memberKey = getMemberKey sessUserId

           existingMessage <- getUniqueProfileMessage memberKey
           message <- getProfileMessage existingMessage "No Message Yet"

           defaultLayout $ do
              addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
              $(widgetFile "SNTemplates/settings")
              
       else
           redirect LoginpageR

postSettingsR :: Handler Html
postSettingsR = do
    uid <- lookupSession "_ID"    
    sessUserId <- getMemberId uid

    if sessUserId > 0
       then do
           message <- runInputPost $ ireq textareaField "txtarea"

           let memberKey = getMemberKey sessUserId

           existingMessage <- getUniqueProfileMessage memberKey

           updatedMessage <- case existingMessage of
                 Just (Entity _ _) -> updateMessage memberKey message
                 Nothing -> return messageNotUpdated

           _ <- insertMessage updatedMessage memberKey message

           defaultLayout $ do
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
             $(widgetFile "SNTemplates/settings")      
             
       else
           redirect LoginpageR   


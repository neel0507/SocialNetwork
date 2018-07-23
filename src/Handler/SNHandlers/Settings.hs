{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Settings where

import Import
import Database.Persist.Sql
import           Text.Julius             (juliusFile)

getSettingsR :: Handler Html
getSettingsR = do
    (userId, user) <- requireAuthPair      
    let loggedInUserId = fromSqlKey userId
    let memberKey = getMemberKey loggedInUserId

    existingMessage <- getUniqueProfileMessage memberKey
    message <- getProfileMessage existingMessage "No message Yet"

    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/settings")
       toWidget $(juliusFile "templates/SNTemplates/messages.julius")
    
postSettingsR :: Handler Html
postSettingsR = do
    (userId, user) <- requireAuthPair      
    let loggedInUserId = fromSqlKey userId

    message <- runInputPost $ ireq textareaField "txtarea"

    let memberKey = getMemberKey loggedInUserId

    existingMessage <- getUniqueProfileMessage memberKey

    updatedMessage <- case existingMessage of
         Just (Entity _ _) -> updateMessage memberKey message
         Nothing -> return messageNotUpdated

    _ <- insertMessage updatedMessage memberKey message

    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/settings")      

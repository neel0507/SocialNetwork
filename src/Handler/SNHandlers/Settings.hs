{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Settings where

import Import
import Database.Persist.Sql
import Text.Julius  (juliusFile)

getSettingsR :: Handler Html
getSettingsR = do
    (userId, user) <- requireAuthPair --Get user details after authentication
    request <- getRequest
    let mtok = reqToken request
    uniqueMember <- getUniqueMember userId
    memberName <- getMemberName uniqueMember "Does not exist"
    message <- getProfileMessage memberName --Get profile message
    defaultLayout $ do
       $(widgetFile "SNTemplates/settings") --template to display settings page
       toWidget $(juliusFile "templates/SNTemplates/messages.julius") --Associated javascript file

    
postSettingsR :: Handler Html
postSettingsR = do
    (userId, user) <- requireAuthPair --Get user details from authentication      
    let loggedInUserId = fromSqlKey userId --Get logged in user id
    let memberKey = getMemberKey loggedInUserId --Get entity member key

    message <- runInputPost $ ireq textareaField "txtarea" --Get user message

    existingMessage <- getUniqueProfileMessage memberKey --Get profile message entity

    _ <- insertMessage existingMessage memberKey message --Insert the message in database

    redirect SettingsR --redirect to the settings page

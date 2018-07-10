{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Settings where

import Import
import Database.Persist.Sql


getSettingsR :: Handler Html
getSettingsR = do
    uid <- lookupSession "_ID"
    let loggedInUserId = sessUserId uid

    if loggedInUserId > 0
       then do
           let memberKey = memberId (sessUserId uid)

           existingMessage <- runDB $ getBy $ UniqueProfileMessage memberKey

           message <- case existingMessage of
               Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
               Nothing -> return $ pack "No message yet"

           defaultLayout $ do
              addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
              $(widgetFile "SNTemplates/settings")
              
       else
           redirect LoginpageR

postSettingsR :: Handler Html
postSettingsR = do
    uid <- lookupSession "_ID"
    let loggedInUserId = sessUserId uid

    if loggedInUserId > 0
       then do
           message <- runInputPost $ ireq textareaField "txtarea"

           let memberKey = memberId (sessUserId uid)

           existingMessage <- runDB $ getBy $ UniqueProfileMessage memberKey

           updatedMessage <- case existingMessage of
                 Just (Entity memberId member) -> updateMessage memberKey message
                 Nothing -> return messageNotUpdated

           insertedMessage <- insertMessage updatedMessage memberKey message

           defaultLayout $ do
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
             $(widgetFile "SNTemplates/settings")      
             
       else
           redirect LoginpageR
   
sessUserId :: Maybe Text -> Int64
sessUserId uid = case uid of
          Just uid -> read (unpack uid) :: Int64
          Nothing  -> 0 :: Int64

memberId :: Int64 -> Key Member
memberId userId = toSqlKey $ userId


updateMessage :: Key Member -> Textarea -> Handler Int64
updateMessage mKey tarea = do
              liftHandler $ runDB $ updateWhere [ProfileMessageMemberId ==. mKey] [ProfileMessageMessage =. tarea]
              return (fromSqlKey $ mKey)

insertMessage :: Int64 -> Key Member -> Textarea -> Handler Int64
insertMessage umessage mKey tarea =
              if (umessage > 0)
                  then    do
                       return $ fromSqlKey mKey
                  else    do
                       insertedMessage <- runDB $ insert $ ProfileMessage mKey tarea
                       return $ fromSqlKey insertedMessage

messageNotUpdated :: Int64
messageNotUpdated = 0


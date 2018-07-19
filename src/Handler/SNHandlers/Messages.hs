{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Messages where

import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Text.Julius             (juliusFile)


getMessagesR :: Handler Html
getMessagesR = do
    uid <- lookupSession "User_Id"
    sessUserId <- getMemberId uid

    if sessUserId > 0
       then do
           uname <- lookupGetParam "view"                    

           page <- case uname of
               Nothing -> do
                  let loggedInMemberKey = getMemberKey sessUserId
                  eraseMessage <- lookupGetParam "erase" 
                  eraseMessageId <- getMemberId eraseMessage
                  let memberMessageKey = getMemberMessageKey eraseMessageId
                  existingMessage <- getUniqueProfileMessage loggedInMemberKey
                  profileMessage <- getProfileMessage existingMessage "No Message Yet"
                  messageRemoved <- removeMessageFromDB eraseMessageId memberMessageKey
                  messageCount <- getMessageCount loggedInMemberKey                                
                  
                  if (messageCount > noMessage)
                    then do
                        messages <- getMemberMessages loggedInMemberKey 
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/memberMessages")
                          toWidget $(juliusFile "templates/SNTemplates/messages.julius")          
                    else
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/messages")

               Just un -> do
                  viewMemberId <- getMemberId uname  
                  let viewMemberKey = getMemberKey viewMemberId
                  viewMemberEntity <- getUniqueMember $ getUserKey viewMemberId
                  viewMemberName <- getMemberName viewMemberEntity "Does not exist"
                  viewProfileMessageEntity <- getUniqueProfileMessage $ getMemberKey viewMemberId
                  viewMemberMessage <- getProfileMessage viewProfileMessageEntity "No Message Yet"
                  messageCount <- getMessageCount viewMemberKey                  
                 
                  if (messageCount > noMessage)
                    then do
                        messages <- getMemberMessages viewMemberKey
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/viewMemberMessages")
                          toWidget $(juliusFile "templates/SNTemplates/messages.julius")                         
                    else
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/viewMessages")
                          toWidget $(juliusFile "templates/SNTemplates/messages.julius")

           return page
          
       else
           redirect LoginpageR
       

postMessagesR :: Handler Html
postMessagesR = do
    uid <- lookupSession "User_Id"
    sessUserId <- getMemberId uid

    if sessUserId > 0
       then do
           uname <- lookupGetParam "view"
           
           message <- runInputPost $ ireq textareaField "txtarea"
           messageType <- runInputPost $ ireq boolField "messagetype"
           time <- liftIO getLocalTime           
           let loggedInMemberKey = getMemberKey sessUserId
           let loggedInUserKey = getUserKey sessUserId
           fromMemberEntity <- getUniqueMember loggedInUserKey
           fromMemberName <- getMemberName fromMemberEntity "Does not exist"

           page <- case uname of
               Nothing -> do
                  existingMessage <- getUniqueProfileMessage loggedInMemberKey
                  profileMessage <- getProfileMessage existingMessage "No message yet"
                  insertMessage <- runDB $ insert $ MemberMessage loggedInMemberKey loggedInMemberKey fromMemberName messageType time message
                  messages <- getMemberMessages loggedInMemberKey

                  defaultLayout $ do              
                    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                    $(widgetFile "SNTemplates/memberMessages")
                    toWidget $(juliusFile "templates/SNTemplates/messages.julius")                                             

               Just un -> do                                    
                  viewMemberId <- getMemberId uname                  
                  let viewMemberKey = getMemberKey viewMemberId                 
                  viewMemberEntity <- getUniqueMember $ getUserKey viewMemberId
                  viewMemberName <- getMemberName viewMemberEntity "Does not exist"
                  viewProfileMessageEntity <- getUniqueProfileMessage $ getMemberKey viewMemberId
                  viewMemberMessage <- getProfileMessage viewProfileMessageEntity "No Message Yet"
                  insertMessage <- runDB $ insert $ MemberMessage viewMemberKey loggedInMemberKey fromMemberName messageType time message
                  messages <- getMemberMessages viewMemberKey
 
                  defaultLayout $ do              
                    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                    $(widgetFile "SNTemplates/viewMemberMessages")
                    toWidget $(juliusFile "templates/SNTemplates/messages.julius")                    
                         
           return page
              
       else
           redirect LoginpageR


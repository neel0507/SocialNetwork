{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Messages where

import Import
import Database.Persist.Sql
import Text.Julius  (juliusFile)

getMessagesR :: Handler Html
getMessagesR = do
    (userId, user) <- requireAuthPair--Get user details from authentication      
    let loggedInUserId = fromSqlKey userId--Convert the key (userId) into an integer to identify the user                   

    let loggedInMemberKey = getMemberKey loggedInUserId --Get the entity key of the logged in member
    eraseMessage <- lookupGetParam "erase" --Is user about to erase their message?
    _ <- case eraseMessage of -- Will be executed when the logged in member decides to erase their message/s
           Just eraseM -> do --Message to remove
                 eraseMessageId <- getMemberId eraseMessage --Get the Id of specific message to remove
                 let memberMessageKey = getMemberMessageKey eraseMessageId -- Get the key of specific message to remove
                 messageRemoved <- removeMessageFromDB eraseMessageId memberMessageKey --Delete the message with the help of key and Id
                 return "Message removed"--Returned, but not used
           Nothing -> 
                 return ""--Returned, but not used
    existingMessage <- getUniqueProfileMessage loggedInMemberKey --Get profile message entity of the logged in member
    profileMessage <- getProfileMessage existingMessage "No message Yet" --Get profile message
                                                            
    messages <- getMemberMessages loggedInMemberKey --Get messages of the logged in member

    defaultLayout $ do              
      addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js" --Jquery script            
      $(widgetFile "SNTemplates/memberMessages") --template to display when there are messages on the logged in member's messages page
      toWidget $(juliusFile "templates/SNTemplates/messages.julius")--Associated Javascript file 

          
postMessagesR :: Handler Html
postMessagesR = do
    (userId, user) <- requireAuthPair --Get user details from authentication     
    let loggedInUserId = fromSqlKey userId--Convert the key (userId) into an integer to identify the user
    (msg, msgType, time, liMemberKey, fromMember) <- getMessageDetails loggedInUserId --Get message details      
    insertMessage <- runDB $ insert $ MemberMessage liMemberKey liMemberKey fromMember msgType time msg -- insert the message in the database
    redirect MessagesR --redirect to where message is posted                                           


getViewMemberMessagesR :: Int -> Handler Html
getViewMemberMessagesR viewMemberId = do
    let vmId = (fromIntegral viewMemberId) :: Int64 --Convert Int Id to Int64 Id
    let viewMemberKey = getMemberKey vmId --Get view member key
    (viewMemberEntity, viewProfileMessageEntity) <- getUniqueMemberAndProfileMessage (getUserKey vmId) (getMemberKey vmId) -- get member entity and profile message entity to display member name and member message with the help of view member id 
    viewMemberName <- getMemberName viewMemberEntity "Does not exist" --Get the name of view member from entity             
    viewMemberMessage <- getProfileMessage viewProfileMessageEntity "No message Yet" --Get the member message with the help of profile message entity
    messages <- getMemberMessages viewMemberKey --Get messages of the member being viewed                 
                 
    defaultLayout $ do              
      addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js" --Jquery script            
      $(widgetFile "SNTemplates/viewMemberMessages") --template to display the page with messages of the member being viewed
      toWidget $(juliusFile "templates/SNTemplates/messages.julius") --Associated Javascript file


postViewMemberMessagesR :: Int -> Handler Html
postViewMemberMessagesR viewMemberId = do
    (userId, user) <- requireAuthPair --Get user details from authentication
    let vmId = (fromIntegral viewMemberId) :: Int64 --Convert Int Id to Int64 Id
    let viewMemberKey = getMemberKey vmId --Get view member key      
    let loggedInUserId = fromSqlKey userId--Convert the key (userId) into an integer to identify the user
    (msg, msgType, time, liMemberKey, fromMember) <- getMessageDetails loggedInUserId --Get message details
    insertMessage <- runDB $ insert $ MemberMessage viewMemberKey liMemberKey fromMember msgType time msg --insert the message in the database
    redirect $ ViewMemberMessagesR viewMemberId --redirect to where message is posted


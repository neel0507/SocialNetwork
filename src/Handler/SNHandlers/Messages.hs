{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Messages where

import Import
import Database.Persist.Sql as PersQ
import Database.Esqueleto as E
import Text.Julius  (juliusFile)

getMessagesR :: Handler Html
getMessagesR = do
    (_, user) <- requireAuthPair--Get user details from authentication                   
    eraseMessage <- lookupGetParam "erase" --Is user about to erase their message?
    _ <- case eraseMessage of -- Will be executed when the logged in member decides to erase their message/s
           Just _ -> do --Message to remove
                 eraseMessageId <- getMemberId eraseMessage --Get the Id of specific message to remove
                 let memberMessageKey = getMemberMessageKey eraseMessageId -- Get the key of specific message to remove
                 _ <- removeMessageFromDB eraseMessageId memberMessageKey --Delete the message with the help of key and Id
                 return "Message removed"--Returned, but not used
           Nothing -> 
                 return ""--Returned, but not used
    profileMessage <- getProfileMessage (userIdent user) --Get profile message
    memberKey <- getViewMemberKey (userIdent user)                                                            
    messages <- getMemberMessages memberKey--Get messages of the logged in member

    defaultLayout $ do            
      $(widgetFile "SNTemplates/memberMessages") --template to display when there are messages on the logged in member's messages page
      toWidget $(juliusFile "templates/SNTemplates/messages.julius")--Associated Javascript file 

          
postMessagesR :: Handler Html
postMessagesR = do
    (userId, user) <- requireAuthPair --Get user details from authentication     
    (msg, msgType, time, liMemberKey) <- getMessageDetails (fromSqlKey userId) --Get message details      
    _ <- runDB $ insert $ MemberMessage liMemberKey liMemberKey msgType time msg -- insert the message in the database
    redirect MessagesR --redirect to where message is posted                                           


getViewMemberMessagesR :: Text -> Handler Html
getViewMemberMessagesR viewMemberName = do
    validMember <- runDB $ PersQ.count [MemberIdent PersQ.==. viewMemberName] --Identify if it is a valid user
    if validMember > 0
       then do            
           viewMemberMessage <- getProfileMessage viewMemberName --Get the member message with the help of memberName
           viewMemberKey <- getViewMemberKey viewMemberName
           messages <- getMemberMessages viewMemberKey --Get messages of the member being viewed                 
                 
           defaultLayout $ do           
              $(widgetFile "SNTemplates/viewMemberMessages") --template to display the page with messages of the member being viewed
              toWidget $(juliusFile "templates/SNTemplates/messages.julius") --Associated Javascript file
       else
           defaultLayout $ do
              [whamlet|
                 Member does not exist
              |]


postViewMemberMessagesR :: Text -> Handler Html
postViewMemberMessagesR viewMemberName = do
    (userId, user) <- requireAuthPair --Get user details from authentication
    viewMemberKey <- getViewMemberKey viewMemberName      
    (msg, msgType, time, liMemberKey) <- getMessageDetails (fromSqlKey userId) --Get message details
    _ <- runDB $ insert $ MemberMessage viewMemberKey liMemberKey msgType time msg --insert the message in the database
    redirect $ ViewMemberMessagesR viewMemberName --redirect to where message is posted


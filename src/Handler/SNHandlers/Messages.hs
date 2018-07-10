{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Messages where

import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Text.Julius             (juliusFile)
import Data.Time

getMessagesR :: Handler Html
getMessagesR = do
    uid <- lookupSession "_ID"
    let loggedInUserId = sessUserId uid

    if loggedInUserId > 0
       then do

           uname <- lookupGetParam "view"
           eraseMessage <- lookupGetParam "erase"
           let loggedInMemberKey = memberKey (sessUserId uid)      
           let viewMemberKey = memberKey (sessUserId uname)

           page <- case uname of
               Nothing -> do
                  
                  let memberMessageId = case eraseMessage of
                        Just removeM ->  read (unpack removeM) :: Int64
                        Nothing -> 0 :: Int64
                  let memberMessageKey = removeMessageKey memberMessageId

                  existingMessage <- runDB $ getBy $ UniqueProfileMessage loggedInMemberKey

                  profileMessage <- case existingMessage of
                      Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
                      Nothing -> return $ pack "No message yet"

                  messageRemoved <- removeMessageFromDB memberMessageId memberMessageKey                  

                  getMessageCount <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val loggedInMemberKey
                      let cnt = E.countRows :: E.SqlExpr (E.Value Int)                 
                      return
                          cnt

                  getAllMessages <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val loggedInMemberKey
                      return
                       ( memberMessage ^. MemberMessageId
                       , memberMessage ^. MemberMessageFromMemberId
                       , memberMessage ^. MemberMessageFromMember
                       , memberMessage ^. MemberMessageIsPrivateMessage
                       , memberMessage ^. MemberMessageTime
                       , memberMessage ^. MemberMessageMessage
                       )
                  
                  let mCount = [E.Value (0::Int)]
                  if (getMessageCount > mCount)
                    then
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/messages")
                          toWidget
                             [julius|
                                 document.getElementById("noMessage").style.display = "none";
                             |]
                          toWidget
                             [hamlet|
                                 $forall (E.Value memberMessageId, E.Value fromMemberId, E.Value fromMember, E.Value messageType, E.Value time, E.Value message) <- getAllMessages
                                    <ul>
                                      $if(messageType == True)
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> whispered--&quot;#{message}&quot;--<a href="/messages?erase=#{fromSqlKey $ memberMessageId}">erase</a>
                                      $else
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> wrote--&quot;#{message}&quot;--<a href="/messages?erase=#{fromSqlKey $ memberMessageId}">erase</a>                         
                             |] 
                    else
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/messages")

               Just un -> do
                  viewMemberEntity <- case uname of
                      Just uname -> runDB $ getBy $ UniqueMember (userKey ((read $ unpack uname)::Int64))  
                      Nothing -> return Nothing

                  viewMemberId <- case viewMemberEntity of
                      Just (Entity mId member) -> return (memberId mId)
                      Nothing -> return nonExistingMember

                  viewMemberName <- case viewMemberEntity of
                      Just (Entity memberId member) -> return (memberIdent member)
                      Nothing -> return $ pack "Does not exist"

                  viewProfileMessageEntity <- case uname of
                      Just uname -> runDB $ getBy $ UniqueProfileMessage (memberKey ((read $ unpack uname)::Int64))
                      Nothing -> return Nothing

                  viewMemberMessage <- case viewProfileMessageEntity of
                      Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
                      Nothing -> return $ pack "No message yet"

                  getMessageCount <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val viewMemberKey
                      let cnt = E.countRows :: E.SqlExpr (E.Value Int)                 
                      return
                          cnt

                  getAllMessages <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val viewMemberKey
                      return
                       ( memberMessage ^. MemberMessageFromMemberId
                       , memberMessage ^. MemberMessageFromMember
                       , memberMessage ^. MemberMessageIsPrivateMessage
                       , memberMessage ^. MemberMessageTime
                       , memberMessage ^. MemberMessageMessage
                       )

                  let mCount = [E.Value (0::Int)]
                  if (getMessageCount > mCount)
                    then
                        defaultLayout $ do              
                          addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                          $(widgetFile "SNTemplates/viewMessages")
                          toWidget $(juliusFile "templates/SNTemplates/messages.julius")
                          toWidget
                             [julius|
                                 document.getElementById("noMessage").style.display = "none";
                             |]
                          toWidget
                             [hamlet|
                                 $forall (E.Value fromMemberId, E.Value fromMember, E.Value messageType, E.Value time, E.Value message) <- getAllMessages
                                    <ul>
                                        $if(messageType == True)
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> whispered--&quot;#{message}&quot;
                                      $else
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> wrote--&quot;#{message}&quot;                                 
                             |] 
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
    uid <- lookupSession "_ID"
    let loggedInUserId = sessUserId uid

    if loggedInUserId > 0
       then do
           uname <- lookupGetParam "view"
           message <- runInputPost $ ireq textareaField "txtarea"
           messageType <- runInputPost $ ireq boolField "messagetype"
           time <- liftIO getCurrentTime
           let viewMemberKey = memberKey (sessUserId uname)
           let loggedInMemberKey = memberKey (sessUserId uid)
           let loggedInUserKey = userKey (sessUserId uid)
           fromMemberEntity <- runDB $ getBy $ UniqueMember loggedInUserKey
           fromMemberName <- case fromMemberEntity of
                Just (Entity memberId member) -> return (memberIdent member)
                Nothing -> return $ pack "Does not exist"

           page <- case uname of
               Nothing -> do               
                  
                  existingMessage <- runDB $ getBy $ UniqueProfileMessage loggedInMemberKey

                  profileMessage <- case existingMessage of
                      Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
                      Nothing -> return $ pack "No message yet"  

                  insertMessage <- runDB $ insert $ MemberMessage loggedInMemberKey loggedInMemberKey fromMemberName messageType (addMinutes 60 time) message
                
                  getAllMessages <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val loggedInMemberKey
                      return
                       ( memberMessage ^. MemberMessageId
                       , memberMessage ^. MemberMessageFromMemberId
                       , memberMessage ^. MemberMessageFromMember
                       , memberMessage ^. MemberMessageIsPrivateMessage
                       , memberMessage ^. MemberMessageTime
                       , memberMessage ^. MemberMessageMessage
                       )

                  defaultLayout $ do              
                    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                    $(widgetFile "SNTemplates/messages")
                    toWidget
                         [julius|
                             document.getElementById("noMessage").style.display = "none";
                         |]
                    toWidget
                         [hamlet|
                             $forall (E.Value memberMessageId, E.Value fromMemberId, E.Value fromMember, E.Value messageType, E.Value time, E.Value message) <- getAllMessages
                                    <ul>
                                        $if(messageType == True)
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> whispered--&quot;#{message}&quot;--<a href="/messages?erase=#{fromSqlKey $ memberMessageId}">erase</a>
                                      $else
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> wrote--&quot;#{message}&quot;--<a href="/messages?erase=#{fromSqlKey $ memberMessageId}">erase</a>                                            
                         |]                         

               Just un -> do
                  viewMemberEntity <- case uname of
                      Just uname -> runDB $ getBy $ UniqueMember (userKey ((read $ unpack uname)::Int64))  
                      Nothing -> return Nothing

                  viewMemberId <- case viewMemberEntity of
                      Just (Entity mId member) -> return (memberId mId)
                      Nothing -> return nonExistingMember

                  viewMemberName <- case viewMemberEntity of
                      Just (Entity memberId member) -> return (memberIdent member)
                      Nothing -> return $ pack "Does not exist"

                  viewProfileMessageEntity <- case uname of
                      Just uname -> runDB $ getBy $ UniqueProfileMessage (memberKey ((read $ unpack uname)::Int64))
                      Nothing -> return Nothing

                  viewMemberMessage <- case viewProfileMessageEntity of
                      Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
                      Nothing -> return $ pack "No message yet"

                  insertMessage <- runDB $ insert $ MemberMessage viewMemberKey loggedInMemberKey fromMemberName messageType (addMinutes 60 time) message 
                 
                  getAllMessages <- runDB
                    $ E.select
                    $ E.from $ \memberMessage -> do                  
                      E.where_ $ memberMessage ^. MemberMessageMemberId E.==. E.val viewMemberKey
                      return
                       ( memberMessage ^. MemberMessageFromMemberId
                       , memberMessage ^. MemberMessageFromMember
                       , memberMessage ^. MemberMessageIsPrivateMessage
                       , memberMessage ^. MemberMessageTime
                       , memberMessage ^. MemberMessageMessage
                       )
 
                  defaultLayout $ do              
                    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"             
                    $(widgetFile "SNTemplates/viewMessages")
                    toWidget $(juliusFile "templates/SNTemplates/messages.julius")
                    toWidget
                         [julius|
                             document.getElementById("noMessage").style.display = "none";
                         |]
                    toWidget
                         [hamlet|
                             $forall (E.Value fromMemberId, E.Value fromMember, E.Value messageType, E.Value time, E.Value message) <- getAllMessages
                                    <ul>
                                        $if(messageType == True)
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> whispered--&quot;#{message}&quot;
                                      $else
                                         <li class="listItem">#{dateFormat $ time}--<a href="/messages?view=#{fromSqlKey $ fromMemberId}">#{fromMember}</a> wrote--&quot;#{message}&quot;                                  
                         |]
                         
           return page
              
       else
           redirect LoginpageR

sessUserId :: Maybe Text -> Int64
sessUserId uid = case uid of
          Just uid -> read (unpack uid) :: Int64
          Nothing  -> 0 :: Int64

memberKey :: Int64 -> Key Member
memberKey userId = toSqlKey $ userId

userKey :: Int64 -> Key User
userKey userId = toSqlKey $ userId

memberMessageKey :: Int64 -> Key MemberMessage
memberMessageKey mmKey = toSqlKey $ mmKey

memberId :: Key Member -> Int64
memberId mKey = fromSqlKey $ mKey

nonExistingMember :: Int64
nonExistingMember = 0

dateFormat :: UTCTime -> String
dateFormat = formatTime defaultTimeLocale "%d/%m/%Y %I:%M:%S %p"

addMinutes :: NominalDiffTime -> UTCTime -> UTCTime
addMinutes minutes = addUTCTime (minutes * 60)

removeMessageFromDB :: Int64 -> Key MemberMessage -> Handler Int64
removeMessageFromDB rmId mmKey = 
           if rmId > 0
              then do
                 liftHandler $ runDB $ deleteWhere [MemberMessageId ==. mmKey]
                 return $ fromSqlKey mmKey
              else do
                 return $ fromSqlKey mmKey

removeMessageKey :: Int64 -> Key MemberMessage
removeMessageKey rmId = toSqlKey $ rmId


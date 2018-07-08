{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Prelude
import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getMembersR :: Handler Html
getMembersR = do  
    uid <- lookupSession "_ID"  
    uname <- lookupGetParam "view"  
    addMember <- lookupGetParam "add"
    removeMember <- lookupGetParam "remove"
  
    let sessUserId = case uid of
          Just uid -> read (unpack uid) :: Int64
          Nothing  -> 0 :: Int64
    
    let memberKey = memberId sessUserId
    let userKey = userId sessUserId

    let addMemberId = case addMember of
            Just addM -> read (unpack addM) :: Int64
            Nothing   -> 0 :: Int64
    let removeMemberId = case removeMember of
            Just removeM ->  read (unpack removeM) :: Int64
            Nothing -> 0 :: Int64
    let addMemberKey = memberId addMemberId
    let removeMemberKey = memberId removeMemberId

    viewMemberEntity <- case uname of
           Just uname -> runDB $ getBy $ UniqueMember (userId ((read $ unpack uname)::Int64))
           Nothing -> return Nothing
    
    viewMemberName <- case viewMemberEntity of
           Just (Entity memberId member) -> return $ memberIdent member
           Nothing -> return $ pack "Does not exist"

    viewProfileMessageEntity <- case uname of
           Just uname -> runDB $ getBy $ UniqueProfileMessage (memberId ((read $ unpack uname)::Int64))
           Nothing -> return Nothing

    viewMemberMessage <- case viewProfileMessageEntity of
           Just (Entity _ pm) -> return $ unTextarea (profileMessageMessage pm)
           Nothing -> return $ pack "No message yet"

    if sessUserId > 0
         then do
             memberFollowed <- addMemberToDB addMemberId memberKey addMemberKey
             memberRemoved <- removeMemberFromDB removeMemberId memberKey removeMemberKey
         
             otherMembers <- runDB
              $ E.select
              $ E.from $ \member -> do                  
                  E.where_ $ member ^. MemberUserId E.!=. E.val userKey
                  return
                      ( member ^. MemberUserId
                      , member ^. MemberIdent
                      )

             followingMembersCount <- runDB
              $ E.select
              $ E.from $ \(followingMembers `E.InnerJoin` member) -> do                  
                  E.on $ followingMembers ^. FollowingMembersFollowingMemberId E.==. member ^. MemberId
                  E.where_ $ followingMembers ^. FollowingMembersMemberId E.==. E.val memberKey
                  let cnt = E.countRows :: E.SqlExpr (E.Value Int)                 
                  return
                      cnt

             
             followingMembers <- runDB
              $ E.select
              $ E.from $ \(followingMembers `E.InnerJoin` member) -> do                  
                  E.on $ followingMembers ^. FollowingMembersFollowingMemberId E.==. member ^. MemberId
                  E.where_ $ followingMembers ^. FollowingMembersMemberId E.==. E.val memberKey                                   
                  return
                      ( member ^. MemberUserId
                      , member ^. MemberIdent
                      )

             unFollowingMembers <- runDB
              $ E.select
              $ E.from $ \member -> do                                                    
                  E.where_ ((member ^. MemberUserId E.!=. E.val userKey) E.&&.
                            (member ^. MemberId `E.notIn` (E.subList_select 
                                                         $ E.from $ \followingMembersDB -> do
                                                             E.where_ (followingMembersDB ^. FollowingMembersMemberId E.==. E.val memberKey) 
                                                             return (followingMembersDB ^. FollowingMembersFollowingMemberId))))                            
                  return
                      ( member ^. MemberUserId
                      , member ^. MemberIdent
                      )
     
             let mCount = [E.Value (0::Int)]
             defaultLayout $ do
                addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                $(widgetFile "SNTemplates/members")                
                toWidget $ case uname of
                         Nothing ->                                                         
                                   [hamlet|
                                     <div class="message"><h2>Other Members</h2>
                                     <ul class="memberlist"> 
                                         $if (followingMembersCount > mCount)
                                           $forall (E.Value userId, E.Value userIdent) <- followingMembers
                                                     <li><a href="@{MembersR}?view=#{fromSqlKey $ userId}">#{userIdent}</a> <a href="@{MembersR}?remove=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}">drop</a> <a href="@{MembersR}?add=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}" style="display:none">follow</a>
                                           $forall (E.Value userId, E.Value userIdent) <- unFollowingMembers         
                                                           <li><a href="@{MembersR}?view=#{fromSqlKey $ userId}">#{userIdent}</a> <a href="@{MembersR}?add=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}">follow</a> <a href="@{MembersR}?remove=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}" style="display:none">drop</a>
                                         $else
                                           $forall (E.Value userId, E.Value userIdent) <- otherMembers

                                                           <li><a href="@{MembersR}?view=#{fromSqlKey $ userId}">#{userIdent}</a> <a href="@{MembersR}?add=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}">follow</a><a href="@{MembersR}?remove=#{fromSqlKey $ userId}" class="membersListItem" id="member#{fromSqlKey $ userId}" style="display:none">drop</a>
                                   |]
                                  
                         Just uname ->                
                                   [hamlet|
                                      <div class="message"><p>#{viewMemberName}&#39;s Profile</p>
                                      <div class="message"><p>Profile Message: #{viewMemberMessage}</p>
                                      <ul class="menu">
                                             <li><a href="@{MessagesR}?view=#{uname}">View #{viewMemberName}&#39;s Messages </a>
                                   |] 
    else
         redirect LoginpageR


userId :: Int64 -> Key User
userId userId = toSqlKey $ userId

memberId :: Int64 -> Key Member
memberId userId = toSqlKey $ userId

addMemberToDB :: Int64 -> Key Member -> Key Member -> Handler Int64
addMemberToDB amId mKey addmKey = 
           if amId > 0
              then do
                 a <- liftHandler $ runDB $ insert $ FollowingMembers mKey addmKey
                 return $ fromSqlKey a
              else do
                 return $ fromSqlKey mKey

removeMemberFromDB :: Int64 -> Key Member -> Key Member -> Handler Int64
removeMemberFromDB rmId mKey removeMKey = 
           if rmId > 0
              then do
                 liftHandler $ runDB $ deleteWhere [FollowingMembersMemberId ==. mKey, FollowingMembersFollowingMemberId ==. removeMKey]
                 return $ fromSqlKey mKey
              else do
                 return $ fromSqlKey mKey


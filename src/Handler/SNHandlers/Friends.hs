{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}


module Handler.SNHandlers.Friends where

import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


getFriendsR :: Handler Html
getFriendsR = do
    uid <- lookupSession "_ID"

    let sessUserId = case uid of
          Just uid -> read (unpack uid) :: Int64
          Nothing  -> 0 :: Int64
    let memberKey = memberId sessUserId

    if sessUserId > 0
         then do
             followingMembers <- runDB
              $ E.select
              $ E.from $ \(followingMembers `E.InnerJoin` member) -> do                  
                  E.on $ followingMembers ^. FollowingMembersFollowingMemberId E.==. member ^. MemberId
                  E.where_ $ followingMembers ^. FollowingMembersMemberId E.==. E.val memberKey             
                  return
                      (member ^. MemberIdent)

             defaultLayout $ do
                 $(widgetFile "SNTemplates/friends")
                 toWidget
                   [hamlet|
                     <div class="message"><h2>You are Following</h2>
                     <ul class="memberlist">
                           $forall (E.Value followedMember) <- followingMembers
                                   <li> #{followedMember}
                     <ul class="menu">
                                   <li><a href="@{MessagesR}?view=">View Your Messages </a>
                   |]
    else
         redirect LoginpageR

memberId :: Int64 -> Key Member
memberId userId = toSqlKey $ userId

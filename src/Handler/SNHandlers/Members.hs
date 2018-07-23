{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import
import Database.Persist.Sql

getMembersR :: Handler Html
getMembersR = do
    (userId, user) <- requireAuthPair      
    let loggedInUserId = fromSqlKey userId
    uname <- lookupGetParam "view"             
    viewMemberId <- getMemberId uname

    page <- case uname of
       Just uname -> do                    
              viewMemberEntity <- getUniqueMember $ getUserKey viewMemberId      
              viewMemberName <- getMemberName viewMemberEntity "Does not exist"
              viewProfileMessageEntity <- getUniqueProfileMessage $ getMemberKey viewMemberId
              viewMemberMessage <- getProfileMessage viewProfileMessageEntity "No Message Yet"

              defaultLayout $ do                
                $(widgetFile "SNTemplates/viewMember")
                      
       Nothing -> do             
              addMember <- lookupGetParam "add"
              removeMember <- lookupGetParam "remove"
              addMemberId <- getMemberId addMember
              removeMemberId <- getMemberId removeMember
              let memberKey = getMemberKey loggedInUserId
              let userKey = getUserKey loggedInUserId             
              let addMemberKey = getMemberKey addMemberId                
              let removeMemberKey = getMemberKey removeMemberId                    
              _ <- addMemberToDB addMemberId memberKey addMemberKey
              _ <- removeMemberFromDB removeMemberId memberKey removeMemberKey
              followingMembers <- getFollowingMembers memberKey                    
              members <- getMembers userKey       
              unFollowingMembers <- getUnFollowingMembers memberKey userKey                    
          
              defaultLayout $ do                
                 $(widgetFile "SNTemplates/members")                                                        
                                      
    return page

getFriendsR :: Handler Html
getFriendsR = do
    (userId, user) <- requireAuthPair
    let loggedInUserId = fromSqlKey userId    
    let memberKey = getMemberKey loggedInUserId                    
    followingMembers <- getFollowingMembers memberKey             

    defaultLayout $ do
       $(widgetFile "SNTemplates/friends")

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import
import Database.Persist.Sql

getMembersR :: Handler Html
getMembersR = do  
    uid <- lookupSession "User_Id"     
    sessUserId <- getMemberId uid

    if sessUserId > 0
         then do
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
                    let memberKey = getMemberKey sessUserId
                    let userKey = getUserKey sessUserId             
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
    else
         redirect LoginpageR

getFriendsR :: Handler Html
getFriendsR = do
    uid <- lookupSession "User_Id"
    sessUserId <- getMemberId uid    

    if sessUserId > 0
         then do
             let memberKey = getMemberKey sessUserId                    
             followingMembers <- getFollowingMembers memberKey             

             defaultLayout $ do
                 $(widgetFile "SNTemplates/friends")
    else
         redirect LoginpageR


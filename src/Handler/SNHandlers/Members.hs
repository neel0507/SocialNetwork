{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import
import Database.Persist.Sql
import Database.Esqueleto as E

getMembersR :: Handler Html
getMembersR = do
    (userId, user) <- requireAuthPair --Get user details from authentication     
    let loggedInUserId = fromSqlKey userId --Convert the entity key (userId) into an integer to identify the user                
    let memberKey = getMemberKey loggedInUserId --Get entity member key of the logged in user            
    addMember <- lookupGetParam "add" --If logged in user clicks on add button on the page
    removeMember <- lookupGetParam "remove" --If logged in user clicks on remove button on the page
    _ <- case addMember of -- Will be executed when the logged in member decides to follow another member on the page
            Just addM -> do --Member to be added
                                    addMemberId <- getMemberId addMember --Get memberId of the member to be added
                                    let addMemberKey = getMemberKey addMemberId --Get member key of the member to be added
                                    _ <- addMemberToDB addMemberId memberKey addMemberKey --Add the member to database
                                    return "Member added" --Returned, but not used
            Nothing -> return "" --if for some reason, member cannot be added
    _ <- case removeMember of --Will be executed when the logged in member decides to remove the member they are following
            Just removeM -> do --Member to be dropped or removed or unfollowed
                          removeMemberId <- getMemberId removeMember --Get memberId of the member to be removed
                          let removeMemberKey = getMemberKey removeMemberId --Get member key of the member to be removed                   
                          _ <- removeMemberFromDB removeMemberId memberKey removeMemberKey-- Remove the member from the database
                          return "Member removed" --Returned, but not used
            Nothing -> return "" --if for some reason, member cannot be removed

    let userKey = getUserKey loggedInUserId --Get entity user key of the logged in user
    followingMembers <- getFollowingMembers memberKey -- Get the following members of the user
    members <- getMembers userKey followingMembers memberKey--Get members of the site if the user is not following any members                   
          
    defaultLayout $ do                
      $(widgetFile "SNTemplates/members") --template to display members of the site                                                       


getViewMemberR :: Int -> Handler Html
getViewMemberR viewMemberId = do
    let vmId = (fromIntegral viewMemberId) :: Int64 --Convert Int Id to Int64 Id
    (viewMemberEntity, viewProfileMessageEntity) <- getUniqueMemberAndProfileMessage (getUserKey vmId) (getMemberKey vmId) -- get member entity and profile message entity to display member name and member message with the help of view member id           
    viewMemberName <- getMemberName viewMemberEntity "Does not exist" -- Get the name of view member from entity              
    viewMemberMessage <- getProfileMessage viewProfileMessageEntity "No message yet" --Get the member message with the help of profile message entity
    defaultLayout $ do                
       $(widgetFile "SNTemplates/viewMember") --template to display a specific member and their details
              

getFriendsR :: Handler Html
getFriendsR = do
    (userId, user) <- requireAuthPair --Get user details from authentication
    let loggedInUserId = fromSqlKey userId --Get logged in user id   
    let memberKey = getMemberKey loggedInUserId --Get logged in entity member key                   
    followingMembers <- getFollowingMembers memberKey --Get following members of the user            

    defaultLayout $ do
       $(widgetFile "SNTemplates/friends") --template to display friends of the logged in user/member

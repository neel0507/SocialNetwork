{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import
import Database.Persist.Sql as PersQ

getMembersR :: Handler Html
getMembersR = do    
    (userId, _) <- requireAuthPair --Get user details from authentication     
    let loggedInUserId = fromSqlKey userId --Convert the entity key (userId) into an integer to identify the user                
    let memberKey = getMemberKey loggedInUserId --Get entity member key of the logged in user            
    addMember <- lookupGetParam "add" --If logged in user clicks on add button on the page
    removeMember <- lookupGetParam "remove" --If logged in user clicks on remove button on the page
    _ <- case addMember of -- Will be executed when the logged in member decides to follow another member on the page
            Just _ -> do --Member to be added
                                    addMemberId <- getMemberId addMember --Get memberId of the member to be added
                                    let addMemberKey = getMemberKey addMemberId --Get member key of the member to be added
                                    _ <- addMemberToDB addMemberId memberKey addMemberKey --Add the member to database
                                    return ()
            Nothing -> return ()
    _ <- case removeMember of --Will be executed when the logged in member decides to remove the member they are following
            Just _ -> do --Member to be dropped or removed or unfollowed
                          removeMemberId <- getMemberId removeMember --Get memberId of the member to be removed
                          let removeMemberKey = getMemberKey removeMemberId --Get member key of the member to be removed                   
                          _ <- removeMemberFromDB removeMemberId memberKey removeMemberKey-- Remove the member from the database
                          return ()
            Nothing -> return ()

    let userKey = getUserKey loggedInUserId --Get entity user key of the logged in user
    mutualMembers <- getMutualMembers memberKey --Get the mutual members of the user
    followingMembers <- getFollowingMembers memberKey-- Get the following members of the user
    followers <- getFollowers memberKey --Get followers of the user                 
    members <- getMembers userKey mutualMembers followingMembers followers memberKey -- Get members who are not mutual members, following members or followers     
    defaultLayout $ do                
      $(widgetFile "SNTemplates/members") --template to display members of the site                                                     


getViewMemberR :: Text -> Handler Html
getViewMemberR viewMemberName = do
    (userId, _) <- requireAuthPair --Get user details from authentication
    validMember <- runDB $ PersQ.count [MemberIdent PersQ.==. viewMemberName] --Identify if it is a valid user
    if validMember > 0
       then do              
          viewMemberMessage <- getProfileMessage viewMemberName --Get the member message with the help of member name
          defaultLayout $ do                
             $(widgetFile "SNTemplates/viewMember") --template to display a specific member and their details
       else
          defaultLayout $ do
             [whamlet|
                Member does not exist
             |]
              

getFriendsR :: Handler Html
getFriendsR = do
    (userId, _) <- requireAuthPair --Get user details from authentication
    let loggedInUserId = fromSqlKey userId --Get logged in user id   
    let memberKey = getMemberKey loggedInUserId --Get logged in entity member key
    mutualMembers <- getMutualMembers memberKey --Get mutual members of the user                  
    followingMembers <- getFollowingMembers memberKey-- Get the following members of the user            
    followers <- getFollowers memberKey --Get followers of the user

    defaultLayout $ do
       $(widgetFile "SNTemplates/friends") --template to display friends of the logged in user/member


{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import
import Database.Persist.Sql as PersQ

getMembersR :: Handler Html
getMembersR = do
 uid <- lookupSession "User_Id"     
 loggedInUserId <- getMemberId uid
 if loggedInUserId > 0
  then do   
   -- (userId, _) <- requireAuthPair --Get user details from authentication     
   -- let loggedInUserId = fromSqlKey userId --Convert the entity key (userId) into an integer to identify the user                
    let memberKey = getMemberKey loggedInUserId --Get entity member key of the logged in user            
    addMember <- lookupGetParam "add" --If logged in user clicks on add button on the page
    removeMember <- lookupGetParam "remove" --If logged in user clicks on remove button on the page
    _ <- case addMember of -- Will be executed when the logged in member decides to follow another member on the page
            Just _ -> do --Member to be added
                                    addMemberId <- getMemberId addMember --Get memberId of the member to be added
                                    let addMemberKey = getMemberKey addMemberId --Get member key of the member to be added
                                    _ <- addMemberToDB addMemberId memberKey addMemberKey --Add the member to database
                                    pure ()
            Nothing -> pure ()
    _ <- case removeMember of --Will be executed when the logged in member decides to remove the member they are following
            Just _ -> do --Member to be dropped or removed or unfollowed
                          removeMemberId <- getMemberId removeMember --Get memberId of the member to be removed
                          let removeMemberKey = getMemberKey removeMemberId --Get member key of the member to be removed                   
                          _ <- removeMemberFromDB removeMemberId memberKey removeMemberKey-- Remove the member from the database
                          pure ()
            Nothing -> pure ()

    let userKey = getUserKey loggedInUserId --Get entity user key of the logged in user
    mutualMembers <- getMutualMembers memberKey
    followingMembers <- getFollowingMembers memberKey-- Get the following members of the user
    followers <- getFollowers memberKey                  
    members <- getMembers userKey mutualMembers followingMembers followers memberKey      
    defaultLayout $ do                
      $(widgetFile "SNTemplates/members") --template to display members of the site
  else
      redirect LoginpageR                                                      


getViewMemberR :: Text -> Handler Html
getViewMemberR viewMemberName = do
 uid <- lookupSession "User_Id"     
 loggedInUserId <- getMemberId uid
 if loggedInUserId > 0
  then do 
   -- (userId, _) <- requireAuthPair --Get user details from authentication
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
  else
      redirect LoginpageR
              

getFriendsR :: Handler Html
getFriendsR = do
 uid <- lookupSession "User_Id"     
 userId <- getMemberId uid
 if userId > 0
  then do
   -- (userId, _) <- requireAuthPair --Get user details from authentication
   -- let loggedInUserId = fromSqlKey userId --Get logged in user id   
    let memberKey = getMemberKey userId --Get logged in entity member key
    mutualMembers <- getMutualMembers memberKey                   
    followingMembers <- getFollowingMembers memberKey-- Get the following members of the user            
    followers <- getFollowers memberKey

    defaultLayout $ do
       $(widgetFile "SNTemplates/friends") --template to display friends of the logged in user/member
  else
     redirect LoginpageR

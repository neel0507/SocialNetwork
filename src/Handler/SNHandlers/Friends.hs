{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}


module Handler.SNHandlers.Friends where

import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))


getFriendsR :: Handler Html
getFriendsR = do
    uid <- lookupSession "_ID"
    sessUserId <- getMemberId uid    

    if sessUserId > 0
         then do
             let memberKey = getMemberKey sessUserId
             let userKey = getUserKey sessUserId
             followingMembersCount <- getFollowingMembersCount memberKey                    
             followingMembers <- getFollowingMembers followingMembersCount noMembers memberKey

             defaultLayout $ do
                 $(widgetFile "SNTemplates/friends")                
    else
         redirect LoginpageR


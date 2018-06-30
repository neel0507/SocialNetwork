{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Prelude
import Import
import Database.Persist.Sql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

getMembersR :: Handler Html
getMembersR = do    
    user <- lookupGetParam "view"
    members <- runDB
             $ E.select
             $ E.from $ \(member `E.InnerJoin` user) -> do
                  E.on $ member ^. MemberUserId E.==. user ^. UserId
                  E.where_ $ member ^. MemberLoggedInUser E.==. E.val False
                  return
                      ( member ^. MemberUserId
                      , user   ^. UserIdent
                      ) 

    defaultLayout $ do
                addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                $(widgetFile "SNTemplates/members")
                toWidget $ case user of
                         Nothing ->                                                         
                                   [hamlet|
                                     <div class="message"><h2>Other Members</h2>
                                     <ul class="memberlist">
                                         $forall (E.Value userId, E.Value userIdent) <- members
                                            <li><a href="@{MembersR}?view=#{userIdent}">#{userIdent}</a> <a href="@{MembersR}?add=#{userIdent}" class="membersListItem" id="member#{fromSqlKey $ userId}">follow</a><a href="@{MembersR}?remove=#{userIdent}" class="membersListItem" id="member#{fromSqlKey $ userId}" name="removeMember#{fromSqlKey $ userId}">drop</a>
                                   |]
                         Just uname ->                
                                   [hamlet|
                                      <div class="message"><p>#{uname}&#39;s Profile</p>
                                   |]


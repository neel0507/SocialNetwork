{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Prelude
import Import
import Database.Persist.Sql

getMembersR :: Handler Html
getMembersR = do    
    user <- lookupGetParam "view"
    allUsers <- runDB $ getAllUsers
    defaultLayout $ do
                addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                $(widgetFile "SNTemplates/members")
                toWidget $ case user of
                         Nothing ->                                                         
                                   [hamlet|
                                     <div class="message"><h2>Other Members</h2>
                                     <ul class="memberlist">
                                         $forall (Entity userId users) <- allUsers
                                            <li><a href="@{MembersR}?view=#{userIdent users}">#{userIdent users}</a> <a href="@{MembersR}?add=#{userIdent users}" class="membersListItem" id="member#{fromSqlKey $ userId}">follow</a><a href="@{MembersR}?remove=#{userIdent users}" class="membersListItem" id="member#{fromSqlKey $ userId}" name="removeMember#{fromSqlKey $ userId}">drop</a>
                                   |]
                         Just uname ->                
                                   [hamlet|
                                      <div class="message"><p>#{uname}&#39;s Profile</p>
                                   |]

getAllUsers :: DB [Entity User]
getAllUsers = selectList [] [Asc UserId]

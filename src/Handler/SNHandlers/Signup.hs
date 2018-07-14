{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where

import Import

getSignupR :: Handler Html
getSignupR = do    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/signup")

postSignupR :: Handler Html
postSignupR = do
    ident <- getPostParameters "ident"
    password <- getPostParameters "password"
    
    existingUser <- getUniqueUser ident
    existingUserId <- createUserRecordAndReturnUserKey existingUser ident password  

    existingMember <- getUniqueMember existingUserId
    _ <- createMemberRecordAndReturnMemberKey existingMember existingUserId ident
 
    defaultLayout $ do
      $(widgetFile "SNTemplates/postSignup")

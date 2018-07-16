{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where
import Text.Julius          (juliusFile)

import Import

getSignupR :: Handler Html
getSignupR = do    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/signup")
       toWidget $(juliusFile "templates/SNTemplates/reglogin.julius")

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

putRegisterVerifyUserR :: Handler String
putRegisterVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]    
     if (existingUser > 0)
        then
            return "User exists"              
        else
            return "Username available"

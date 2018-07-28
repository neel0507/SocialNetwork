{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where
import Text.Julius          (juliusFile)
import Import

getSignupR :: Handler Html
getSignupR = do
    (widget, enctype) <- generateFormPost userForm --Generate the sign up form    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js" --Jquery script
       $(widgetFile "SNTemplates/signup") --template for signing up
       toWidget $(juliusFile "templates/SNTemplates/reglogin.julius") --associated javascript file

postSignupR :: Handler Html
postSignupR = do
    ((result, widget), enctype) <- runFormPost userForm --Get user registration details 
    case result of 
      FormSuccess user -> do
                        existingUser <- getUniqueUser (userIdent user) --Identify if it an existing user
                        existingUserId <- createUserRecordAndReturnUserKey existingUser (userIdent user) (userPassword user) --Insert user details in the database  

                        existingMember <- getUniqueMember existingUserId --Identify if the user already exists as a member
                        _ <- createMemberRecordAndReturnMemberKey existingMember existingUserId (userIdent user) --Insert member details in the database
 
                        defaultLayout $ do
                          $(widgetFile "SNTemplates/postSignup") --template to display after signup
      _ -> redirect SignupR --If unsuccessful signup 

putRegisterVerifyUserR :: Handler String
putRegisterVerifyUserR = do
     user <- requireJsonBody :: Handler User --Get Json data
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)] --Identify if it is an existing user   
     if (existingUser > 0)
        then
            return "User exists"              
        else
            return "Username available"


putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User --Get Json data
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)] --Identify if it is an existing user
     if (existingUser > 0)
        then return "Valid Username"
     else
             return "Invalid Username"

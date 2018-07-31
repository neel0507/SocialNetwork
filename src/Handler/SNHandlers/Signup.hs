{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where
import Import

getSignupR :: Handler Html
getSignupR = do
    (widget, enctype) <- generateFormPost userForm --Generate the sign up form    
    defaultLayout $ do
       $(widgetFile "SNTemplates/signup") --template for signing up

postSignupR :: Handler Html
postSignupR = do
    ((result, _), _) <- runFormPost userForm --Get user registration details 
    case result of 
      FormSuccess user -> do
                        existingUser <- getUniqueUser (userIdent user) --Identify if it an existing user
                        existingUserId <- createUserRecordAndReturnUserKey existingUser (userIdent user) (userPassword user) --Insert user details in the database  

                        existingMember <- getUniqueMember existingUserId --Identify if the user already exists as a member
                        _ <- createMemberRecordAndReturnMemberKey existingMember existingUserId (userIdent user) --Insert member details in the database
 
                        defaultLayout $ do
                          $(widgetFile "SNTemplates/postSignup") --template to display after signup
      _ -> redirect SignupR --If unsuccessful signup 


getRegisterVerifyUserR :: Text -> Handler String
getRegisterVerifyUserR uname= do
     existingUser <- runDB $ count [UserIdent ==. uname] --Identify if it is an existing user   
     if (existingUser > 0)
        then
            return "User exists"              
        else
            return "Username available"


getLoginVerifyUserR :: Text -> Handler String
getLoginVerifyUserR uname = do
     existingUser <- runDB $ count [UserIdent ==. uname] --Identify if it is a valid user
     if (existingUser > 0)
        then return "Valid username"
     else
             return "Invalid username"

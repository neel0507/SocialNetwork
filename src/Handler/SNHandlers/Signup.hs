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
    ((result, widget), enctype) <- runFormPost userForm --Get user registration details 
    case result of 
      FormSuccess user -> do
                        existingUser <- getUniqueUser (userIdent user) --Identify if it an existing user
                        existingUserId <- createUserRecordAndReturnUserKey existingUser (userIdent user) (userPassword user) --Insert user details in the database  

                        existingMember <- getUniqueMember existingUserId --Identify if the user already exists as a member
                        _ <- createMemberRecordAndReturnMemberKey existingMember existingUserId (userIdent user) --Insert member details in the database
                        defaultLayout $ do
                          $(widgetFile "SNTemplates/postSignup") --template to display after signup
      _ -> defaultLayout
           [whamlet|
             <p>Invalid input, lets try again.
             <form method=post action=@{SignupR} enctype=#{enctype}>
                ^{widget}                
                <button name="submit" class="submit">Submit
           |]


getRegisterVerifyUserR :: Text -> Handler String
getRegisterVerifyUserR uname= do
     existingUser <- runDB $ count [UserIdent ==. uname] --Identify if it is an existing user   
     if (existingUser > 0)
        then
            return "exists"              
        else
            return "is available"


getLoginVerifyUserR :: Text -> Handler String
getLoginVerifyUserR uname = do
     existingUser <- runDB $ count [UserIdent ==. uname] --Identify if it is a valid user
     if (existingUser > 0)
        then return "a valid username"
     else
             return "an invalid username"

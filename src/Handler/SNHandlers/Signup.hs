{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where
import Text.Julius          (juliusFile)
import Import

getSignupR :: Handler Html
getSignupR = do
    (widget, enctype) <- generateFormPost userForm    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/signup")
       toWidget $(juliusFile "templates/SNTemplates/reglogin.julius")

postSignupR :: Handler Html
postSignupR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of 
      FormSuccess user -> do
                        existingUser <- getUniqueUser (userIdent user)
                        existingUserId <- createUserRecordAndReturnUserKey existingUser (userIdent user) (userPassword user)  

                        existingMember <- getUniqueMember existingUserId
                        _ <- createMemberRecordAndReturnMemberKey existingMember existingUserId (userIdent user)
 
                        defaultLayout $ do
                          $(widgetFile "SNTemplates/postSignup")
      _ -> defaultLayout
           [whamlet|
             <p>Invalid input, let's try again.
             <form method=post action=@{SignupR} enctype=#{enctype}>
                ^{widget}                
                <button name="submit" class="submit">Submit
           |] 

putRegisterVerifyUserR :: Handler String
putRegisterVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]    
     if (existingUser > 0)
        then
            return "User exists"              
        else
            return "Username available"


putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]
     if (existingUser > 0)
        then return "Valid Username"
     else
             return "Invalid Username"

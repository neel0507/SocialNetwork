{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import
import Text.Julius          (juliusFile)

getLoginpageR :: Handler Html
getLoginpageR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/login")
       toWidget $(juliusFile "templates/SNTemplates/reglogin.julius")

postLoginpageR :: Handler Html
postLoginpageR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of 
      FormSuccess user -> do                       
                         existingUser <- getUniqueUser (userIdent user)         
                         isValid <- isSiteUser existingUser (userPassword user)
        
                         if isValid
                             then do
                                 _ <- setUserSessionId existingUser
                                 let username = userIdent user
                                 defaultLayout $ do
                                   $(widgetFile "SNTemplates/validUser")   
                             else     
                                 defaultLayout $ do
                                   [whamlet| <p>Invalid User |]
      _ -> defaultLayout
           [whamlet|
             <p>Invalid input, let's try again.
             <form method=post action=@{LoginpageR} enctype=#{enctype}>
                ^{widget}                
                <button name="submit" class="submit">Submit
           |]   

putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]
     if (existingUser > 0)
        then return "Valid Username"
     else
             return "Invalid Username"      

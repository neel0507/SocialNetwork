{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import
import Text.Julius          (juliusFile)

getLoginpageR :: Handler Html
getLoginpageR = do
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/login")
       toWidget $(juliusFile "templates/SNTemplates/reglogin.julius")

postLoginpageR :: Handler Html
postLoginpageR = do
    user <- getPostParameters "ident"
    password <- getPostParameters "password"
    
    existingUser <- getUniqueUser user    
    _ <- setUserSessionId existingUser
         
    isValid <- isSiteUser existingUser password
        
    if isValid
        then 
             defaultLayout $ do
               $(widgetFile "SNTemplates/validUser")   
        else     
             defaultLayout $ do
               [whamlet| <p>Invalid User |]   

putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]
     if (existingUser > 0)
        then return "Valid Username"
     else
             return "Invalid Username"      

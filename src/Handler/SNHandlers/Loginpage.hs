{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import

getLoginpageR :: Handler Html
getLoginpageR = do
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/login")

postLoginpageR :: Handler Html
postLoginpageR = do
    user <- getPostParameters "ident"
    password <- getPostParameters "password"
    
    existingUser <- getUniqueUser user    
    setUserSessionId existingUser
         
    isValid <- isSiteUser existingUser password
        
    if isValid
        then 
             defaultLayout $ do
               $(widgetFile "SNTemplates/validUser")   
        else     
             defaultLayout $ do
               [whamlet| <p>Invalid User |]         

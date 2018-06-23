{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where

import Import
import Yesod.Form (runInputPost, textField, ireq)
--import Yesod.Core

getSignupR :: Handler Html
getSignupR = do    
    defaultLayout $ do
       addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
       $(widgetFile "SNTemplates/signup")

postSignupR :: Handler Html
postSignupR = do
    ident <- runInputPost $ ireq textField "ident"
    password <- runInputPost $ ireq textField "password"
    
    existingUser <- runDB $ getBy $ UniqueUser ident
    _ <- case existingUser of
         Nothing -> runDB $ insert $ User 
          { userIdent = ident
          , userPassword = password
          }
         Just (Entity userId _) -> return userId
    redirect SignupR
         
     


{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Logoutpage where

import Import
import Text.Julius (juliusFile)

getLogoutpageR :: Handler Html
getLogoutpageR = do
    setSession "_ID" "0"
    defaultLayout $ do       
       $(widgetFile "SNTemplates/homepage")
       toWidget $(juliusFile "templates/SNTemplates/logout.julius")
       

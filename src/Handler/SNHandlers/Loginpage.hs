{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import
import Text.Lucius (luciusFile)

getLoginpageR :: Handler Html
getLoginpageR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/login")
       toWidget $(luciusFile "templates/SNTemplates/signup.lucius")

postLoginpageR :: Handler Html
postLoginpageR = error "Not yet implemented: postLoginpageR"

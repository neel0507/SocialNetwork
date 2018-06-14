{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Loginpage where

import Import

getLoginpageR :: Handler Html
getLoginpageR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/login")

postLoginpageR :: Handler Html
postLoginpageR = error "Not yet implemented: postLoginpageR"

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Homepage where

import Import

getHomepageR :: Handler Html
getHomepageR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/homepage")

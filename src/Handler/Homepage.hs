{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.Homepage where

import Import

getHomepageR :: Handler Html
getHomepageR = do
    defaultLayout $ do
       $(widgetFile "pageTemplate")

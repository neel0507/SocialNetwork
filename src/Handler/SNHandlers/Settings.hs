{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Settings where

import Import
import Database.Persist.Sql

getSettingsR :: Handler Html
getSettingsR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/settings")

postSettingsR :: Handler Html
postSettingsR = error "Not yet implemented: postSettingsR"

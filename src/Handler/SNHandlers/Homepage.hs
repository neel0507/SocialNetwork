{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Homepage where

import Import

getHomepageR :: Handler Html
getHomepageR = do
    uid <- lookupSession "_ID"
    sessUserId <- getMemberId uid

    if sessUserId > 0
        then
            defaultLayout $ do
              $(widgetFile "SNTemplates/loggedInMember")
        else
            defaultLayout $ do
              $(widgetFile "SNTemplates/homepage")

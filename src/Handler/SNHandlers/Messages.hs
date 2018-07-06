{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Messages where

import Import

getMessagesR :: Handler Html
getMessagesR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/messages")

postMessagesR :: Handler Html
postMessagesR = error "Not yet implemented: postMessagesR"

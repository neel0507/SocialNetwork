{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}


module Handler.SNHandlers.Friends where

import Import

getFriendsR :: Handler Html
getFriendsR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/friends")

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Members where

import Import

getMembersR :: Handler Html
getMembersR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/members")

postMembersR :: Handler Html
postMembersR = error "Not yet implemented: postMembersR"

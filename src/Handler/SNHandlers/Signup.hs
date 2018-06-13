{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Signup where

import Import

getSignupR :: Handler Html
getSignupR = do
    defaultLayout $ do
       $(widgetFile "SNTemplates/signup")

postSignupR :: Handler Html
postSignupR = error "Not yet implemented: postSignupR"

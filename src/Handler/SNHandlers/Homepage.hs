{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}

module Handler.SNHandlers.Homepage where

import Import

getHomepageR :: Handler Html
getHomepageR = do
    uid <- lookupSession "_ID"
    sessUserId <- getMemberId uid

    if sessUserId > 0
        then do            
            memberEntity <- getUniqueMember $ getUserKey sessUserId
            user <- getMemberName memberEntity "Does not exist"
            defaultLayout $ do
              $(widgetFile "SNTemplates/validUser")              
        else
            defaultLayout $ do
              $(widgetFile "SNTemplates/homepage")              

getLogoutpageR :: Handler Html
getLogoutpageR = do
    setSession "_ID" "0"
    defaultLayout $ do       
       $(widgetFile "SNTemplates/homepage")

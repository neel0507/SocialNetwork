{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Handler.SNHandlers.RegisterVerifyUser where

import Import

putRegisterVerifyUserR :: Handler String
putRegisterVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]
    -- returnJson existingUser
     if (existingUser > 0)
        then
            return "User exists"              
        else
            return "Username available" 
             

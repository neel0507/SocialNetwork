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
     if (existingUser > 0)
        then return "User exists, please select a different username"
     else
             return "This username is available"

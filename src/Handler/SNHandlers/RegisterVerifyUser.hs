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
     existingUser <- runDB $ getBy $ UniqueUser (userIdent user)
     return $ case existingUser of
           Just _  -> "User exists, please select a different username"
           Nothing -> "This username is available"

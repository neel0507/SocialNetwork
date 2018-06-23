module Handler.SNHandlers.RegisterVerifyUser where

import Import

putRegisterVerifyUserR :: Handler String
putRegisterVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ getBy $ UniqueUser (userIdent user)
     return $ case existingUser of
           Just _ -> "existing user"
           Nothing -> "username available"

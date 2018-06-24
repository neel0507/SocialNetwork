module Handler.SNHandlers.LoginVerifyUser where

import Import

putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ getBy $ UniqueUser (userIdent user)
     return $ case existingUser of
           Just _  -> "Valid Username"
           Nothing -> "Invalid Username"

module Handler.SNHandlers.LoginVerifyUser where

import Import

putLoginVerifyUserR :: Handler String
putLoginVerifyUserR = do
     user <- requireJsonBody :: Handler User
     existingUser <- runDB $ count [UserIdent ==. (userIdent user)]
     if (existingUser > 0)
        then return "Valid Username"
     else
             return "Invalid Username"

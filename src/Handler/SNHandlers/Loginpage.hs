{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
module Handler.SNHandlers.Loginpage where

import Import
import Text.Julius          (juliusFile) 

getLoginpageR :: Handler Html
getLoginpageR = do
   uid <- lookupSession "User_Id"     
   loggedInUserId <- getMemberId uid
   if loggedInUserId > 0
     then do
      redirect HomepageR
     else do
      (widget, enctype) <- generateFormPost userForm
      defaultLayout $ do       
         $(widgetFile "SNTemplates/login")     
 
postLoginpageR :: Handler Html
postLoginpageR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of 
      FormSuccess user -> do                       
                         existingUser <- getUniqueUser (userIdent user)         
                         isValid <- isSiteUser existingUser (userPassword user)
        
                         if isValid
                             then do
                                 _ <- setUserSessionId existingUser
                                 redirect HomepageR   
                             else     
                                 defaultLayout $ do
                                   [whamlet| <p>Invalid User |]
      _ -> defaultLayout
           [whamlet|
             <p>Invalid input, lets try again.
             <form method=post action=@{LoginpageR} enctype=#{enctype}>
                ^{widget}                
                <button name="submit" class="submit">Submit
           |]


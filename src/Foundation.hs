{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}


module Foundation where

import Prelude
import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Lucius          (luciusFile)
import Text.Julius          (juliusFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Database.Persist.Sql as PersQ
import Data.Time
import Yesod.Auth.Message
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
--import qualified Data.CaseInsensitive as CI
--import qualified Data.Text.Encoding as TE
import Yesod.Auth.HashDB (authHashDBWithForm, setPassword)
import Database.Esqueleto as E    
-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a


-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware Prelude.. defaultCsrfMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        uid <- lookupSession "User_Id"
        sitemember <- case uid of
           Just u -> runDB $ getBy $ UniqueMember (toSqlKey ((read $ unpack u)::Int64))
           Nothing -> return Nothing
    
        memberName <- case sitemember of
           Just (Entity _ dbMember) -> return $ unpack (memberIdent dbMember)
           Nothing -> return "" 

        let memberNameLength = Prelude.length memberName
        pc <- widgetToPageContent $ do
           widget
           addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js" 
           toWidget $(luciusFile "templates/SNTemplates/defaultLayout.lucius")
        withUrlRenderer
         [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>Social Network                                                          
                    ^{pageHead pc} 
                <body>
                    <div class="bddiv">
                     <div class="imgdiv">
                     $if memberNameLength > 0                        
                           <div class="title">Social Network (#{memberName})
                     $else
                           <div class="title">Social Network               
                     ^{pageBody pc}            
        |]

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ LoginpageR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (LoginpageR _) _ = return Authorized
    isAuthorized CommentR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized HomepageR _ = return Authorized
    isAuthorized SignupR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized MainImageR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (RegisterVerifyUserR _) _ = return Authorized
    isAuthorized (LoginVerifyUserR _) _ = return Authorized
    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized ProfileR _ = isAuthenticated
    isAuthorized MembersR _ = isAuthenticated
    isAuthorized FriendsR _ = isAuthenticated
    isAuthorized MessagesR _ = isAuthenticated
    isAuthorized SettingsR _ = isAuthenticated
    isAuthorized (ViewMemberR _) _ = isAuthenticated
    isAuthorized (ViewMemberMessagesR _) _ = isAuthenticated
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR Prelude.. flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" Prelude.++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return Prelude.. appLogger


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool


instance YesodAuth App where
    type AuthId App = UserId
    
    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomepageR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomepageR
    onLogout = do
         setMessage ""
         deleteSession "User_Id"
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = False

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [authHashDBWithForm loginPageForm (Just Prelude.. UniqueUser)]
       where
            loginPageForm :: Route App -> Widget
            loginPageForm action = do
                    mmsg <- getMessage
                    request <- getRequest
                    let mtok = reqToken request
                    result <- $(whamletFile "templates/SNTemplates/login.hamlet")
                    toWidget $(juliusFile "templates/SNTemplates/login.julius")
                    return result
    
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        _ <- liftHandler $ setUserSessionId x
        return $ case x of
            Nothing -> UserError InvalidLogin
            Just (Entity uid _) -> Authenticated uid

    loginHandler = do
        ma <- liftHandler $ maybeAuthId
        when (isJust ma) $
            liftHandler $ redirect HomepageR
        defaultLoginHandler

    authHttpManager = authHttpManager

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage


unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger


userForm :: Form User
userForm = renderDivs $ User
    <$> areq textField userNameSettings Nothing
    <*> areq passwordField passwordSettings Nothing
    where userNameSettings = FieldSettings
           { fsLabel = "Username",
             fsTooltip = Nothing,
             fsId = Just "username",
             fsName = Just "username",
             fsAttrs = [("class","signupunameField")]
           }
          passwordSettings = FieldSettings
           { fsLabel = "Password",
             fsTooltip = Nothing,
             fsId = Just "password",
             fsName = Just "password",
             fsAttrs = [("class","signupPasswordField")]
           }


getUniqueUser :: Text -> Handler (Maybe (Entity User))
getUniqueUser uname = do
       result <- liftHandler $ runDB $ getBy $ UniqueUser uname
       return result


getUniqueMember :: Key User -> Handler (Maybe (Entity Member))
getUniqueMember uKey = do
       result <- liftHandler $ runDB $ getBy $ UniqueMember uKey
       return result


getUniqueMemberAndProfileMessage :: Key User -> Key Member -> Handler (Maybe (Entity Member), Maybe (Entity ProfileMessage))
getUniqueMemberAndProfileMessage userKey memberKey= liftHandler $ runDB $ do
       memberEntity <- getBy $ UniqueMember userKey
       profileMessageEntity <- getBy $ UniqueProfileMessage memberKey
       return (memberEntity, profileMessageEntity)


getUniqueProfileMessage :: Key Member -> Handler (Maybe (Entity ProfileMessage))
getUniqueProfileMessage mKey = do
       result <- liftHandler $ runDB $ getBy $ UniqueProfileMessage mKey
       return result


createUserRecordAndReturnUserKey :: Maybe (Entity User) -> Text -> Text -> Handler (Key User)
createUserRecordAndReturnUserKey userEntity uname pass = do
      result <- case userEntity of
         Nothing -> do
           let user = User uname ""
           userPass <- setPassword pass user
           insertedUser <- runDB $ insertBy $ userPass
           return $
             case insertedUser of
               Left (Entity userid _) -> userid -- newly added user
               Right userid -> userid -- existing user
         Just (Entity userId _) -> return userId --existing user
      return result


createMemberRecordAndReturnMemberKey :: Maybe (Entity Member) ->  Key User -> Text -> Handler (Key Member)
createMemberRecordAndReturnMemberKey entityMember userId uname = do
      result <- case entityMember of
         Nothing ->
           liftHandler $ runDB $ insert $ Member
            { memberUserId = userId
            , memberIdent = uname
            }
         Just (Entity memberId _) -> return memberId
      return result


setUserSessionId :: Maybe (Entity User) -> Handler ()
setUserSessionId userEntity = do
      result <- case userEntity of
          Just(Entity userId _) -> setSession "User_Id" (pack $ show $ fromSqlKey userId)
          Nothing -> deleteSession "User_Id"
      return result


getUserKey :: Int64 -> Key User
getUserKey userId = toSqlKey $ userId
           


getMemberId :: Maybe (Text) -> Handler Int64
getMemberId uid = do
     return $ case uid of
        Just u -> read (unpack u) :: Int64
        Nothing  -> 0 :: Int64


getMemberKey :: Int64 -> Key Member
getMemberKey userId = toSqlKey $ userId


getMemberMessageKey :: Int64 -> Key MemberMessage
getMemberMessageKey mmKey = toSqlKey $ mmKey


getMemberName :: Maybe (Entity Member) -> String -> Handler Text
getMemberName memberEntity message = do
      result <- case memberEntity of
          Just (Entity _ dbMember)-> return $ memberIdent dbMember
          Nothing -> return $ pack message
      return result  


getProfileMessage :: Text -> Handler Text
getProfileMessage memberName= liftHandler $ runDB $ do
      memberEntity <- selectFirst [MemberIdent PersQ.==. memberName] []
      memberKey <- case memberEntity of
                 Just (Entity memberId _) -> return memberId
                 Nothing                  -> return $ toSqlKey (0::Int64)
      profileMessageEntity <- selectFirst [ProfileMessageMemberId PersQ.==. memberKey] []
      profileMessage <- case profileMessageEntity of
                 Just (Entity _ profileMessage) -> return $ unTextarea (profileMessageMessage profileMessage)
                 Nothing -> return $ pack ""
      return profileMessage 


addMemberToDB :: Int64 -> Key Member -> Key Member -> Handler Int64
addMemberToDB amId mKey addmKey = 
      if amId > 0
        then do
          a <- liftHandler $ runDB $ insert $ FollowingMembers mKey addmKey
          return $ fromSqlKey a
        else do
          return $ fromSqlKey mKey


removeMemberFromDB :: Int64 -> Key Member -> Key Member -> Handler Int64
removeMemberFromDB rmId mKey removeMKey = 
      if rmId > 0
        then do
          liftHandler $ runDB $ deleteWhere [FollowingMembersMemberId PersQ.==. mKey, FollowingMembersFollowingMemberId PersQ.==. removeMKey]
          return $ fromSqlKey mKey
        else do
          return $ fromSqlKey mKey


getMembers :: Key User -> [Entity Member] -> [Entity Member] -> [Entity Member]-> Key Member -> Handler [Entity Member]
getMembers uKey mutualMemberEntity flgMemberEntity flwMemberEntity mKey=
      if Prelude.null mutualMemberEntity && Prelude.null flgMemberEntity && Prelude.null flwMemberEntity 
        then do
           result <- liftHandler $ runDB $ selectList [MemberUserId PersQ.!=. uKey] [Asc MemberId]
           return result
        else do
           unfollowingMembers <- getUnFollowingMembers mKey uKey
           return unfollowingMembers 

getUnFollowingMembers :: Key Member -> Key User -> Handler [Entity Member]
getUnFollowingMembers mKey uKey = do
       result <- runDB
          $ select
          $ from $ \dbMember -> do                                                    
          E.where_ ((dbMember ^. MemberUserId E.!=. val uKey) &&.
                    (dbMember ^. MemberId `notIn` 
                       (subList_select 
                        $ from $ \followingMembers -> do
                        E.where_ (followingMembers ^. FollowingMembersMemberId E.==. val mKey) 
                        return 
                          (followingMembers ^. FollowingMembersFollowingMemberId)
                       )
                    ) &&.
                    (dbMember ^. MemberId `notIn` 
                       (subList_select 
                        $ from $ \followingMembers2 -> do
                        E.where_ (followingMembers2 ^. FollowingMembersFollowingMemberId E.==. val mKey) 
                        return 
                          (followingMembers2 ^. FollowingMembersMemberId)
                       )
                    )
                   )                            
          return 
           (dbMember)  
       return result                                                      
                        

getFollowingMembers :: Key Member -> Handler [Entity Member]
getFollowingMembers mKey = do
       result <- runDB
           $ select
           $ from $ \(followingMembers `InnerJoin` dbMember) -> do
           E.on (followingMembers ^. FollowingMembersFollowingMemberId E.==. dbMember ^. MemberId)
           E.where_ (followingMembers ^. FollowingMembersMemberId E.==. val mKey &&.
                     followingMembers ^. FollowingMembersFollowingMemberId `notIn`
                      (subList_select 
                       $ from $ \followingMembers2 -> do
                        E.where_ (followingMembers2 ^. FollowingMembersMemberId E.==. val mKey
                                  &&. followingMembers2 ^. FollowingMembersFollowingMemberId `in_`
                                   (subList_select
                                    $ from $ \followingMembers3 -> do
                                    E.where_ (followingMembers3 ^. FollowingMembersMemberId E.!=. val mKey &&.
                                              followingMembers3 ^. FollowingMembersFollowingMemberId E.==. val mKey) 
                                    return 
                                     (followingMembers3 ^. FollowingMembersMemberId)
                                   )
                                 ) 
                        return 
                          (followingMembers2 ^. FollowingMembersFollowingMemberId)
                      )
                    )
           return
            (dbMember)
       return result


getFollowers :: Key Member -> Handler [Entity Member]
getFollowers mKey = do
       result <- runDB
           $ select
           $ from $ \(followingMembers `InnerJoin` dbMember) -> do
           E.on (followingMembers ^. FollowingMembersMemberId E.==. dbMember ^. MemberId)
           E.where_ (followingMembers ^. FollowingMembersFollowingMemberId E.==. val mKey &&.
                     followingMembers ^. FollowingMembersMemberId `notIn`
                      (subList_select 
                       $ from $ \followingMembers2 -> do
                        E.where_ (followingMembers2 ^. FollowingMembersMemberId E.==. val mKey
                                  &&. followingMembers2 ^. FollowingMembersFollowingMemberId `in_`
                                   (subList_select
                                    $ from $ \followingMembers3 -> do
                                    E.where_ (followingMembers3 ^. FollowingMembersMemberId E.!=. val mKey &&.
                                              followingMembers3 ^. FollowingMembersFollowingMemberId E.==. val mKey) 
                                    return 
                                     (followingMembers3 ^. FollowingMembersMemberId)
                                   )
                                 ) 
                        return 
                          (followingMembers2 ^. FollowingMembersFollowingMemberId)
                      )
                    )
           return
            (dbMember)
       return result  


getMutualMembers :: Key Member -> Handler [Entity Member]  
getMutualMembers mKey = do
       result <- runDB
           $ select
           $ from $ \(followingMembers `InnerJoin` dbMember) -> do
             E.on (followingMembers ^. FollowingMembersFollowingMemberId E.==. dbMember ^. MemberId)
             E.where_ (followingMembers ^. FollowingMembersMemberId E.==. val mKey &&. 
                       followingMembers ^. FollowingMembersFollowingMemberId  `in_` 
                       (subList_select 
                        $ from $ \followingMembers2 -> do
                        E.where_ (followingMembers2 ^. FollowingMembersMemberId E.!=. val mKey &&.
                                  followingMembers2 ^. FollowingMembersFollowingMemberId E.==. val mKey) 
                        return 
                          (followingMembers2 ^. FollowingMembersMemberId)
                       )
                      )
             return
              (
               dbMember
              )
       return result


insertMessage :: Maybe (Entity ProfileMessage) -> Key Member -> Textarea -> Handler ()
insertMessage profileMessageEntity mKey tarea = do
     result <- case profileMessageEntity of
        Just (Entity _ _) -> liftHandler $ runDB $ updateWhere [ProfileMessageMemberId PersQ.==. mKey] [ProfileMessageMessage PersQ.=. tarea]
        Nothing -> do
             _ <- runDB $ insert $ ProfileMessage mKey tarea
             return ()
     return result


messageNotUpdated :: Int64
messageNotUpdated = 0


removeMessageFromDB :: Int64 -> Key MemberMessage -> Handler Int64
removeMessageFromDB rmId mmKey = 
     if rmId > 0
        then do
           liftHandler $ runDB $ deleteWhere [MemberMessageId PersQ.==. mmKey]
           return $ fromSqlKey mmKey
        else do
           return $ fromSqlKey mmKey


dateFormat :: UTCTime -> String
dateFormat = formatTime defaultTimeLocale "%d/%m/%Y %I:%M:%S %p"


addMinutes :: NominalDiffTime -> UTCTime -> UTCTime
addMinutes minutes = addUTCTime (minutes * 60) 


getLocalTime :: IO UTCTime
getLocalTime = do
    result <- getCurrentTime
    return (addMinutes 60 result)


getViewMemberKey :: Text -> Handler (Key Member)
getViewMemberKey viewMemberName = do
       memberEntity <- liftHandler $ runDB $ selectFirst [MemberIdent PersQ.==. viewMemberName] []
       memberId <- case memberEntity of
             Just (Entity memberId _) -> return memberId
             Nothing -> return $ toSqlKey (0::Int64)
       return memberId  


getMemberMessages :: Key Member -> Handler [((Entity MemberMessage), (E.Value Text))]
getMemberMessages mKey= do
    result <- runDB
      $ select
      $ from $ \(member_message `InnerJoin` dbMember)-> do
      E.on (member_message ^. MemberMessageFromMemberId E.==. dbMember ^. MemberId)                                                      
      E.where_ (member_message ^. MemberMessageMemberId E.==. val mKey)
      return
       ( member_message
       , dbMember ^. MemberIdent
       )
    return result               


getMessageDetails :: Int64 -> Handler ((Textarea, Bool, UTCTime, Key Member))
getMessageDetails loggedInUserId= do
    message <- runInputPost $ ireq textareaField "txtarea" --Get the message provided through the textbox
    messageType <- runInputPost $ ireq boolField "messagetype" --Get the message type (i.e. public/private)
    time <- liftIO getLocalTime --Get current time          
    let loggedInMemberKey = getMemberKey loggedInUserId --Get logged in entity member key
    return (message, messageType, time, loggedInMemberKey)


-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

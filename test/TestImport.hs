{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
import Database.Persist      as X hiding (get)
import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)
import Foundation            as X
import Model                 as X
import Test.Hspec            as X
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X
import Yesod.Core.Unsafe     (fakeHandlerGetLogger)

-- Wiping the database
import Database.Persist.Sqlite              (sqlDatabase, mkSqliteConnectionInfo, fkEnabled, createSqlitePoolFromInfo)
import Control.Monad.Logger                 (runLoggingT)
import Lens.Micro                           (set)
import Settings (appDatabaseConf)
import Yesod.Core (messageLoggerSource)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        useEnv
    foundation <- makeFoundation settings
    wipeDB foundation
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to use a connection which has
    -- foreign key checks disabled.  Foreign key checks are enabled or disabled
    -- per connection, so this won't effect queries outside this function.
    --
    -- Aside: foreign key checks are enabled by persistent-sqlite, as of
    -- version 2.6.2, unless they are explicitly disabled in the
    -- SqliteConnectionInfo.

    let logFunc = messageLoggerSource app (appLogger app)

    let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
        connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

    pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
    tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
    return (fmap unSingle tables)

createUser :: Text -> Text -> YesodExample App ()
createUser username password= runDB $ do
    user <- insertEntity User
        { userIdent = username
        , userPassword = password
        }
    return ()


doSignup :: Text -> Text -> YesodExample App ((Text), (Text))
doSignup username password = do
    get SignupR
    statusIs 200
    request $ do
      setMethod "POST"
      setUrl SignupR
      addPostParam "submit" ""
      addPostParam "password" password
      addPostParam "username" username
      addToken
    return (username, password)
    

doLogin :: Text -> Text -> YesodExample App ()
doLogin username password = do   
   get LoginpageR
   statusIs 200
   request $ do
    setMethod "POST"
    setUrl LoginpageR
    addPostParam "submit" ""
    addPostParam "password" password
    addPostParam "username" username
    addToken
   return ()

postProfileMessage :: Text -> YesodExample App ()
postProfileMessage message = do
    request $ do
      setMethod "POST"
      setUrl SettingsR
      addPostParam "txtarea" message
      addPostParam "submit" ""
    return ()

postPrivateMessage :: Text -> YesodExample App ()
postPrivateMessage message = do
    request $ do
      setMethod "POST"
      setUrl MessagesR
      addPostParam "txtarea" message
      addPostParam "messagetype" "on"
      addPostParam "submit" ""
    return ()

postPublicMessage :: Text -> YesodExample App ()
postPublicMessage message = do
    request $ do
      setMethod "POST"
      setUrl MessagesR
      addPostParam "txtarea" message
      addPostParam "messagetype" "no"
      addPostParam "submit" ""
    return ()

postPrivateMessageToMember :: Text -> Text -> YesodExample App ()
postPrivateMessageToMember message member = do
    request $ do
      setMethod "POST"
      setUrl (ViewMemberMessagesR member)
      addPostParam "txtarea" message
      addPostParam "messagetype" "on"
      addPostParam "submit" ""
    return ()

postPublicMessageToMember :: Text -> Text -> YesodExample App ()
postPublicMessageToMember message member = do
    request $ do
      setMethod "POST"
      setUrl (ViewMemberMessagesR member)
      addPostParam "txtarea" message
      addPostParam "messagetype" "no"
      addPostParam "submit" ""
    return ()

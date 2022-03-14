{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Brainard.Server where

import Brainard.Model (Rememberance)
import qualified Brainard.Model as M
import Common
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Database.Esqueleto.Experimental (BackendCompatible, Entity (Entity), SqlBackend, SqlPersistT, runSqlConn, toSqlKey, runMigrationSilent, runMigration, fromSqlKey, Value (unValue))
import Servant
import Servant.JS
import Servant.Swagger
import Data.Swagger(Swagger, ToSchema, info, title, version)
import Control.Exception (try, throwIO)
import Database.Persist.Sqlite (withSqliteConn)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (ToJSON, ToJSON2 (liftToJSON2), object, KeyValue ((.=)), defaultOptions, genericToEncoding)
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Data.Aeson.Types (ToJSON2(liftToEncoding2), ToJSON (toEncoding))
import Control.Lens

data DbRecord a b = DbRecord { key :: a, val :: b } deriving (Eq, Ord, Show, Generic, Typeable)

instance (ToJSON a, ToJSON b) => ToJSON (DbRecord a b) where
  toEncoding = genericToEncoding defaultOptions

instance (ToSchema a, ToSchema b) => ToSchema (DbRecord a b)

type RememberancesGet = QueryParam "limit" Int64 :> QueryParam "offset" Int64 :> Get '[JSON] [DbRecord Int64 Rememberance]

type PostRememberance = ReqBody '[JSON] Text :> Post '[JSON] Int64

type UpvoteRememberance = "upvote" :> PostNoContent

type DownvoteRememberance = "downvote" :> PostNoContent

type InteractRememberance = Capture "id" Int64 :> (UpvoteRememberance :<|> DownvoteRememberance)

type MainAPI = "rememberances" :> (RememberancesGet :<|> PostRememberance :<|> InteractRememberance)

type API = MainAPI :<|> ("api" :> Get '[JSON] Swagger) :<|> Raw

type AppContext m = (MonadIO m, MonadReader SqlBackend m)

mainAPI :: Proxy MainAPI
mainAPI = Proxy

api :: Proxy API
api = Proxy

apiJs :: FilePath -> IO ()
apiJs = writeJSForAPI mainAPI vanillaJS

apiSwagger :: (Monad m) => m Swagger
apiSwagger = return $ toSwagger mainAPI
  & info.title .~ "Brainard"
  & info.version .~ "1.0"

type APIContext m = (MonadUnliftIO m, MonadReader SqlBackend m)

apiServer :: APIContext m => ServerT API m
apiServer =
  (getRememberances :<|> postRememberance
    :<|> (\id -> upvoteRememberance id :<|> downvoteRememberance id))
  :<|> apiSwagger
  :<|> serveDirectoryWebApp "static"

getRememberances ::
  (APIContext m) =>
  Maybe M.Limit ->
  Maybe M.Offset ->
  m [DbRecord Int64 Rememberance]
getRememberances limitM offsetM =
  let limit = fromMaybe 100 limitM
      offset = fromMaybe 0 offsetM
   in runDb $ do
     rs <- M.getRememberancesByScorePageWithScore limit offset Nothing
     liftIO $ mapM_ (\(_, v) -> print $ unValue v) rs
     let rs' = map (\(Entity k v, _) -> DbRecord (fromSqlKey k) v) rs
     return rs'

postRememberance :: APIContext m => Text -> m Int64
postRememberance body = runDb $ do
  key <- M.postRememberance body Nothing
  return $ fromSqlKey key

upvoteRememberance :: APIContext m => Int64 -> m NoContent
upvoteRememberance id = runDb $ do
  x <- M.upvote . toSqlKey $ id
  unless x . liftIO $ throwIO err404
  return NoContent

downvoteRememberance :: APIContext m => Int64 -> m NoContent
downvoteRememberance id = runDb $ do
  x <- M.downvote . toSqlKey $ id
  unless x . liftIO $ throwIO err404
  return NoContent

type AppT m a = SqlPersistT m a

type App a = AppT IO a

runApp :: MonadIO m => AppT m a -> SqlBackend -> m a
runApp = runReaderT

convertApp :: App a -> SqlBackend -> Handler a
convertApp a = Handler . ExceptT . try . runApp a

runDb :: APIContext m => SqlPersistT m b -> m b
runDb action = ask>>=runSqlConn action

hoistAppServer :: SqlBackend -> Server API
hoistAppServer b = hoistServer api (`convertApp` b) apiServer

app :: SqlBackend -> Application
app = serve api . hoistAppServer

runAppSqliteConn :: Text -> IO ()
runAppSqliteConn conn = runStderrLoggingT $ withSqliteConn conn (\db -> do
  runReaderT (runMigration M.migrateAll) db
  LoggingT (\l -> run 8081 . app $ db))
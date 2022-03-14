{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
module Brainard.Model where

import Database.Esqueleto.Experimental
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Text (Text)
import Common
import Data.Coerce (coerce)
import GHC.Generics (Generic)
import Data.Data (Typeable)
import Data.Swagger (ToSchema)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Rememberance json
    body Text sqltype=varchar(512)
    created UTCTime default=CURRENT_TIMESTAMP
    createdEpochSeconds Int64 default=0
    upvotes Int64 default=0
    downvotes Int64 default=0
    deriving Eq Show Generic Typeable
|]

instance ToSchema Rememberance

type InsertContext m b = (MonadIO m, PersistStoreWrite b, BaseBackend b ~ SqlBackend)
type UpdateContext m b = (MonadIO m, BackendCompatible SqlBackend b, PersistQueryWrite b, PersistUniqueWrite b)

timeOrCurrent :: (MonadIO m) => Maybe UTCTime -> m UTCTime
timeOrCurrent (Just x) = return x
timeOrCurrent Nothing = liftIO getCurrentTime

secondsSinceEpoch :: UTCTime -> Int64
secondsSinceEpoch t =
    let x = t `diffUTCTime` UTCTime (fromOrdinalDate 1970 1) (secondsToDiffTime 0)
    in floor $ nominalDiffTimeToSeconds x

postRememberance :: (InsertContext m b) => Text -> Maybe UTCTime -> ReaderT b m (Key Rememberance)
postRememberance body mTime = do
  t <- timeOrCurrent mTime
  let d = secondsSinceEpoch t
  insert (Rememberance body t d 0 0)

getRememberances :: MonadIO m => SqlReadT m [Entity Rememberance]
getRememberances = select $ do
  r <- from $ table @Rememberance
  orderBy [ desc $ r^.RememberanceCreated ]
  pure r

type Limit = Int64
type Offset = Int64

score :: SqlExpr (Entity Rememberance) -> UTCTime -> SqlExpr (Value Double)
score r now = score
  where
    nowS = secondsSinceEpoch now
    -- Idea from:  https://julesjacobs.com/2015/08/17/bayesian-scoring-of-ratings.html
    upvotes :: SqlExpr (Value Double)
    upvotes = castNum  (r ^.RememberanceUpvotes) +. val 4.0
    downvotes :: SqlExpr (Value Double)
    downvotes = castNum (r ^. RememberanceDownvotes) +. val 10.0

    age = castNum (val nowS) -. castNum (r ^. RememberanceCreatedEpochSeconds)
    ageFactor = val 86400  /. (age +. val 1) 
    score = ageFactor *. (upvotes /. (upvotes +. downvotes))

getRememberancesByScorePage :: MonadIO m => Limit -> Offset -> Maybe UTCTime -> SqlReadT m [Entity Rememberance]
getRememberancesByScorePage limitN offsetN mTime =
  map fst <$> getRememberancesByScorePageWithScore limitN offsetN mTime

getRememberancesByScorePageWithScore :: MonadIO m => Limit -> Offset -> Maybe UTCTime -> SqlReadT m [(Entity Rememberance, Value Double)]
getRememberancesByScorePageWithScore limitN offsetN mTime = do
  n <- timeOrCurrent mTime
  select $ do
    r <- from $ table @Rememberance
    orderBy [ desc $ score r n]
    pure (r, score r n)

upvote :: UpdateContext m b => RememberanceId -> ReaderT b m Bool
upvote rememberanceId = do
  n <- updateCount $ \r ->  do
    set r [RememberanceUpvotes =. (r^.RememberanceUpvotes) +. val 1]
    where_ $ r^.RememberanceId ==. val rememberanceId
  return $ n == 1

downvote :: UpdateContext m b => RememberanceId -> ReaderT b m Bool
downvote rememberanceId = do
  n <- updateCount $ \r -> do
    set r [RememberanceDownvotes =. (r^.RememberanceDownvotes) +. val 1]
    where_ $ r^.RememberanceId ==. val rememberanceId
  return $ n == 1

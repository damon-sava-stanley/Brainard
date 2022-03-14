{-# LANGUAGE OverloadedStrings #-}

module Brainard.ModelSpec where

import Brainard.Model
import Common
import Conduit (ResourceT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Database.Esqueleto.Experimental
import qualified Database.Persist.Sqlite as S
import Test.Hspec


sqlite :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a
sqlite action = S.runSqlite ":memory:" $ do
  runMigrationSilent migrateAll
  action

time :: Integer -> UTCTime
time n = UTCTime (fromOrdinalDate 2022 1) (secondsToDiffTime n)

spec :: SpecWith ()
spec = do
  describe "Rememberance Crud" $ do
    it "Can Be Added" $
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        rs' <- getRememberances
        let rs = map entityVal rs'
        liftIO $ rs `shouldBe` [Rememberance "A" (time 0) 0 0]
    it "Orders by Date" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        postRememberance "B" (Just $ time 1)
        rs' <- getRememberances 
        let rs = map (rememberanceBody . entityVal) rs'
        liftIO $ rs `shouldBe` ["B", "A"]
    it "Can Upvote" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        rs' <- getRememberances 
        let k = entityKey . head $ rs'
        upvote k
        rs' <- getRememberances
        let upvotes = rememberanceUpvotes . entityVal . head $ rs'
        liftIO $ upvotes `shouldBe` 1
    it "Can Downvote" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        rs' <- getRememberances 
        let k = entityKey . head $ rs'
        downvote k
        rs' <- getRememberances
        let downvotes = rememberanceDownvotes . entityVal . head $ rs'
        liftIO $ downvotes `shouldBe` 1
  describe "Rememberance Scores" $ do
    it "Upvotes Increase Score" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        postRememberance "B" (Just $ time 0)
        rs' <- getRememberances
        let [a, b] = map entityKey rs'
        upvote a
        rs' <- getRememberancesByScorePage 2 0 (Just $ time 1)
        let favoredKey = entityKey . head $ rs'
        liftIO $ a `shouldBe` favoredKey
    it "Downvote Decreases Score" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 0)
        postRememberance "B" (Just $ time 0)
        rs' <- getRememberances
        let [a, b] = map entityKey rs'
        downvote b
        rs' <- getRememberancesByScorePage 2 0 (Just $ time 1)
        let favoredKey = entityKey . head $ rs'
        liftIO $ a `shouldBe` favoredKey
    it "Age Decreases Score" $ do
      sqlite $ do
        postRememberance "A" (Just $ time 100)
        postRememberance "B" (Just $ time 0)
        rs' <- getRememberances
        let [a, b] = map entityKey rs'
        downvote b
        rs' <- getRememberancesByScorePage 2 0 (Just $ time 100)
        let favoredKey = entityKey . head $ rs'
        liftIO $ a `shouldBe` favoredKey
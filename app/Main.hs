{-# LANGUAGE OverloadedStrings #-}
module Main where
import Brainard.Server
import Control.Monad(when) 
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  when ("gen" `elem` args) $ apiJs "static/js/query.js"
  when ("run" `elem` args) $ runAppSqliteConn "test.db"
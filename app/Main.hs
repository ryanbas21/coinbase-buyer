module Main where

import Configuration.Dotenv (Config, defaultConfig, loadFile)
import Control.Monad.Reader

type GdaxConfig = [(String, String)]

gdaxConfig :: IO GdaxConfig
gdaxConfig = loadFile defaultConfig

main :: IO ()
main = do
  config <- gdaxConfig
  putStrLn $ show config

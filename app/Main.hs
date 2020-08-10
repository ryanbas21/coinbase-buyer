{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Configuration.Dotenv (Config, defaultConfig, loadFile)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time.Clock.POSIX

type GdaxConfig = [(String, String)]

data RequestPath = RequestPath
  { orders :: String
  }
  deriving (Show)

data OrderBody = OrderBody
  { price :: T.Text,
    size :: T.Text,
    side :: T.Text,
    product_id :: T.Text
  }
  deriving (Show)

instance ToJSON OrderBody where
  toJSON (OrderBody price size side product_id) =
    object
      [ "price" .= price,
        "size" .= size,
        "side" .= side,
        product_id .= product_id
      ]

  toEncoding (OrderBody price size side product_id) =
    pairs
      ( "price" .= price
          <> "size" .= size
          <> "side" .= side
          <> product_id .= product_id
      )

gdaxConfig :: IO GdaxConfig
gdaxConfig = loadFile defaultConfig

getTime :: IO Integer
getTime = round <$> getPOSIXTime

btcOrder :: OrderBody
btcOrder =
  OrderBody
    { price = "1.0",
      size = "1.0",
      side = "buy",
      product_id = "BTC-USD"
    }

createRequest :: B.ByteString -> B.ByteString
createRequest time = do
  value <- time <> (B.pack "GET") <> (B.pack "/orders") <> (encode btcOrder)
  pure value

main :: IO ()
main = do
  -- config <- gdaxConfig
  time <- B.pack . fromIntegral <$> getTime
  B.putStrLn $ (createRequest time)

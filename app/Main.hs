{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import Servant.Types.SourceT (source)
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import GHC.RTS.Flags (TraceFlags(user))

type UserAPI5 = Capture "echo" String :> Get '[JSON] EchoMessage
                -- equivalent to 'GET /:echo'
           :<|> "user" :> Capture "userid" Integer :> Get '[JSON] HelloMessage
                -- equivalent to 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

newtype EchoMessage = EchoMessage String
    deriving Generic
instance ToJSON EchoMessage

newtype HelloMessage = HelloMessage Integer
    deriving Generic
instance ToJSON HelloMessage

-- Alternative
--newtype HelloMessage = HelloMessage { msg :: Integer }
--    deriving Generic
--instance ToJSON HelloMessage

server1 :: Server UserAPI5
server1 = echo :<|>  user
    where echo :: String -> Handler EchoMessage
          echo str = return . EchoMessage $ str

          user :: Integer -> Handler HelloMessage
          user mname = return . HelloMessage $ mname

-- "boilerplate"
userAPI :: Proxy UserAPI5
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = run 8081 app1

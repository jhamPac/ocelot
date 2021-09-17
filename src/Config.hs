{-# LANGUAGE FlexibleInstances #-}

module Config where

import           Data.Aeson            (eitherDecode, withObject, (.:))
import           Data.Bifunctor        (first)
import           Data.ByteString.Lazy  as DBL
import           Data.Functor.Identity (Identity (Identity))

type Host = String
type Port = Integer

type CompleteAppConfig = AppConfig Identity
type PartialAppConfig = AppConfig Maybe

data AppConfig f = AppConfig {acHost :: f Host, acPort :: f Port}

class HasDefaultValue a where
    defaultValue :: a

defaultHost :: Host
defaultHost = "localhost"

defaultPort :: Port
defaultPort = 5000

instance HasDefaultValue (AppConfig Identity) where
    defaultValue = AppConfig (Identity defaultHost) (Identity defaultPort)

instance HasDefaultValue (AppConfig Maybe) where
    defaultValue = AppConfig (Just defaultHost) (Just defaultPort)




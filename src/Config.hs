{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


module Config where

import           Data.Aeson            (FromJSON (parseJSON), eitherDecode,
                                        toJSON)
import           Data.Bifunctor        (first)
import           Data.ByteString.Lazy  as DBL
import           Data.Functor.Identity (Identity (Identity))
import           GHC.Generics
import           System.FilePath       as FP
import           Text.Parsec.Error     (ParseError)

type Host = String
type Port = Integer

data AppConfig f = AppConfig
    { acHost :: f Host
    , acPort :: f Port
    } deriving (Generic)

type CompleteAppConfig = AppConfig Identity
deriving instance Generic CompleteAppConfig
deriving instance Eq CompleteAppConfig
deriving instance Show CompleteAppConfig
deriving instance FromJSON CompleteAppConfig

type PartialAppConfig = AppConfig Maybe
deriving instance Generic PartialAppConfig
deriving instance Eq PartialAppConfig
deriving instance Show PartialAppConfig
deriving instance FromJSON PartialAppConfig

data ConfigurationError = ConfigParseError String
                        | TOMLParseError ParseError
                        | InvalidConfigError String
                        | InvalidPath FP.FilePath String
                            deriving (Eq)

class (FromJSON cfg) => FromJSONFile cfg where
    fromJSONFile :: FilePath -> IO (Either ConfigurationError cfg)

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

instance FromJSON PartialAppConfig where
    parseJSON = withObject "cfg" parseObj
        where
            parseObj :: Object -> Parser PartialAppConfig
            parseObj obj = obj .: "host"
                            >>= \host -> obj .: "port"
                            >>= \port -> pure $ AppConfig {host=host, port=port}




module Main where

import           Control.Monad       (join)
import           Data.Semigroup      ((<>))
import           Lib
import           Options.Applicative (CommandFields, Mod, Parser, ParserInfo,
                                      argument, command, execParser, idm, info,
                                      str, subparser)

type Host = String
type Port = Integer

data Command = Serve Host Port

newtype Options = Options {cmd :: Command}

server :: IO ()
server = putStrLn "Ocelot...Server"

opts :: Parser (IO ())
opts = subparser commands
    where
        serverAction :: Parser (IO ())
        serverAction = pure server

        serverCmd :: ParserInfo (IO ())
        serverCmd = info serverAction idm

        commands :: Mod CommandFields (IO ())
        commands = command "server" serverCmd



main :: IO ()
main = join $ execParser parser
    where
        parser :: ParserInfo (IO ())
        parser = info opts idm

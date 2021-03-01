module Main where

import Control.Applicative

import Control.Concurrent

import Control.Monad

import Options.Applicative

import Network.TT.TCP

data Config = TCPServer String Int
            | TCPClient String Int Int Int

parseConfig :: ParserInfo Config
parseConfig = info (config <**> helper) $ mconcat
    [ fullDesc
    , progDesc "Stupid Simple Network Throughput Test"
    ]

config :: Parser Config
config = hsubparser $ mconcat
    [ command "tcp-server"
        (info optTcpServer
            (fullDesc <> progDesc "TCP Test Server")
        )
    , command "tcp-client"
        (info optTcpClient
            (fullDesc <> progDesc "UDP Test Client")
        )
    ]

optTcpServer :: Parser Config
optTcpServer = TCPServer <$> parseServerHost <*> parsePort

optTcpClient :: Parser Config
optTcpClient = TCPClient <$> parseClientHost
                         <*> parsePort
                         <*> parsePayloadSize
                         <*> parseSamplePeriod

parseServerHost :: Parser String
parseServerHost = strOption $ mconcat
    [ long "host"
    , short 'h'
    , help "Listen host."
    , metavar "HOST"
    , value "0.0.0.0"
    ]

parsePort :: Parser Int
parsePort = option auto $ mconcat
    [ long "port"
    , short 'p'
    , help "Port."
    , metavar "PORT"
    , value 23456
    ]

parseClientHost :: Parser String
parseClientHost = strOption $ mconcat
    [ long "host"
    , short 'h'
    , help "Connect host."
    , metavar "HOST"
    ]

parsePayloadSize :: Parser Int
parsePayloadSize = option auto $ mconcat
    [ long "chunk"
    , short 's'
    , help "Test payload chunk size."
    , metavar "CHUNK_SIZE"
    , value 1000000
    ]

parseSamplePeriod :: Parser Int
parseSamplePeriod = option auto $ mconcat
    [ long "period"
    , short 'd'
    , help "Throughput sample period in seconds."
    , metavar "PERIOD_SECONDS"
    , value 2
    ]

printT :: Double -> String
printT t
    | t > 1000000000 = show (t / 1000000000) ++ " GB/sec"
    | t > 1000000 = show (t / 1000000) ++ " MB/sec"
    | t > 1000 = show (t / 1000) ++ " KB/sec"
    | otherwise = show t ++ " B/sec"

runConfig :: Config -> IO ()
runConfig (TCPServer h p) = tcpServer h p
runConfig (TCPClient h p ps sp) = do
    tr <- tcpClient h p ps
    forever $ do
        threadDelay (sp * 1000000)
        t <- tcpObs tr
        putStrLn $ printT t

main :: IO ()
main = customExecParser opts parseConfig >>= runConfig
    where opts = prefs (showHelpOnEmpty <> showHelpOnError)

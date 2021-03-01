module Network.TT.TCP where

import Control.Concurrent

import Control.Concurrent.STM

import Control.Monad

import qualified Data.ByteString as BS

import Data.Time.Clock.POSIX

import Network.Socket
import Network.Socket.ByteString

getAI :: AddrInfo -> HostName -> Int -> IO AddrInfo
getAI hints h p = do
    ais <- getAddrInfo (Just hints) (Just h) (Just (show p))
    case ais of
        [] -> error "getAddrInfo failed"
        (ai:_) -> pure ai

tcpServer :: String -> Int -> IO ()
tcpServer h p = do
    let hints = defaultHints {
            addrFamily = AF_INET
          , addrSocketType = Stream
          }
    ai <- getAI hints h p
    s <- openSocket ai
    setSocketOption s ReuseAddr 1
    bind s $ addrAddress ai
    listen s 1024
    forever $ do
        (cs, ca) <- accept s
        print ca
        forkIO $ tcpServerWorker cs

-- Just gobble up all the bytes without doing anything.
tcpServerWorker :: Socket -> IO ()
tcpServerWorker s = do
    bs <- recv s 1000000
    if BS.null bs
    then close s
    else tcpServerWorker s

data TCPResults = TCPResults {
    trBytes :: TVar Integer
  , trLastObsTime :: TVar POSIXTime
  }

tcpClient :: String -> Int -> Int -> IO TCPResults
tcpClient h p cs = do
    let hints = defaultHints {
            addrFamily = AF_INET
          , addrSocketType = Stream
          }
        bs = BS.replicate cs 0xbe
    ai <- getAI hints h p
    s <- openSocket ai
    connect s $ addrAddress ai
    tb <- newTVarIO 0
    tl <- getPOSIXTime >>= newTVarIO
    forkIO $ tcpClientWorker bs s tb
    pure $ TCPResults tb tl

tcpClientWorker :: BS.ByteString -> Socket -> TVar Integer -> IO ()
tcpClientWorker bs s tb = 
    let ss = fromIntegral $ BS.length bs
    in forever $ do
        sendAll s bs
        atomically $ modifyTVar' tb (+ss)

tcpObs :: TCPResults -> IO Double
tcpObs (TCPResults tb tl) = do
    t <- getPOSIXTime
    (t', bc) <- atomically $ do
        t' <- readTVar tl
        bc <- readTVar tb
        writeTVar tl t
        writeTVar tb 0
        pure (t', bc)
    pure (fromIntegral bc / (fromRational (toRational (t - t'))))

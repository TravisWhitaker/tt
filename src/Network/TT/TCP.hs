module Network.TT.TCP where

import Control.Concurrent

import Control.Concurrent.STM

import Control.Monad

import qualified Data.ByteString as BS

import Data.Time.Clock.POSIX

import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as N

openSocket :: N.AddrInfo -> IO N.Socket
openSocket addr = N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)

getAI :: N.AddrInfo -> N.HostName -> Int -> IO N.AddrInfo
getAI hints h p = do
    ais <- N.getAddrInfo (Just hints) (Just h) (Just (show p))
    case ais of
        [] -> error "getAddrInfo failed"
        (ai:_) -> pure ai

tcpServer :: String -> Int -> IO ()
tcpServer h p = do
    let hints = N.defaultHints {
            N.addrFamily = N.AF_INET
          , N.addrSocketType = N.Stream
          }
    ai <- getAI hints h p
    s <- openSocket ai
    N.setSocketOption s N.ReuseAddr 1
    N.bind s $ N.addrAddress ai
    N.listen s 1024
    forever $ do
        (cs, ca) <- N.accept s
        print ca
        forkIO $ tcpServerWorker cs

-- Just gobble up all the bytes without doing anything.
tcpServerWorker :: N.Socket -> IO ()
tcpServerWorker s = do
    bs <- N.recv s 1000000
    if BS.null bs
    then N.close s
    else tcpServerWorker s

data TCPResults = TCPResults {
    trBytes :: TVar Integer
  , trLastObsTime :: TVar POSIXTime
  }

tcpClient :: String -> Int -> Int -> IO TCPResults
tcpClient h p cs = do
    let hints = N.defaultHints {
            N.addrFamily = N.AF_INET
          , N.addrSocketType = N.Stream
          }
        bs = BS.replicate cs 0xbe
    ai <- getAI hints h p
    s <- openSocket ai
    N.connect s $ N.addrAddress ai
    tb <- newTVarIO 0
    tl <- getPOSIXTime >>= newTVarIO
    forkIO $ tcpClientWorker bs s tb
    pure $ TCPResults tb tl

tcpClientWorker :: BS.ByteString -> N.Socket -> TVar Integer -> IO ()
tcpClientWorker bs s tb = 
    let ss = fromIntegral $ BS.length bs
    in forever $ do
        N.sendAll s bs
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

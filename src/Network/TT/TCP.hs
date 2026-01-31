{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           #-}

module Network.TT.TCP where

import Control.Concurrent

import Control.Concurrent.STM

import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS

import Data.Time.Clock.POSIX

import Data.Word

import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr

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

tcpServer :: String -> Int -> Int -> IO ()
tcpServer h p c = do
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
        N.setSocketOption cs N.RecvBuffer c
        N.setSocketOption cs N.NoDelay 1
        forkIO $ tcpServerWorker c cs

-- Just gobble up all the bytes without doing anything.
tcpServerWorker :: Int -> N.Socket -> IO ()
tcpServerWorker c s =
    let go = do
            -- bs <- N.recv s c
            bs <- spinRecv s c
            if BS.null bs
            then N.close s
            else go
    in go

data TCPResults = TCPResults {
    trBytes :: TVar Integer
  , trLastObsTime :: TVar POSIXTime
  }

oneTCPClient :: TCPResults -> String -> Int -> Int -> IO ()
oneTCPClient TCPResults{..} h p cs =  do
    let hints = N.defaultHints {
            N.addrFamily = N.AF_INET
          , N.addrSocketType = N.Stream
          }
        bs = BS.replicate cs 0xbe
    ai <- getAI hints h p
    s <- openSocket ai
    N.connect s $ N.addrAddress ai
    N.setSocketOption s N.SendBuffer cs
    N.setSocketOption s N.NoDelay 1
    forkIO $ tcpClientWorker bs s trBytes
    pure ()

tcpClients :: Int -> String -> Int -> Int -> IO TCPResults
tcpClients ns h p cs = do
    trBytes <- newTVarIO 0
    trLastObsTime <- getPOSIXTime >>= newTVarIO
    let tr = TCPResults{..}
    replicateM_ ns $ oneTCPClient tr h p cs
    pure tr

tcpClientWorker :: BS.ByteString -> N.Socket -> TVar Integer -> IO ()
tcpClientWorker bs s tb = 
    let ss = fromIntegral $ BS.length bs
    in forever $ do
        -- N.sendAll s bs
        spinSendAll s bs
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
    pure (fromIntegral bc / fromRational (toRational (t - t')))

spinRecv :: N.Socket -> Int -> IO BS.ByteString
spinRecv s n = BS.createAndTrim n $ \ptr -> spinRecvBuf s ptr n

spinRecvBuf :: N.Socket -> Ptr Word8 -> Int -> IO Int
spinRecvBuf s ptr n = do
    len <- N.withFdSocket s $ \fd ->
            throwErrnoIfMinus1 "spinRecvBuf" $ spin_recv fd (castPtr ptr) (fromIntegral n)
    pure $ fromIntegral len

spinSendAll :: N.Socket -> BS.ByteString -> IO ()
spinSendAll s bs = BS.unsafeUseAsCStringLen bs $ \(str, len) ->
    spinSendBuf s (castPtr str) len

spinSendBuf :: N.Socket -> Ptr Word8 -> Int -> IO ()
spinSendBuf s ptr n = N.withFdSocket s $ \fd -> do
    len <- throwErrnoIfMinus1 "spinSendBuf" $ spin_send fd (castPtr ptr) (fromIntegral n)
    if fromIntegral n /= len
    then error ("spinSendBuf: wrong len " <> show len)
    else pure ()

foreign import ccall unsafe spin_recv :: CInt -> Ptr () -> CSize -> IO CInt

foreign import ccall unsafe spin_send :: CInt -> Ptr () -> CSize -> IO CInt

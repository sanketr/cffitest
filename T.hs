import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar, readMVar)
import Control.Monad.Primitive (PrimState)
import Control.Monad (mapM, mapM_, forM, forM_)
import Control.Exception
import System.Exit
import Foreign.Ptr (Ptr, FunPtr, castPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.C.Types (CInt,CUInt,CShort, CFloat,CDouble,CChar)
import Foreign.C
import qualified Data.Vector.Storable as SV (Storable, Vector, fromList, unsafeToForeignPtr)

-- a "wrapper" import is a converter for converting a Haskell 
-- function to a foreign function pointer
foreign import ccall "wrapper"
  syncWithCWrap :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "mt.h sendSignal"
  sendSignal :: CShort -> IO()


syncWithC :: MVar CInt -> MVar CInt -> CInt -> IO ()
syncWithC m1 m2 x = do
              putMVar m2 x
              takeMVar m1 -- wait for done signal from timerevent function
              return ()

timerevent :: [MVar CInt] -> [MVar CInt] -> Int -> IO()
timerevent m1 m2 t =  run where
    run = do
    -- pause for t microseconds
    threadDelay t
    print "Processing data"
    forM_ listOfThreads $ \x -> forkIO $ sendSignal x
    -- collect mvar from each C FFI thread
    -- all C threads have been paused by sendSignal above
    mvars <- forM m2 takeMVar
    -- signal each thread to continue
    forM_ m1 (\x -> putMVar x 0)
    print $ "Processed data"
    run
      where 
      listOfThreads = [0..fromIntegral $ (length m1) - 1]

foreign import ccall safe "test.h initThreads"
  initThreads :: CInt -> Ptr (FunPtr (IO())) -> IO()

getPtr :: (SV.Storable a) => SV.Vector a -> Ptr a
getPtr = unsafeForeignPtrToPtr . (\(x,_,_) -> x) . SV.unsafeToForeignPtr

main :: IO ()
main = do
  let nThreads = 12 
  m1 <- mapM (const newEmptyMVar) [1..nThreads] :: IO [MVar CInt]
  m2 <- mapM (const newEmptyMVar) [1..nThreads] :: IO [MVar CInt]
  fnptrs <- mapM (\(x,y) -> syncWithCWrap $ syncWithC x y 0) (zip m1 m2)
  let vfnptrs = SV.fromList fnptrs
  forkIO $ initThreads nThreads (getPtr vfnptrs)
  timerevent m1 m2 500000
  return ()

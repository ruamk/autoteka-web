module Main where

import           Control.Concurrent (threadDelay, forkIO)
import           Control.Monad (forever, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.IORef
import           Data.Aeson (ToJSON)
import qualified Data.Aeson.Text as Aeson
import           Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Servant.Client as Servant
import           System.Environment (lookupEnv)

import           Web.Scotty

import qualified Autoteka as Att


data AppContext = AppContext
  { frontDir :: FilePath
  , clientEnv :: Servant.ClientEnv
  , session :: IORef Att.AccessToken
  }

-- TODO: translate ClientError to Att-specific errors

main :: IO ()
main = do
  Just clientId <- fmap T.pack <$> lookupEnv "CLIENT_ID"
  Just clientSecret <- fmap T.pack <$> lookupEnv "CLIENT_SECRET"

  mgr <- newManager tlsManagerSettings
  let env = Att.mkClientEnv mgr
  let getToken = Servant.runClientM
        (Att.getToken clientId clientSecret)
        env

  getToken >>= \case
    Left err -> print err
    Right s -> do
      ss <- newIORef s
      void $ forkIO $ forever $ do -- refresh session once in a while
        threadDelay (20 * 60 * 1000000 :: Int) -- 20 min
        getToken >>= \case
          Left err -> print err
          Right s' -> print s' >> writeIORef ss s'
      scotty 3000 $ server $ AppContext "./dist/" env ss


server :: AppContext -> ScottyM ()
server AppContext{..} = do
  let runClient f = do
        res <- liftIO $ do
          ss <- readIORef session
          Servant.runClientM (f ss) clientEnv
        either (text . L.pack . show) json res

  get "/autoteka/packet_info"
    $ runClient Att.getActivePackage

  let search f = do
        q <- f <$> param "search"
        runClient $ \ss -> do
          pid <- Att.createPreview ss q
          loop (1 & seconds)
            (\p -> Att.p_status p == "processing")
            (Att.getPreview ss pid)

  get "/autoteka/reg/:search" $ search Att.RegNumber
  get "/autoteka/vin/:search" $ search Att.VIN

  get "/autoteka/report/:previewId" $ do
    pId <- Att.PreviewId . read <$> param "previewId"
    runClient $ \ss -> do
      rep <- Att.createReport ss pId
      loop (10 & seconds)
        (\r -> Att.r_status r == "processing")
        (Att.getReport ss $ Att.r_reportId rep)

  get "/"
    $ file $ frontDir <> "index.html"

  get "/:file"
    $ param "file" >>= file . (frontDir <>)


seconds :: Int -> Int
seconds n = n * 1000000


loop :: (MonadIO m, ToJSON r) => Int -> (r -> Bool) -> m r -> m r
loop usec cond f = go
  where
    go = do
      res <- f
      liftIO $  L.putStrLn $ Aeson.encodeToLazyText res
      if cond res
        then liftIO (threadDelay usec) >> go
        else return res

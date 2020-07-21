module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Aeson (ToJSON)
import qualified Data.Aeson.Text as Aeson
import           Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client (BaseUrl(..), Scheme(Https))
import qualified Servant.Client as Servant
import           System.Environment (lookupEnv)

import           Web.Scotty

import qualified Autoteka as Att


data AppContext = AppContext
  { frontDir :: FilePath
  , session :: Att.AccessToken
  , clientEnv :: Servant.ClientEnv
  }

-- TODO: Att.mkClientEnv :: Manager -> ClientEnv
-- TODO: translate ClientError to Att-specific errors

main :: IO ()
main = do
  Just clientId <- fmap T.pack <$> lookupEnv "CLIENT_ID"
  Just clientSecret <- fmap T.pack <$> lookupEnv "CLIENT_SECRET"

  mgr <- newManager tlsManagerSettings
  let env = Servant.mkClientEnv mgr (BaseUrl Https "api.avito.ru" 443 "")

  let runClient = flip Servant.runClientM env
  runClient (Att.getToken clientId clientSecret)
    >>= \case
      Left err -> print err
      Right session -> do
        scotty 3000 $ server $ AppContext "./dist/" session env


server :: AppContext -> ScottyM ()
server AppContext{..} = do
  let runClient f = liftIO (Servant.runClientM f clientEnv)
        >>= either (text . L.pack . show) json

  get "/autoteka/packet_info"
    $ runClient $ Att.getActivePackage session

  let search f = do
        q <- f <$> param "search"
        runClient $ do
          pid <- Att.createPreview session q
          loop (1 & seconds)
            (\p -> Att.p_status p == "processing")
            (Att.getPreview session pid)

  get "/autoteka/reg/:search" $ search Att.RegNumber
  get "/autoteka/vin/:search" $ search Att.VIN

  get "/autoteka/report/:previewId" $ do
    pId <- Att.PreviewId . read <$> param "previewId"
    runClient $ do
      rep <- Att.createReport session pId
      loop (10 & seconds)
        (\r -> Att.r_status r == "processing")
        (Att.getReport session $ Att.r_reportId rep)

  get "/"
    $ file $ frontDir <> "index.html"

  get "/:file"
    $ param "file" >>= file . (frontDir <>)


seconds :: Int -> Int
seconds n = n * 1000000


loop :: (MonadIO m, ToJSON r) => Int -> (r -> Bool) -> m r -> m r
loop usec cond f = go
  where
    go = f >>= \case
      res | cond res -> do
        liftIO $  L.putStrLn $ Aeson.encodeToLazyText res
        liftIO (threadDelay usec) >> go
      res -> return res

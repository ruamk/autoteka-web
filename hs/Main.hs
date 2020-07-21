module Main where

import qualified Data.Text as T
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
        scotty 3000 $ server $ AppContext "./dist" session env


server :: AppContext -> ScottyM ()
server AppContext{..} = do
  -- let runClient = flip Servant.runClientM clientEnv
  get "/autoteka/packet_info" $ html "HEllo"
  get "/autoteka/preview/:search" $ html "create preview, wait"
  post "/autoteka/report/:previewId" $ html "HEllo"
  get "/autoteka/report/:reportId" $ html "HEllo"

  get "/"
    $ file $ frontDir <> "index.html"
  get "/:file"
    $ param "file" >>= file . (frontDir <>)

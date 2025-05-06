module Web.Mercury.Client
  ( getAccounts
  , getAccount
  , getTransactions
  , getTransaction
  , requestSendMoney
  , runMercuryClient
  , MercuryEnv(..)
  , MercuryClient
  , module Web.Mercury.API
  ) where

import Web.Mercury.API hiding (getAccounts, getAccount, getTransaction, getTransactions, requestSendMoney)
import qualified Web.Mercury.API as API
import Control.Monad.Reader
import Servant.Client (ClientEnv, runClientM, ClientM, mkClientEnv, BaseUrl (BaseUrl), Scheme (Https))
import Servant.API (BasicAuthData (BasicAuthData))
import Control.Exception
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Data.ByteString (ByteString)
import Data.String (IsString(fromString))

newtype MercuryClient a = MC (ReaderT (BasicAuthData, ClientEnv) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (BasicAuthData, ClientEnv))

--------------------------------------------------------------------------------

getAccounts      :: MercuryClient Accounts
getAccount       :: Text -> MercuryClient Account
getTransactions  :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> MercuryClient Transactions
getTransaction   :: Text -> Text -> MercuryClient Transaction
requestSendMoney :: Text -> RequestSendMoney -> MercuryClient RequestSendMoneyResponse

getAccounts                   = performReq API.getAccounts
getAccount b                  = performReq (\a -> API.getAccount a b)
getTransactions b c d e f g h = performReq (\a -> API.getTransactions a b c d e f g h)
getTransaction b c            = performReq (\a -> API.getTransaction a b c)
requestSendMoney b c          = performReq (\a -> API.requestSendMoney a b c)

performReq :: (BasicAuthData -> ClientM a) -> MercuryClient a
performReq act = do
  (a, c) <- ask
  liftIO $ runClientM (act a) c >>= \case
    Left e -> throwIO e
    Right x -> return x

--------------------------------------------------------------------------------

data MercuryEnv = Sandbox | Production

runMercuryClient :: ByteString {-^ Secret Token -} -> MercuryEnv -> MercuryClient a -> IO a
runMercuryClient token tgt (MC r) = do
  manager <- newTlsManager
  let host = case tgt of Sandbox -> "api-sandbox.mercury.com"
                         Production -> "api.mercury.com"
  let env = mkClientEnv manager (BaseUrl Https host 443 "api/v1")
  runReaderT r (BasicAuthData token (fromString ""), env)


{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields #-}
module Web.Mercury.API where

import GHC.Generics
import Data.Proxy
import Data.Aeson
import Data.Text (Text)

import Servant.API hiding (Delete)
import qualified Servant.Client as C

type Mercury a = BasicAuth "Mercury API" () :> a

type GetAccounts = "accounts" :> Get '[JSON] Accounts
type GetAccount  = "account" :> Capture "id" Text :> Get '[JSON] Account
type GetAccountTransactions
      = "account"
      :> Capture "id" Text
      :> "transactions"
      :> QueryParam "limit" Int   -- Limit how many transactions you want to retrieve.
      :> QueryParam "offset" Int  -- The number of most recent transactions you want to omit.
      :> QueryParam "start" Text  -- Earliest createdAt date to filter for. If it's not provided, it defaults to 30 days ago. Format: YYYY-MM-DD or an ISO 8601 string
      :> QueryParam "end" Text    -- Latest createdAt date to filter for. If it's not provided, it defaults to current day. Format: YYYY-MM-DD or an ISO 8601 string.
      :> QueryParam "search" Text -- Search term to look for in transaction descriptions.
      :> QueryParam "order" Text  -- Defaults to "desc". Other option: "asc"
      :> Get '[JSON] Transactions
      
type GetAccountTransaction
      = "account"
      :> Capture "id" Text
      :> "transaction"
      :> Capture "transactionId" Text
      :> Get '[JSON] Transaction

type PostRequestSendMoney
      = "account"
      :> Capture "id" Text
      :> "request-send-money"
      :> ReqBody '[JSON] RequestSendMoney
      :> Post '[JSON] RequestSendMoneyResponse

--------------------------------------------------------------------------------

getAccounts      :: BasicAuthData -> C.ClientM Accounts
getAccount       :: BasicAuthData -> Text -> C.ClientM Account
getTransaction   :: BasicAuthData -> Text -> Text -> C.ClientM Transaction
getTransactions  :: BasicAuthData -> Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> C.ClientM Transactions
requestSendMoney :: BasicAuthData -> Text -> RequestSendMoney -> C.ClientM RequestSendMoneyResponse

getAccounts      = C.client (Proxy @(Mercury GetAccounts))
getAccount       = C.client (Proxy @(Mercury GetAccount))
getTransaction   = C.client (Proxy @(Mercury GetAccountTransaction))
getTransactions  = C.client (Proxy @(Mercury GetAccountTransactions))
requestSendMoney = C.client (Proxy @(Mercury PostRequestSendMoney))

--------------------------------------------------------------------------------

data Accounts = Accounts
  { accounts :: [Account]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Account = Account
  { accountNumber :: Text
  , availableBalance :: Double
  , id :: Text
  , kind :: Text
  , routingNumber :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Transactions = Transactions
  { total :: Int
  , transactions :: [Transaction]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Transaction = Transaction
  { amount :: Double
  , bankDescription :: Maybe Text
  , counterpartyId :: Text
  , counterpartyName :: Text
  , id :: Text
  , note :: Maybe Text
  , externalMemo :: Maybe Text
  , status :: Text
  , mercuryCategory :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data RequestSendMoney = RequestSendMoney
      { recipientId :: Text -- ^ Recipient ID from the /recipients endpoint.
      , amount :: Double -- ^ Amount of USD you want to send, must be a positive number.
      , paymentMethod :: Text -- ^ Payment method to use, currently only supports "ach"
      , note :: Maybe Text
      , externalMemo :: Maybe Text
      , idempotencyKey :: Text -- ^ Unique string identifying the transaction (WARNING: READ EXPLANATION)
      }
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

data RequestSendMoneyResponse = RequestSendMoneyResponse
  { accountId :: Text
  , requestId :: Text
  , recipientId :: Text
  , memo :: Maybe Text
  , paymentMethod :: Text
  , amount :: Double
  , status :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)


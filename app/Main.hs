{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

-- import qualified Data.ByteString as B

import Data.Aeson
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as L
import Data.ByteString.UTF8 as BSU
import Data.Text qualified as T
import GHC.Generics
import Network.HTTP.Client
-- import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Network.URI
import System.Process
import Prelude hiding (id)

apiEndpoint :: BC.ByteString
apiEndpoint = "www.googleapis.com"

createVpcPath :: BC.ByteString
createVpcPath = "/compute/v1/projects/fred-hsu-veos/global/networks"

createInstancePath :: BC.ByteString
createInstancePath = "/compute/v1/projects/fred-hsu-veos/zones/us-west1-a/instances"

createVpcRequest :: BC.ByteString -> Request
createVpcRequest token = buildRequest token apiEndpoint "POST" createVpcPath

createInstanceRequest :: BC.ByteString -> Request
createInstanceRequest token = buildRequest token apiEndpoint "POST" createInstancePath

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method $
    setRequestHost host $
      setRequestBearerAuth token $
        setRequestPath path $
          setRequestSecure True $
            setRequestPort 443 $
              defaultRequest

gcloudToken :: IO String
gcloudToken = readProcess "gcloud" ["auth", "application-default", "print-access-token"] []

-- getToken = BSU.fromString (myStrip gcloudToken)

myStrip :: [a] -> [a]
myStrip a = reverse $ tail $ reverse a

data VpcCreate = VpcCreate
  { autoCreateSubnetworks :: Bool,
    mtu :: Integer,
    name :: String,
    routingConfig :: RoutingConfig
  }
  deriving (Show, Generic)

data RoutingConfig = RoutingConfig
  { routingMode :: T.Text
  }
  deriving (Show, Generic)

instance ToJSON RoutingConfig

instance ToJSON VpcCreate

data ZonalOperation = ZonalOperation
  { kind :: String,
    id :: String,
    name :: String,
    zone :: String,
    clientOperationId :: Maybe String,
    operationType :: String,
    targetLink :: String,
    targetId :: String,
    status :: String,
    user :: String,
    progress :: Integer,
    insertTime :: String,
    startTime :: String,
    selfLink :: String
  }
  deriving (Show, Generic)

instance FromJSON ZonalOperation

data GlobalOperation = GlobalOperation
  { kind :: String,
    id :: String,
    operationType :: String,
    targetLink :: String,
    status :: String,
    user :: String,
    progress :: Integer,
    insertTime :: String,
    startTime :: String,
    selfLink :: String
  }
  deriving (Show, Generic)

instance FromJSON GlobalOperation

data Instance = Instance
  { machineType :: T.Text,
    name :: T.Text,
    disks :: [Disk],
    networkInterfaces :: [NetworkInterface]
  }
  deriving (Show, Generic)

instance ToJSON Instance

data Disk = Disk
  { autoDelete :: Bool,
    boot :: Bool,
    deviceName :: T.Text,
    initializeParams :: InitializeParam,
    mode :: T.Text,
    diskType :: String
  }
  deriving (Show, Generic)

instance ToJSON Disk where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = renameTypeField}

renameTypeField :: String -> String
renameTypeField "diskType" = "type"
renameTypeField s = s

createDisk :: T.Text -> Disk
createDisk diskName =
  Disk
    { autoDelete = True,
      boot = True,
      deviceName = diskName,
      initializeParams = iParams,
      mode = "READ_WRITE",
      diskType = "PERSISTENT"
    }
  where
    iParams =
      InitializeParam
        { diskSizeGb = "10",
          diskType = "projects/fred-hsu-veos/zones/us-central1-a/diskTypes/pd-balanced",
          sourceImage = "projects/debian-cloud/global/images/debian-11-bullseye-v20221206"
        }

data InitializeParam = InitializeParam
  { diskSizeGb :: String,
    diskType :: String,
    sourceImage :: String
  }
  deriving (Show, Generic)

instance ToJSON InitializeParam

data NetworkInterface = NetworkInterface
  { accessConfigs :: [AccessConfig],
    stackType :: String,
    subnetwork :: T.Text
  }
  deriving (Show, Generic)

instance ToJSON NetworkInterface

createNetworkInterface :: T.Text -> NetworkInterface
createNetworkInterface sn =
  NetworkInterface
    { accessConfigs = ac,
      stackType = "IPV4_ONLY",
      subnetwork = subnet
    }
  where
    ac =
      [ AccessConfig
          { name = "External NAT",
            networkTier = "PREMIUM"
          }
      ]
    subnet = T.append "projects/fred-hsu-veos/regions/us-west1/subnetworks/" sn

data AccessConfig = AccessConfig
  { name :: String,
    networkTier :: String
  }
  deriving (Show, Generic)

instance ToJSON AccessConfig

createVpc :: String -> VpcCreate
createVpc name =
  VpcCreate
    { autoCreateSubnetworks = True,
      mtu = 1460,
      name = name,
      routingConfig = RoutingConfig {routingMode = "REGIONAL"}
    }

createInstance :: T.Text -> T.Text -> T.Text -> Instance
createInstance name subnet zone =
  Instance
    { machineType = ("projects/fred-hsu-veos/zones/" :: T.Text) <> zone <> ("/machineTypes/e2-micro" :: T.Text),
      name = name,
      disks = [createDisk name],
      networkInterfaces = [createNetworkInterface subnet]
    }

requestVpc :: String -> IO (Response GlobalOperation)
requestVpc name = do
  let vpc = createVpc name
  -- Get token
  token <- gcloudToken
  let token' = BSU.fromString (myStrip token)
  -- Send request
  let vpcRequest = setRequestBodyJSON vpc $ createVpcRequest token'
  httpJSON vpcRequest :: IO (Response GlobalOperation)

requestInstance :: T.Text -> T.Text -> IO (Response ZonalOperation)
requestInstance name vpcName = do
  let i = createInstance name vpcName "us-west1-a"
  token <- gcloudToken
  let token' = BSU.fromString (myStrip token)
  let instanceRequest = setRequestBodyJSON i $ createInstanceRequest token'
  httpJSON instanceRequest :: IO (Response ZonalOperation)

waitUrl :: String -> BC.ByteString
waitUrl operationPath = BSU.fromString $ operationPath ++ "/wait"

-- TODO make this work for wait request endpoint = compute.googleapis.com path = selfLink operation
createWaitRequest :: String -> BC.ByteString -> Request
createWaitRequest operationPath token = buildRequest token apiEndpoint "POST" (waitUrl operationPath)

waitOnOperation :: GlobalOperation -> IO (Response GlobalOperation)
waitOnOperation operationResponse = do
  let operationPath = maybe "no uri" uriPath $ parseURI (selfLink (operationResponse :: GlobalOperation))

  print operationPath
  token <- gcloudToken
  let token' = BSU.fromString (myStrip token)
  let waitRequest = createWaitRequest operationPath token'
  let req = waitRequest {responseTimeout = responseTimeoutNone}
  httpJSON req :: IO (Response GlobalOperation)

main :: IO ()
main = do
  let vpcName = "fh-test-vpc"
  response <- requestVpc vpcName
  let rb = getResponseBody response
  putStrLn "Waiting on VPC to finish"
  vpcWaitOp <- waitOnOperation rb
  print vpcWaitOp
  putStrLn "VPC created"
  print $ show rb
  putStrLn $ targetLink (rb :: GlobalOperation)

  instanceResponse <- requestInstance "fh-test-vm" (T.pack vpcName)
  let zonalOp = getResponseBody instanceResponse
  putStrLn "Waiting on VM to finish"

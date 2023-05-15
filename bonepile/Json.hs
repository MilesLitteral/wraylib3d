module Json where 

import GHC.Generics(Generic)
import Control.Lens    (makePrisms)
import Control.Lens.TH (makePrisms)
import Data.Default
import qualified Data.Aeson   as JSON

data ProjectSpec = ProjectSpec {
    producer_config :: [ProducerDefinitionJS],
    dataset_config  :: DatasetConfigJS,
    view_config     :: ViewConfigJS -- [ViewStateJS] -- soon
} deriving(Generic, Default, Show, Eq)

newtype ProducerConfigJS = ProducerConfigJS {
    producerObject :: [ProducerDefinitionJS]
} deriving(Generic, Default, Show, Eq)

data ProducerDefinitionJS = ProducerDefinitionJS {
    producer_name   :: String,
    producer_type   :: String,
    producer_path   :: [FilePath]
} deriving(Generic, Default, Show, Eq)

data DatasetConfigJS = DatasetConfigJS {
    ground_truths     :: GroundTruthsJS,
    sequence_content :: FilePath,
    train_content    :: FilePath,
    test_content     :: FilePath
} deriving(Generic, Default, Show, Eq)

data GroundTruthsJS = GroundTruthsJS {
    recon_spec_path :: FilePath,
    train_sv      :: FilePath,
    scene_sv      :: FilePath,
    pred_sv       :: FilePath
} deriving(Generic, Default, Show, Eq)

data ViewConfigJS = ViewConfigJS {
    main_section   :: [WidgetJS],
    graph_section  :: [WidgetJS]
} deriving(Generic, Default, Show, Eq)

newtype ViewStateJS = ViewStateJS {
    viewStates :: [ViewConfigJS]
} deriving(Generic, Default, Show, Eq)

data WidgetJS = WidgetJS {
    wdgt_key           :: String,
    wdgt_type          :: String,
    wdgt_viewingOption :: String, -- Show ImageSrc
    wdgt_src           :: [String]
} deriving(Generic, Default, Show, Eq)

data ReconSpec = 
    ReconSpecification {
    rspec :: [ReconSpecSlice]
} deriving(Eq, Show)

data ReconSpecSlice = ReconSpecSlice {
    recon_property_kind       :: String, 
    recon_property_span       :: Int, 
    recon_property_range      :: [[Float]], 
    recon_learnable           :: [Bool],
    recon_property_descriptor :: String
} deriving(Eq, Show)

instance JSON.ToJSON ProjectSpec 
instance JSON.ToJSON ProducerConfigJS 
instance JSON.ToJSON ProducerDefinitionJS 
instance JSON.ToJSON ViewStateJS 
instance JSON.ToJSON DatasetConfigJS
instance JSON.ToJSON GroundTruthsJS
instance JSON.ToJSON ViewConfigJS
instance JSON.ToJSON WidgetJS

instance JSON.FromJSON ProjectSpec 
instance JSON.FromJSON ProducerConfigJS 
instance JSON.FromJSON ProducerDefinitionJS 
instance JSON.FromJSON ViewStateJS 
instance JSON.FromJSON DatasetConfigJS
instance JSON.FromJSON GroundTruthsJS
instance JSON.FromJSON ViewConfigJS
instance JSON.FromJSON WidgetJS

instance Default ReconSpec where 
    def = ReconSpecification []
  
-- | IntakeSlice instance for FromJSON typeclass
instance JSON.FromJSON ReconSpec where
    parseJSON v = ReconSpecification <$> JSON.parseJSON v

-- | IntakeSlice instance for FromJSON typeclass
instance JSON.FromJSON ReconSpecSlice where
    parseJSON (JSON.Object v) = ReconSpecSlice
                        <$> v JSON..: "property_kind"
                        <*> v JSON..: "property_span"
                        <*> v JSON..: "property_range"
                        <*> v JSON..: "learnable"
                        <*> v JSON..: "descriptor"


loadLabelsFromReconSpec :: String -> IO (Maybe ReconSpec)
loadLabelsFromReconSpec path = do
  js <- JSON.eitherDecodeFileStrict (path)
  case js of 
    Left  e -> error e
    Right j -> return (Just j)

makePrisms ''ReconSpecSlice
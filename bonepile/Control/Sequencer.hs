module Manifest.Control.Sequencer where

import Data.Aeson

data EventType 
    = SET_LOW -- UNTRIGGER
    |SET_HIGH -- TRIGGER
    deriving (Show, Eq, Enum)

data Sequence =
    Sequence {
        stepDuration :: Float,
        seqLen       :: Int --seqSlices    :: [SequenceSlice]
} deriving (Show, Eq)

data SequenceEvent = 
    SequenceEvent {
        groupIndex     :: Int,
        deviceIndex    :: Int,
        state          :: EventType
    } deriving (Show, Eq)

-- Represents a vertical slice of one step in a sequence
-- There is one BankState per DeviceBank
-- data SequenceSlice =
--     SequenceSlice {
--     bankStates :: [BankState]  
-- } deriving (Show, Eq)

-- class IsSequence a where 
--     executeSequence :: a -> Bool;

-- instance ToJSON SequenceEvent where
-- toJSON s = object
--     [  "sequence_index"  .=    sequenceIndex s,
--         "bank_index"     .=    bankIndex     s,
--         "device_index"   .=    deviceIndex   s,
--         "event"          .=    event s
--     ]

-- instance FromJSON SequenceEvent where
--     parseJSON (Object v) = SequenceEvent
--                             <$> v .: "sequence_index"
--                             <*> v .: "bank_index"
--                             <*> v .: "device_index"
--                             <*> v .: "event"


-- eventsToSlices :: [SequenceEvent] -> [DeviceBank] -> Int -> [SequenceSlice] 
-- eventsToSlices events banks numSteps = do
--     undefinied

-- executeSequence :: SRBoard -> Bool
-- executeSequence sr = undefined

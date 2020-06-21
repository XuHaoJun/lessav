{-# LANGUAGE DeriveGeneric #-}

module Av where

import GHC.Generics (Generic)
import Data.Aeson

data Av = Av { 
    avId :: String,
    fullAvId :: String,
    jpDisplay :: String,
    url :: String,
    filePath :: FilePath,
    actors :: [String],
    tags :: [String] 
} deriving (Generic, Show)


instance ToJSON Av where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Av

setAvId :: String -> Av -> Av
setAvId avId av = av { avId = avId }

setFilePath :: FilePath -> Av -> Av
setFilePath filePath av = av { filePath = filePath }

getFilePath :: Av -> FilePath
getFilePath (Av _ _ _ _ filePath _ _) = filePath

getAvId :: Av -> String
getAvId (Av avId _ _ _ _ _ _) = avId

getFullAvId :: Av -> String
getFullAvId (Av _ fullAvId _ _ _ _ _) = fullAvId

getUrl :: Av -> String
getUrl (Av _ _ _ url _ _ _) = url

getActors :: Av -> [String]
getActors (Av _ _ _ _ _ actors _) = actors

getTags :: Av -> [String]
getTags (Av _ _ _ _ _ _ tags) = tags

getJpDisplay :: Av -> String
getJpDisplay (Av _ _ jpDisplay _ _ _ _) = jpDisplay

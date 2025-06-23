{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Nux.User where

import RIO
import RIO.File
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L

data User = User
  { userUid :: Maybe Int
  , userGid :: Maybe Int
  , userDescription :: String
  , userEmail :: String
  , userAutos :: [String]
  , userModules :: [String]
  , userServices :: [String]
  , userPrograms :: [String]
  , userPackages :: [String]
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .:? "uid"
    <*> v .:? "gid"
    <*> v .: "description"
    <*> v .: "email"
    <*> v .: "autos"
    <*> v .: "modules"
    <*> v .: "services"
    <*> v .: "programs"
    <*> v .: "packages"

instance ToJSON User where
  toJSON (User uid gid desc email autos mods svcs progs pkgs) = object
    [ "uid"         .= uid
    , "gid"         .= gid
    , "description" .= desc
    , "email"       .= email
    , "autos"       .= autos
    , "modules"     .= mods
    , "services"    .= svcs
    , "programs"    .= progs
    , "packages"    .= pkgs
    ]

type Users = Map String User

writeUser :: FilePath -> User -> RIO env ()
writeUser path user = do
  writeBinaryFile path $ BL.toStrict $ encodePretty user

readUser :: FilePath -> RIO env User
readUser path = do
  content <- readFileUtf8 path
  case eitherDecodeStrictText content of
    Left err -> throwString $ "Failed to parse user file: " <> err
    Right user -> return user

emptyUser :: User
emptyUser = User
  { userUid = Nothing
  , userGid = Nothing
  , userDescription = ""
  , userEmail = ""
  , userAutos = []
  , userModules = []
  , userServices = []
  , userPrograms = []
  , userPackages = []
  }

newUser :: String -> String -> User
newUser desc email = emptyUser
  { userDescription = desc
  , userEmail = email
  }

nuxUser :: User
nuxUser = newUser "Nux User" "nux@localhost"

addUserAuto :: String -> User -> User
addUserAuto name user =
    user { userAutos = name : userAutos user }

delUserAuto :: String -> User -> User
delUserAuto name user =
  user { userAutos = L.delete name (userAutos user) }
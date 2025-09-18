{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Nux.User where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Nux.Process
import           Nux.Util
import           RIO
import qualified RIO.ByteString.Lazy          as BL
import           RIO.Directory
import           RIO.File
import           RIO.FilePath
import qualified RIO.List                     as L
import           RIO.Process                  (HasProcessContext)
import qualified RIO.Text                     as T
import           System.Posix.User.ByteString

data User = User
  { userUid         :: Int
  , userGid         :: Int
  , userDescription :: String
  , userEmail       :: String
  , userAutos       :: [String]
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> User
    <$> v .:? "uid"         .!= 0
    <*> v .:? "gid"         .!= 0
    <*> v .:? "description" .!= ""
    <*> v .:? "email"       .!= ""
    <*> v .:? "autos"       .!= []

instance ToJSON User where
  toJSON (User uid gid desc email autos) = object $
       ["uid"         .= uid   | uid   /= 0]
    ++ ["gid"         .= gid   | gid   /= 0]
    ++ ["description" .= desc  | desc  /= ""]
    ++ ["email"       .= email | email /= ""]
    ++ ["autos"       .= autos | autos /= []]


type Users = Map String User

usersDirPath :: FilePath -> FilePath
usersDirPath flake = flake </> "nix/users"

userDirPath :: FilePath -> String -> FilePath
userDirPath flake user = usersDirPath flake </> user

userNixFilePath :: FilePath -> String -> FilePath
userNixFilePath flake user = userDirPath flake user </> "default.nix"

userFilePath :: FilePath -> String -> FilePath
userFilePath flake user = userDirPath flake user </> "user.json"

doesUserExist :: FilePath -> String -> RIO env Bool
doesUserExist flake username = doesFileExist $ userFilePath flake username

writeUser
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> User -> RIO env ()
writeUser path user = do
  writeFileAtomicOwner path $ BL.toStrict $ encodePretty user

readUser :: FilePath -> RIO env User
readUser path = do
  content <- readFileUtf8 path
  case eitherDecodeStrictText content of
    Left err   -> throwString $ "Failed to parse user file: " <> err
    Right user -> return user

readFlakeUser :: FilePath -> String -> RIO env User
readFlakeUser flake username = readUser $ userFilePath flake username

writeFlakeUser
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> User -> RIO env ()
writeFlakeUser flake username = writeUser (userFilePath flake username)

writeFlakeUserNix :: FilePath -> String -> RIO env ()
writeFlakeUserNix flake username = do
  let nixFile = userNixFilePath flake username
  writeBinaryFile nixFile $ fromString $ L.unlines
    [ "with builtins; fromJSON (readFile ./user.json)"
    ]

addFlakeUser
  :: (HasProcessContext env, HasLogFunc env)
  => FilePath -> String -> User -> RIO env ()
addFlakeUser flake username user = do
  let userDir = userDirPath flake username
  createDirectoryIfMissing True userDir
  writeFlakeUserNix flake username
  writeFlakeUser flake username user

emptyUser :: User
emptyUser = User
  { userUid = 0
  , userGid = 0
  , userDescription = ""
  , userEmail = ""
  , userAutos = []
  }

newUser :: String -> String -> User
newUser desc email = emptyUser
  { userDescription = desc
  , userEmail = email
  }

nuxUser :: User
nuxUser = newUser "Nux User" "nux@localhost"

data UserInfo = UserInfo
  { userInfoUid         :: Int
  , userInfoGid         :: Int
  , userInfoDescription :: String
  , userInfoEmail       :: String
  } deriving (Show)

defaultUserInfo :: UserInfo
defaultUserInfo =
  UserInfo 1000 1000 "" ""

fromUserEntry :: UserEntry -> UserInfo
fromUserEntry UserEntry{..} =
  UserInfo uid gid desc email
  where
    uid = fromIntegral userID
    gid = fromIntegral userGroupID
    desc = decodeUtf8' userGecos & fromRight "" & T.unpack
    email = ""

getUserEmail
  :: (HasProcessContext env, HasLogFunc env)
  => String -> RIO env String
getUserEmail name = do
  user <- liftIO getEffectiveUserName
  if user == fromString name
    then tryAny gitConfigGetEmail >>= \case
      Left _ -> return ""
      Right s  -> return $ trim s
    else return ""

getUserEntry :: String -> RIO env UserEntry
getUserEntry = liftIO . getUserEntryForName . fromString

getUserId :: String -> RIO env Int
getUserId = fmap (fromIntegral . userID) . getUserEntry

getUserInfo
  :: (HasProcessContext env, HasLogFunc env)
  => String -> RIO env UserInfo
getUserInfo username = do
  email <- getUserEmail username
  ui <- tryAny (getUserEntry username) >>= \case
    Left _ -> return defaultUserInfo
    Right u -> return $ fromUserEntry u
  return ui { userInfoEmail = email }

initUser
  :: (HasProcessContext env, HasLogFunc env)
  => String
  -> String
  -> String
  -> Int
  -> Int
  -> [String]
  -> RIO env User
initUser username desc email uid gid autos = do
  UserInfo{..} <- getUserInfo username
  return emptyUser { userDescription = desc  `nullOr` userInfoDescription
                   , userEmail       = email `nullOr` userInfoEmail
                   , userUid         = uid   `zeroOr` userInfoUid
                   , userGid         = gid   `zeroOr` userInfoGid
                   , userAutos       = autos
                   }

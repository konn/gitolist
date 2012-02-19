{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module DataTypes where
import Data.Data
import Control.Exception
import System.Git
import Data.ByteString.Char8 (ByteString)
import Text.Parsec
import System.FilePath

data Repository = Repository { repoName        :: RepoName
                             , repoPermissions :: [(Permission, [UserName])]
                             }
                  deriving (Show, Eq, Ord, Typeable, Data)

repoDir :: Gitolite -> Repository -> GitDir
repoDir git repo = gitolitePath git </> repoName repo ++ ".git"

permissionForUser :: UserName -> Repository -> [Permission]
permissionForUser uName repo =
    map fst $ filter ((uName `elem`).snd) $ repoPermissions repo

type Refex = String

data Permission = C      {refex :: [Refex]}
                | R      {refex :: [Refex]}
                | RW     {refex :: [Refex]}
                | RWPlus {refex :: [Refex]}
                | Minus  {refex :: [Refex]}
                  deriving (Show, Eq, Ord, Data, Typeable)

data User = User { userName     :: String
                 , userPubKey   :: ByteString
                 } deriving (Eq, Ord, Typeable, Data, Show)

data Gitolite = Gitolite { gitolitePath :: FilePath
                         , admin :: Repository
                         , repositories :: [Repository]
                         , users :: [User]
                         } deriving (Show, Eq, Ord, Data, Typeable)

data GitoliteException = GitError GitError
                       | FileError String
                       | ParseError ParseError
                       | ConfError String
                         deriving (Show, Typeable)

instance Exception GitoliteException

type RepoName = String
type UserName = String

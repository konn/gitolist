{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module DataTypes where
import Data.Data
import Control.Exception
import System.Git
import Data.ByteString.Char8 (ByteString)
import Text.Parsec
import System.FilePath
import Prelude
import Data.Monoid

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

data Permission = Permission { _writable  :: Bool
                             , _plus      :: Bool
                             , _creatable :: Bool
                             , _deletable :: Bool
                             , refexes   :: [Refex]
                             }
                | Deny { refexes :: [Refex] }
                  deriving (Show, Eq, Ord, Data, Typeable)

readable, writable, plus, creatable, deletable, denied :: Permission -> Bool
readable  Deny {} = False
readable  _       = True
writable  Deny {} = False
writable  perm    = _writable perm
plus      Deny {} = False
plus      perm    = _plus perm
creatable Deny {} = False
creatable perm    = _creatable perm
deletable Deny {} = False
deletable perm    = _deletable perm
denied    Deny {} = True
denied    _       = False

isReadableFor, isWritableFor, isDeletableFor, isCreatableFor :: Repository -> UserName -> Bool
repo `isReadableFor` uName =
    let ps = permissionForUser uName repo
    in not (null ps) && all readable ps
repo `isWritableFor` uName =
    let ps = permissionForUser uName repo
    in all readable ps && any writable ps
repo `isDeletableFor` uName =
    let ps     = permissionForUser uName repo
        allPs  = map fst $ repoPermissions repo
        anyDel = any deletable allPs
    in if anyDel then any deletable ps else any deletable ps || any plus ps
repo `isCreatableFor` uName =
    let ps     = permissionForUser uName repo
        allPs  = map fst $ repoPermissions repo
        anyCre = any creatable allPs
    in if anyCre then any creatable ps else any creatable ps || any plus ps

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

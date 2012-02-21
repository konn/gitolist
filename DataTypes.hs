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
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Zip as Zip
import Data.Git
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative

data Repository = Repository { repoName        :: RepoName
                             , repoPermissions :: [(Permission, [UserName])]
                             }
                  deriving (Show, Eq, Ord, Typeable, Data)

repoDir :: Gitolite -> Repository -> GitDir
repoDir git repo = gitolitePath git </> repoName repo ++ ".git"

permissionForUser :: UserName -> Repository -> [Permission]
permissionForUser uName repo =
    map fst $ filter ((uName `elem`).snd) $ repoPermissions repo

type Commit = GitPath

gitPathToTarEntry :: Gitolite -> Repository -> GitPath -> IO [Tar.Entry]
gitPathToTarEntry git repo path = runner path
  where
    root = case splitDirectories path of
             []    -> "HEAD"
             "/":_ -> "HEAD"
             s:_   -> s
    dir  = repoDir git repo
    base = path
    rootPath = concat [repoName repo, "-", root]
    runner p = do
      obj <- gitPathToObj p dir
      case obj of
        GoBlob size src -> do
          let Right tPath = Tar.toTarPath False (rootPath </> makeRelative base p)
          return [Tar.fileEntry tPath $ LBS.fromChunks [src]]
        GoTree _ es -> do
          let Right tPath = Tar.toTarPath True (rootPath </> makeRelative base p)
          (Tar.directoryEntry tPath : ) . concat <$>
            mapM (runner . (p </>) . fileName) es

tarGitPath :: Gitolite -> Repository -> GitPath -> IO LBS.ByteString
tarGitPath git repo path = Tar.write <$> gitPathToTarEntry git repo path

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

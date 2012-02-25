{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, DoAndIfThenElse #-}
module GitUtils where
import Data.Data
import Control.Exception
import System.Git
import Data.ByteString.Char8 (ByteString)
import Text.Parsec (ParseError)
import System.FilePath
import Prelude
import Data.Either
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Zip as Zip
import Data.Git hiding (GitTag(..), GitCommit(..))
import qualified Data.Git as Git
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.Monoid
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad
import Data.Time
import Data.Maybe
import System.Locale

data Repository = Repository { repoName        :: RepoName
                             , repoPermissions :: [(Permission, [UserName])]
                             }
                  deriving (Show, Eq, Ord, Typeable, Data)

repoDir :: Gitolite -> Repository -> GitDir
repoDir git repo = gitolitePath git </> repoName repo ++ ".git"

permissionForUser :: UserName -> Repository -> [Permission]
permissionForUser uName repo =
    map fst $ filter ((uName `elem`).snd) $ repoPermissions repo

listChildren :: Bool            -- List directory or not
             -> FilePath        -- Root path
             -> IO [FilePath]   -- Paths of children
listChildren lDir fp = do
  isDir <- doesDirectoryExist fp
  if isDir
  then do
    cs <- filter (`notElem` [".", ".."]) <$> getDirectoryContents fp
    if lDir
    then (fp :) . concat <$> mapM (listChildren lDir . (fp </>)) cs
    else concat <$> mapM (listChildren lDir . (fp </>)) cs
  else do
    isFile <- doesFileExist fp
    if isFile
    then return [fp]
    else return []

sourceChildren :: ResourceIO m => Bool -> FilePath -> Source m FilePath
sourceChildren lDir fp = Source { sourcePull = pull, sourceClose = close }
  where
    close = return ()
    pull  = do
      isDir <- liftIO $ doesDirectoryExist fp
      if isDir
      then do
        cs <- map (fp </>) . filter (`notElem` [".", ".."]) <$>
                liftIO (getDirectoryContents fp)
        let src = mconcat $ map (sourceChildren lDir) cs
        if lDir then return $ Open src fp else sourcePull src
      else do
        isFile <- liftIO $ doesFileExist fp
        if isFile then return $ Open mempty fp else return Closed

data Branch = Branch { branchName :: String
                     , branchRef  :: SHA1
                     , branchHEAD :: Commit
                     } deriving (Show, Eq, Typeable)

try' :: IO a -> IO (Maybe a)
try' act = either (const Nothing) Just <$> (try :: IO a -> IO (Either SomeException a)) act

repoBranches :: Gitolite -> Repository -> IO [Branch]
repoBranches git repo = do
  gitBranches $ repoDir git repo

data Tag = Tag { tagName    :: String
               , tagger     :: String
               , tagDate    :: UTCTime
               , tagMessage :: String
               , tagRef     :: SHA1
               }
           deriving (Show, Eq, Typeable)

data Commit = Commit { commitRef        :: SHA1
                     , commitParent     :: Maybe SHA1
                     , commitAuthor     :: String
                     , commitAuthorDate :: UTCTime
                     , committer        :: String
                     , commitDate       :: UTCTime
                     , commitLog        :: String
                     } deriving (Show, Eq, Typeable)

repoTags :: Gitolite -> Repository -> IO [Tag]
repoTags git repo = do
  gitTags $ repoDir git repo

repoCommitsForBranch :: Gitolite -> Repository -> Branch -> IO [Commit]
repoCommitsForBranch git repo branch = do
  gitCommitsForBranch branch (repoDir git repo)

gitCommitsForBranch :: Branch -> GitDir -> IO [Commit]
gitCommitsForBranch branch dir = do
  runner $ branchRef branch
  where
    runner sha1 = do
      GoCommit _ gc <- sha1ToObj sha1 dir
      let c = gitComToCommit gc
      case commitParent c of
        Just paren -> maybe [c] (c:) <$> try' (runner paren)
        Nothing    -> return [c]

-- gitGetUpdatedFiles :: GitCommit -> GitPath -> IO (GoBlob

gitComToCommit :: Git.GitCommit -> Commit
gitComToCommit (Git.GitCommit ref ps ath cmtr lg) =
  let (author, authorDate)    = naiveSplitDate $ T.unpack $ T.decodeUtf8 ath
      (committer, commitDate) = naiveSplitDate $ T.unpack $ T.decodeUtf8 cmtr
  in Commit { commitRef        = ref
            , commitParent     = listToMaybe ps
            , commitAuthor     = author
            , commitAuthorDate = authorDate
            , committer        = committer
            , commitDate       = commitDate
            , commitLog        = T.unpack $ T.decodeUtf8 lg
            }

gitBranch :: String -> GitDir -> IO Branch
gitBranch bName dir = do
  sha1 <- head . lines <$> readFile (dir </> "refs" </> "heads" </> bName)
  GoCommit _ comm <- sha1ToObj (SHA1 sha1) dir
  return $ Branch bName (SHA1 sha1) $ gitComToCommit comm
  
gitBranches :: GitDir -> IO [Branch]
gitBranches dir = do
  let branchPath = dir </> "refs" </> "heads"
  bs <- listChildren False branchPath
  liftM catMaybes . forM bs $ \fpath -> try' $ do
    sha1 <- SHA1 . head . lines <$> readFile fpath
    GoCommit _ comm <- sha1ToObj sha1 dir
    return $ Branch (makeRelative branchPath fpath) sha1 (gitComToCommit comm)

gitTags :: GitDir -> IO [Tag]
gitTags dir = do
  let tagsPath = dir </> "refs" </> "tags"
  ts <- listChildren False tagsPath
  liftM catMaybes . forM ts $ \fpath -> try' $ do
    sha1 <- SHA1 . head . lines <$> readFile fpath
    objs <- sha1ToObj sha1 dir
    case objs of
      GoCommit _ c -> do
        let comm = gitComToCommit c
        return $ Tag { tagRef     = commitRef comm
                     , tagName    = makeRelative tagsPath fpath
                     , tagger     = committer comm
                     , tagMessage = commitLog comm
                     , tagDate    = commitDate comm
                     }
      GoTag _ (Git.GitTag ref typ name tgr lg) -> do
        let (tagger, tagDate) = naiveSplitDate $ T.unpack $ T.decodeUtf8 tgr
        return $ Tag { tagRef     = ref
                     , tagName    = T.unpack $ T.decodeUtf8 name
                     , tagger     = tagger
                     , tagDate    = tagDate
                     , tagMessage = T.unpack $ T.decodeUtf8 lg
                     }
      _ -> throwIO $ ObjectError "Object mismatch"

naiveSplitDate :: String -> (String, UTCTime)
naiveSplitDate src =
  let wds  = reverse $ words src
      date = unwords $ reverse $ take 2 wds
      tagger = unwords $ reverse $ drop 2 wds
  in (tagger, readTime defaultTimeLocale "%s %z" date)

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
        _ -> return []

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
                       | ObjectError String
                         deriving (Show, Typeable)

instance Exception GitoliteException

type RepoName = String
type UserName = String

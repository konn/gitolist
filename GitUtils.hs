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
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Data.Git hiding (GitTag(..), GitCommit(..))
import qualified Data.Git as Git
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad
import Data.Time
import Data.Maybe
import System.Locale
import Data.Char
import Text.Pandoc
import Data.List (find)
import Encodings

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

getDescription :: Gitolite -> Repository -> IO (Maybe String)
getDescription git repo = do
  let desc = repoDir git repo </> "description"
  ext <- doesFileExist desc
  if ext
    then Just <$> readFile desc
    else do
      obj <- gitPathToObj "/" $ repoDir git repo
      case obj of
        GoTree _ es -> do
          case find ((=="README") . map toUpper . dropExtension . fileName) es of
            Just (GitTreeEntry (RegularFile _) fname ref) -> do
              blob <- sha1ToObj ref $ repoDir git repo
              case fromBlob blob of
                Just src -> do
                  let ext         = takeExtension fname
                      Pandoc _ bs = fromMaybe (readMarkdown defaultParserState) (lookup ext pandocDic) src
                  return $ listToMaybe $ mapMaybe p bs
                _ -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
  where
    p h@(Header 1 _) = Just $ writePlain defaultWriterOptions $ Pandoc (Meta [] [] []) [h]
    p _              = Nothing
    pandocDic :: [(String, String -> Pandoc)]
    pandocDic = [(".xhtml", readHtml defaultParserState)
                ,(".html", readHtml defaultParserState)
                ,(".htm", readHtml defaultParserState)
                ,(".rst", readRST defaultParserState)
                ,(".textile", readTextile defaultParserState)
                ,(".md", readMarkdown defaultParserState)
                ,(".markdown", readMarkdown defaultParserState)
                ]


fromBlob :: GitObject -> Maybe String
fromBlob (GoBlob _ bs) = (flip decode bs =<< detectEncoding bs)
                     <|> either (const Nothing) Just (T.unpack <$> T.decodeUtf8' bs)
fromBlob _             = Nothing


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
      (ctr, cd) = naiveSplitDate $ T.unpack $ T.decodeUtf8 cmtr
  in Commit { commitRef        = ref
            , commitParent     = listToMaybe ps
            , commitAuthor     = author
            , commitAuthorDate = authorDate
            , committer        = ctr
            , commitDate       = cd
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
      GoTag _ (Git.GitTag ref _ name tgr lg) -> do
        let (tr, td) = naiveSplitDate $ T.unpack $ T.decodeUtf8 tgr
        return $ Tag { tagRef     = ref
                     , tagName    = T.unpack $ T.decodeUtf8 name
                     , tagger     = tr
                     , tagDate    = td
                     , tagMessage = T.unpack $ T.decodeUtf8 lg
                     }
      _ -> throwIO $ ObjectError "Object mismatch"

naiveSplitDate :: String -> (String, UTCTime)
naiveSplitDate src =
  let wds  = reverse $ words src
      date = unwords $ reverse $ take 2 wds
      tgr = unwords $ reverse $ drop 2 wds
  in (tgr, readTime defaultTimeLocale "%s %z" date)

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
        GoBlob _ src -> do
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

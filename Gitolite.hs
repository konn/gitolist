{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Gitolite where
import Data.Git
import System.Git
import Data.Data
import System.FilePath
import Data.ByteString.Char8 hiding (tail, filter)
import Control.Exception
import Data.Attoparsec.Char8
import Control.Applicative
import Prelude hiding (takeWhile)
import Data.List (partition)
import Control.Monad

data Repository = Repository { repoName         :: RepoName
                             , repoAccessLevels :: [(AccessLevel, [Member])]
                             }
                  deriving (Show, Eq, Ord, Typeable, Data)

data Member = UserMember  String
            | GroupMember String
              deriving (Data, Typeable, Show, Eq, Ord)

data AccessLevel = R | W | RW | RWPlus
                   deriving (Show, Eq, Ord, Data, Typeable)

data User = User { userName     :: String
                 , userPubKey   :: ByteString
                 } deriving (Eq, Ord, Typeable, Data, Show)

data Gitolite = Gitolite { admin :: Repository
                         , repositories :: [Repository]
                         , users :: [User]
                         , groups :: [Group]
                         } deriving (Show, Eq, Ord, Data, Typeable)

data Group = Group { groupName   :: String
                   , groupMember :: [UserName]
                   } deriving (Show, Eq, Ord, Data, Typeable)

data GitoliteException = GitError GitError
                       | FileError String
                       | ParseError String
                       | ConfError String
                         deriving (Show, Typeable)

instance Exception GitoliteException

type RepoName = String
type UserName = String

-- | parse Gitolite repository
parseGitolite :: FilePath       -- ^ Path to gitolite repositories
              -> IO Gitolite    -- ^ Gitolite repos
parseGitolite path = do
  let adminDir = path </> "gitolite-admin.git"
  gobj <- gitPathToObj ("/conf" </> "gitolite.conf") adminDir
  case gobj of
    GoBlob _ src ->
      case parseOnly (skipSpace *> gitoliteP <* skipSpace <* endOfInput) src of
        Left err  -> throwIO $ ParseError err
        Right (gs, rs) -> do
          print (gs, rs)             
          let (ads, rest) = partition ((== "gitolite-admin") . repoName) rs
          case ads of
            []      -> throwIO $ ConfError "No settings about gitolite-admin"
            _:_:_   -> throwIO $ ConfError "Too many settings about gitolite-admin"
            [admin] -> do
              users <- getUsers path
              return $ Gitolite { admin = admin, repositories = rest, users = users, groups =  gs}
    _ -> throwIO $ FileError "gitolite.conf not found"

getUsers :: FilePath -> IO [User]
getUsers path = do
  let adpath = path </> "gitolite-admin.git"
  gobj <- gitPathToObj "/keydir" adpath
  case gobj of
    GoTree _ ents -> forM (filter cond ents) $ \ ~(GitTreeEntry (RegularFile _) name ref) -> do
      let uname = dropExtension name
      obj <- sha1ToObj ref adpath
      case obj of
        GoBlob _ blob -> return $ User uname blob
        _ -> throwIO $ FileError $ "something wrong with pubkey: " ++ name
    _ -> throwIO $ FileError "something wrong with keydir"
  where
    cond (GitTreeEntry (RegularFile _) name _) = takeExtension name == ".pub"
    cond _                                     = False

gitoliteP :: Parser ([Group], [Repository])
gitoliteP = do
  (,) <$> groupP `sepBy` endOfLine
      <*  skipMany endOfLine
      <*> repoP `sepBy` many endOfLine

symbol :: ByteString -> Parser ByteString
symbol str = string str <* skipSpace

repoP :: Parser Repository
repoP = Repository <$ skipSpace <* string "repo" <* skipSpace
                   <*> repoIdent <* endOfLine
                   <*> many (skipSpace *> accesslevelP)

accesslevelP :: Parser (AccessLevel, [Member])
accesslevelP = (,) <$ skipSpace
                   <*> accessLevelP
                   <* skipSpace <* symbol "="
                   <*> memberP `sepBy` space

accessLevelP :: Parser AccessLevel
accessLevelP = RWPlus <$ string "RW+"
           <|> RW     <$ string "RW"
           <|> R      <$ string "R"

identifier, repoIdent :: Parser String
identifier = unpack <$> takeWhile1 (inClass "-a-zA-Z0-9")
repoIdent = unpack <$> takeWhile1 (inClass "-/a-zA-Z0-9")

groupP :: Parser Group
groupP = Group <$> (tail <$> groupNameP)
               <*  skipSpace <* symbol "="
               <*> userNameP `sepBy1` space

memberP :: Parser Member
memberP = GroupMember . tail <$> groupNameP
      <|> UserMember  <$> userNameP

groupNameP :: Parser String
groupNameP = (:) <$> char '@' <*> identifier

userNameP :: Parser String
userNameP = identifier
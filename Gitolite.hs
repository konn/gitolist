module Gitolite (module DataTypes, parseGitolite) where
import Data.Git
import System.Git
import System.FilePath
import Data.ByteString.Char8 hiding (filter, concat, map, count, empty)
import Control.Exception hiding (try)
import Text.Parsec
import DataTypes
import Data.Map hiding (map, filter, partition)
import Data.List (partition)
import Control.Applicative hiding ((<|>), many, empty)
import Data.Either
import Control.Monad
import Prelude

type GroupState = Map String [String]
type Parser = Parsec ByteString GroupState
data GLine = RepoLine [String] | PermissionLine Permission [UserName]
             deriving (Show, Eq, Ord)

type GLineParser = Parsec [GLine] ()


-- | parse Gitolite repository
parseGitolite :: FilePath       -- ^ Path to gitolite repositories
              -> IO Gitolite    -- ^ Gitolite repos
parseGitolite path = do
  let adminDir = path </> "gitolite-admin.git"
  gobj <- gitPathToObj ("/conf" </> "gitolite.conf") adminDir
  us <- getUsers path
  case gobj of
    GoBlob _ src ->
      case runParser (confParser <* eof) (fromList [("all", map userName us)]) "gitolite.conf" src of
        Left err  -> throwIO $ ParseError err
        Right rs -> do
          let (ads, rest) = partition ((== "gitolite-admin") . repoName) rs
          case ads of
            []      -> throwIO $ ConfError "No settings about gitolite-admin"
            _:_:_   -> throwIO $ ConfError "Too many settings about gitolite-admin"
            [admin] -> do
              return $ Gitolite { gitolitePath = path, admin = admin, repositories = rest, users = us}
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

repoLine :: GLineParser [String]
repoLine = tokenPrim show (const . const) cond
    where
      cond (RepoLine ss) = Just ss
      cond _             = Nothing

permLine :: GLineParser (Permission, [UserName])
permLine = tokenPrim show (const . const) cond
    where
      cond (PermissionLine p us) = Just (p, us)
      cond _                     = Nothing

confParser :: Parser [Repository]
confParser = do
  ls <- rights  <$> (Left <$> groupDefP <|> Right <$> glineP)
                          `sepEndBy1` (many1 newline)
  pos <- getPosition
  let ans = parse (setPosition pos >> lineParser) (sourceName pos) ls
  case ans of
    Left err -> fail $ show err
    Right an -> return an

lineParser :: GLineParser [Repository]
lineParser = concat <$> many l
  where
    l = flip (map . (flip Repository)) <$> repoLine <*> many1 permLine

glineP :: Parser GLine
glineP = try repoLineP <|> permLineP

groupP :: Parser [String]
groupP = findWithDefault [] <$> groupNameP <*> getState

groupDefP :: Parser ()
groupDefP = do
  name <- groupNameP <* skipSpace <* char '=' <* skipSpace
  members <- (groupP <|> count 1 anyName) `sepEndBy1` skipSpace
  modifyState $ insertWith (++) name $ concat members

groupNameP :: Parser String
groupNameP = char '@' *> identifier

skipSpace :: Parser ()
skipSpace = skipMany $ oneOf " \t"

anyName :: Parser String
anyName = concat <$>
            (many1 $ (:) <$> char '\\' <*> count 1 anyChar
                 <|> count 1 (noneOf " \t\r\n="))

identifier, repoIdent :: Parser  String
identifier = many1 alphaNum
repoIdent = many1 (alphaNum <|> oneOf "-/")

repoLineP :: Parser GLine
repoLineP = do
  skipSpace >> string "repo" >> skipSpace
  RepoLine . concat <$> (groupP <|> count 1 repoIdent) `sepEndBy1` skipSpace

permLineP :: Parser GLine
permLineP = do
  skipSpace
  PermissionLine <$> (levelP <* skipSpace <*> anyName `sepEndBy` skipSpace)
                 <*  char '=' <* skipSpace
                 <*> membersP

levelP :: Parser ([Refex] -> Permission)
levelP = RWPlus <$ try (string "RW+")
     <|> RW     <$ try (string "RW")
     <|> R      <$ string "R"
     <|> Minus  <$ string "-"
     <|> C      <$ string "C"

userNameP :: Parser String
userNameP = identifier

membersP :: Parser [String]
membersP = concat <$> ((groupP <|> count 1 userNameP) `sepEndBy1` skipSpace)

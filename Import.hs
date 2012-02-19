module Import
    ( module Prelude
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , module Gitolite
    , module Data.Maybe
    , getGitolite
    , withRepo
    , withRepoObj
    , isBlob
    , isTree
    , isRegularFile
    , isDirectory
    , renderPath
    ) where

import Prelude hiding (writeFile, readFile)
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Gitolite
import qualified Data.Git as Git
import qualified System.Git as Git
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe

isBlob, isTree :: Git.GitObject -> Bool
isBlob (Git.GoBlob _ _) = True
isBlob _                = False
isTree (Git.GoTree _ _) = True
isTree _                = False

isRegularFile :: Git.GitTreeEntry -> Bool
isRegularFile (Git.GitTreeEntry (Git.RegularFile _) _ _) = True
isRegularFile _ = False

isDirectory :: Git.GitTreeEntry -> Bool
isDirectory (Git.GitTreeEntry Git.Directory _ _) = True
isDirectory _             = False

withRepoObj :: String
            -> ObjPiece
            -> (Gitolite -> Repository -> Git.GitObject -> Handler a)
            -> Handler a
withRepoObj repon (ObjPiece commit path) act = do
  withRepo repon $ \git repo -> do
    let curPath = intercalate "/" $ commit:path
    obj <- liftIO $ Git.gitPathToObj curPath (repoDir git repo)
    setSessionBS "curPath" (BS.pack curPath)
    ans <- act git repo obj
    deleteSession "curPath"
    return ans

withRepo :: String -> (Gitolite -> Repository -> Handler a) -> Handler a
withRepo repon act = do
  git <- getGitolite
  let mrep = find ((== repon) . repoName) $ repositories git
  case mrep of
    Nothing -> notFound
    Just repo -> act git repo

getGitolite :: Handler Gitolite
getGitolite = liftIO $ parseGitolite repositoriesPath

renderPath :: ObjPiece -> String
renderPath (ObjPiece a b) = intercalate "/" (a:b)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

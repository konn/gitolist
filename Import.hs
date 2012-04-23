{-# LANGUAGE ScopedTypeVariables #-}
module Import
    ( module Prelude
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , module Gitolite
    , module Data.Maybe
    , module Settings.StaticFiles
    , getGitolite
    , withRepo
    , withRepoObj
    , isBlob
    , repoLayout
    , treeLink
    , isTree
    , module Yesod.Auth
    , module ContentTypes
    , isRegularFile
    , isDirectory
    , renderPath
    , module Encodings
    ) where

import Yesod.Auth hiding (Route)
import Yesod.Default.Config
import Prelude hiding (writeFile, readFile, catch)
import Foundation
import Text.Hamlet (hamletFile)
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Gitolite hiding (User)
import qualified Data.Git as Git
import qualified System.Git as Git
import qualified Data.ByteString.Char8 as BS
import Data.List
import qualified Settings
import Settings.StaticFiles
import Data.Maybe (fromMaybe, listToMaybe)
import Database.Persist.Store
import qualified Data.Text as T
import Encodings
import ContentTypes
import Control.Exception (try, SomeException(..))
import System.FilePath
import System.Directory

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
    let gitDir         = repoDir git repo
        (prefix, rest) = splitAt 2 commit
    
    root <- liftIO $ do
      isHash <- doesFileExist $ gitDir </> "objects" </> prefix </> rest
      if isHash
        then Git.sha1ToObj (Git.SHA1 commit) gitDir
        else repoBranch git repo commit >>= flip Git.sha1ToObj gitDir . commitRef . branchHEAD
    let curPath = intercalate "/" (commit:path)
    obj <- liftIO $ traverseGoTree git repo path root
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
    Just repo -> do
      mu <- maybeAuth
      let uName = maybe Settings.guestName (userIdent . entityVal) mu
      if repo `isReadableFor` uName
        then act git repo
        else permissionDenied $ T.pack $
               "You don't have permission for repository " ++ repon

getGitolite :: Handler Gitolite
getGitolite = liftIO $ parseGitolite repositoriesPath

renderPath :: ObjPiece -> String
renderPath (ObjPiece a b) = intercalate "/" (a:b)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

treeLink :: String -> ObjPiece -> Widget
treeLink repon (ObjPiece c as) =
  let ents = if null as then [[]] else init $ inits as
  in [whamlet|
     <ul .breadcrumb>
       $forall e <- ents
        <li>
           <span .divider>/
           <a href=@{TreeR repon (ObjPiece c e)}>
             $if null e
               #{c}
             $else
               #{last e}
       $if (not (null as))
         <li>
           <span .divider>/ #
           #{last as}
     |]

repoLayout :: String -> ObjPiece -> Widget -> Handler RepHtml
repoLayout repon op@(ObjPiece commit ps) widget = withRepoObj repon op $ \git repo obj -> do
  master <- getYesod
  mmsg <- getMessage
  route <- getCurrentRoute
  let curTab = case route of
                 Just (TreeR _ _)    -> "tab_files" :: String
                 Just (BlobR _ _)    -> "tab_files"
                 Just (TagsR _)      -> "tab_tags"
                 Just (CommitsR _ _) -> "tab_commits"
                 Just (CommitR _ _)  -> "tab_commits"
                 _                   -> "tab_files"
  description <- liftIO $ getDescription git repo
  branches <- liftIO $ repoBranches git repo
  musr <- fmap entityVal <$> maybeAuth
  let curPath = treeLink repon op
  pc <- widgetToPageContent $ do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"
    addScript $ StaticR js_bootstrap_dropdown_js
    addStylesheet $ StaticR css_bootstrap_responsive_css
    addStylesheet $ StaticR css_bootstrap_css
    $(widgetFile "normalize")
    $(widgetFile "repo-layout")
  hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

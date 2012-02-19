{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Repos where

import Import hiding (fileName)
import Control.Monad
import Data.Git
import System.Git
import Text.Pandoc.Highlighting
import Data.String
import System.FilePath
import Data.List (intercalate)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Control.Exception.Lifted (throwIO, evaluate, catch, SomeException(..))
import Text.Blaze

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getTreeR :: String -> ObjPiece -> Handler RepHtml
getTreeR repon op@(ObjPiece a xs) = withRepoObj repon op $ \git repo obj -> do
  unless (isTree obj) $ notFound
  liftIO $ print (repon, op)
  let GoTree _ es = obj
      curPath = treeLink repon op
      mkP pth = ObjPiece a (xs ++ [fileName pth])
  entries <-
    if null xs
      then return es
      else do
        sha1 <- liftIO $ gitPathToSha1 (intercalate "/" $ a:init xs) (repoDir git repo)
        return $ GitTreeEntry Directory  ".." sha1 : es
  defaultLayout $ do
    setTitle $ fromString $ repon ++ " - Gitolist"
    $(widgetFile "tree")

getBlobR :: String -> ObjPiece -> Handler RepHtml
getBlobR repon op@(ObjPiece c ps) = withRepoObj repon op $ \git repo obj -> do
  unless (isBlob obj) $ notFound
  let langs      = languagesByExtension $ takeExtension $ last ps
      GoBlob _ b = obj
  src <- evaluate (T.unpack $ T.decodeUtf8 b)
    `catch` \(e :: SomeException) -> liftIO $ throwIO $ InternalError $ T.pack $ show e
  let curPath = treeLink repon op
      blob    = fromMaybe (toHtml src) $
                  highlight formatHtmlBlock ("", "number":langs, []) src
  defaultLayout $ do
    setTitle $ fromString $ repon ++ " - Gitolist"
    $(widgetFile "blob")

getTagsR :: String -> Handler RepHtml
getTagsR repon = defaultLayout $ do
                   let curPath = repon
                   setTitle "Tags"
                   $(widgetFile "tags")

getCommitsR :: String -> ObjPiece -> Handler RepHtml
getCommitsR = undefined

getCommitR :: String -> BS.ByteString -> Handler RepHtml
getCommitR = undefined

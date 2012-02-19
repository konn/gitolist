module Handler.Repos where

import Import hiding (fileName)
import Control.Monad
import Data.List (intersperse)
import Prelude
import Data.Git
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import System.Git
import qualified Data.ByteString as BS

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
  let GoTree _ entries = obj
      curPath = repon ++ "/" ++ renderPath op
      mkP pth = ObjPiece a (xs ++ [fileName pth])
  defaultLayout $ do
    setTitle "#{repon}"
    $(widgetFile "tree")

getBlobR :: String -> ObjPiece -> Handler RepHtml
getBlobR repon op = withRepoObj repon op $ \git repo obj -> do
  unless (isBlob obj) $ notFound
  let GoBlob _ b = obj
      blob       = T.decodeUtf8 b
      curPath    = repon ++ "/" ++ renderPath op
  defaultLayout $ do
    setTitle $ "#{repon}"
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

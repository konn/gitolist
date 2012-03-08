{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module Handler.Repos where

import Import hiding (fileName, joinPath)
import Control.Monad
import Data.Git hiding (GitTag(..), GitCommit(..))
import System.Git
import Text.Pandoc
import Text.Pandoc.Highlighting
import qualified Text.Highlighting.Kate as Kate
import Data.String
import System.FilePath
import Data.List (intercalate, find)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Text.Blaze
import Data.Char (toUpper)
import Blaze.ByteString.Builder
import Text.Blaze.XHtml1.Strict hiding (b, map, head, br)
import qualified Text.Blaze.XHtml1.Strict.Attributes as B
import Text.Blaze.XHtml1.Strict.Attributes (href, class_)
import Data.Conduit
import Control.Applicative
import qualified Data.Conduit.List as LC
import Data.Conduit.Zlib
import Data.List (groupBy)
import Data.Function
import Control.Arrow
import Data.Time
import System.Locale

fromBlob :: GitObject -> Maybe String
fromBlob (GoBlob _ bs) = (flip decode bs =<< detectEncoding bs)
                     <|> either (const Nothing) Just (T.unpack <$> T.decodeUtf8' bs)
fromBlob _             = Nothing

getTreeR :: String -> ObjPiece -> Handler RepHtml
getTreeR repon op@(ObjPiece com xs) = withRepoObj repon op $ \git repo obj -> do
  unless (isTree obj) $ notFound
  let GoTree _ es = obj
      mkP pth = ObjPiece com (xs ++ [fileName pth])
      mReadme = find ((=="README") . map toUpper . dropExtension . fileName) es
  readme <-
    case mReadme of
      Just (GitTreeEntry (RegularFile _) fname ref) -> do
        o <- liftIO $ sha1ToObj ref (repoDir git repo)
        case fromBlob o of
          Just src -> do
            let ext    = takeExtension fname
                reader = fromMaybe (readMarkdown defaultParserState) $ lookup ext pandocDic
            return $ Just $ writeHtml defaultWriterOptions $ reader  src
          _ -> return Nothing
      _ -> return Nothing
  entries <-
    if null xs
      then return es
      else do
        sha1 <- liftIO $ gitPathToSha1 (intercalate "/" $ com:init xs) (repoDir git repo)
        return $ GitTreeEntry Directory  ".." sha1 : es
  repoLayout repon op $ do
    setTitle $ fromString $ repon ++ " - Gitolist"
    $(widgetFile "tree")

pandocDic :: [(String, String -> Pandoc)]
pandocDic = [(".xhtml", readHtml defaultParserState)
            ,(".html", readHtml defaultParserState)
            ,(".htm", readHtml defaultParserState)
            ,(".rst", readRST defaultParserState)
            ,(".textile", readTextile defaultParserState)
            ,(".md", readMarkdown defaultParserState)
            ,(".markdown", readMarkdown defaultParserState)
            ]

isImage :: String -> Bool
isImage = (`elem` [".jpg", ".jpeg", ".png", ".gif"])

getBlobR :: String -> ObjPiece -> Handler RepHtml
getBlobR repon op@(ObjPiece c ps) = withRepoObj repon op $ \git repo obj -> do
  unless (isBlob obj) $ notFound
  renderUrl <- getUrlRender
  let ext   = takeExtension $ last ps
      langs = languagesByExtension ext
      blob =
        if isImage ext
        then p $ img ! B.src (toValue $ renderUrl $ RawBlobR repon op)
        else case fromBlob obj of
          Just src ->
            case lookup ext pandocDic of
              Just reader -> writeHtml defaultWriterOptions $ reader src
              _ -> myHighlight (Kate.languagesByFilename $ last ps) src
          _ ->
            pre ! class_ "sourceCode linenums" $ do
              td ! class_ "lineNumbers" $ pre " "
              td ! class_ "sourceCode" $ pre $
                code ! class_ "sourceCode plain" $
                  a ! href (toValue $ renderUrl $ RawBlobR repon op) $ "see raw file"
  repoLayout repon op $ do
    setTitle $ fromString $ repon ++ " - Gitolist"
    $(widgetFile "blob")
      
getRawBlobR :: String -> ObjPiece -> Handler (ContentType, Content)
getRawBlobR repon op@(ObjPiece _ ps) = withRepoObj repon op $ \_ _ obj -> do
  unless (isBlob obj) notFound
  let GoBlob _ b = obj
      ctype      = fromMaybe "text/plain" $ lookup (takeExtension $ last ps) ctypeDic
  return (ctype, ContentBuilder (fromByteString b) (Just $ BS.length b))

myHighlight :: [String] -> String -> Html
myHighlight langs rawCode =
  let lang = fromMaybe "plain" $ listToMaybe langs
  in formatHtmlBlock Kate.defaultFormatOpts {Kate.numberLines = True, Kate.codeClasses = [lang]} $
       Kate.highlightAs lang rawCode

getTagsR :: String -> Handler RepHtml
getTagsR repon = withRepo repon $ \git repo -> do
  tags <- liftIO $ repoTags git repo
  defaultLayout $ do
    $(widgetFile "tags")

getCommitsR :: String -> ObjPiece -> Handler RepHtml
getCommitsR repon op@(ObjPiece br _) = withRepoObj repon op $ \git repo obj -> do
  brs <- liftIO $ repoBranches git repo
  commits <- liftIO (concat <$> mapM (repoCommitsForBranch git repo) brs)
  let commitGroups = map (pprTime . commitDate . head &&& id) $
                       groupBy ((==) `on` utctDay . commitDate) commits
  repoLayout repon op $ do
    $(widgetFile "commits")

pprTime :: UTCTime -> String
pprTime = formatTime defaultTimeLocale "%F"

unSHA1 :: SHA1 -> BS.ByteString
unSHA1 (SHA1 sha1) = T.encodeUtf8 $ T.pack sha1

getCommitR :: String -> BS.ByteString -> Handler RepHtml
getCommitR = undefined

getCompressR :: String -> ObjPiece -> Handler RepTarball
getCompressR repon (ObjPiece c path) = withRepo repon $ \git repo -> do
  tar <- liftIO $ tarGitPath git repo (joinPath $ c:path)
  setHeader "Content-Disposition" $
    T.concat ["attachment; finename=\"", T.pack (repon ++ "-" ++ c), ".tar.gz\""]
  return $ RepTarball $ ContentSource $
             LC.sourceList (LBS.toChunks tar) $= gzip $= LC.map (Chunk . fromByteString)

getBranchesR :: String -> Handler RepHtml
getBranchesR repon = withRepo repon $ \git repo -> do
  branches <- liftIO $ repoBranches git repo
  defaultLayout $ do
    setTitle $ toHtml $ "branches for " ++ repon
    $(widgetFile "branches")

{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
module Handler.Repos where

import Import hiding (fileName)
import Control.Monad
import Data.Git
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
import Text.Blaze
import Data.Char (toUpper)
import Blaze.ByteString.Builder
import Text.Blaze.XHtml1.Strict hiding (b, map)
import Text.Blaze.XHtml1.Strict.Attributes (href, class_)

fromBlob :: GitObject -> Maybe String
fromBlob (GoBlob _ bs) = maybe (either (const Nothing) Just $ T.unpack <$> T.decodeUtf8' bs)
                           (Just . flip decode bs) $
                           detectEncoding bs
fromBlob _             = Nothing

getTreeR :: String -> ObjPiece -> Handler RepHtml
getTreeR repon op@(ObjPiece com xs) = withRepoObj repon op $ \git repo obj -> do
  unless (isTree obj) $ notFound
  let GoTree _ es = obj
      curPath = treeLink repon op
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
  defaultLayout $ do
    setTitle $ fromString $ repon ++ " - Gitolist"
    $(widgetFile "tree")

pandocDic :: [(String, String -> Pandoc)]
pandocDic = [(".xhtml", readHtml defaultParserState)
            ,(".html", readHtml defaultParserState)
            ,(".htm", readHtml defaultParserState)
            ,(".rst", readRST defaultParserState)
            ,(".txt", readNative)
            ,(".textile", readTextile defaultParserState)
            ,(".md", readMarkdown defaultParserState)
            ,(".markdown", readMarkdown defaultParserState)
            ]

getBlobR :: String -> ObjPiece -> Handler RepHtml
getBlobR repon op@(ObjPiece c ps) = withRepoObj repon op $ \git repo obj -> do
  unless (isBlob obj) $ notFound
  renderUrl <- getUrlRender
  let curPath = treeLink repon op
      ext   = takeExtension $ last ps
      langs = languagesByExtension ext
      blob =
        case fromBlob obj of
          Just src ->
            case lookup ext pandocDic of
              Just reader -> writeHtml defaultWriterOptions $ reader src
              _ -> myHighlight (Kate.languagesByFilename $ last ps) src
          _ ->
            table ! class_ "sourceCode" $
              tr ! class_ "sourceCode" $
                mconcat
                  [ td ! class_ "lineNumbers" $ pre " "
                  , td ! class_ "sourceCode" $ pre $
                       code ! class_ "sourceCode plain" $
                          a ! href (toValue $ renderUrl $ RawBlobR repon op) $ "see raw file"
                  ]
  defaultLayout $ do
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
getTagsR repon = defaultLayout $ do
                   let curPath = repon
                   setTitle "Tags"
                   $(widgetFile "tags")

getCommitsR :: String -> ObjPiece -> Handler RepHtml
getCommitsR = undefined

getCommitR :: String -> BS.ByteString -> Handler RepHtml
getCommitR = undefined

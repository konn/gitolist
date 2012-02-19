module Foundation
    ( Gitolist (..)
    , Route (..)
    , GitolistMessage (..)
    , resourcesGitolist
    , Handler
    , Widget
    , ObjPiece(..)
    , module Yesod.Core
    , module Settings
    , liftIO
    , mkObjPiece
    ) where

import Prelude
import Yesod.Core hiding (Route)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static
import Settings.StaticFiles
import Yesod.Logger (Logger, logMsg, formatLogText)
import qualified Settings
import Settings (Extra (..), widgetFile, repositoriesPath)
import Control.Monad.IO.Class (liftIO)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Gitolist = Gitolist
    { settings  :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    }

instance PathPiece BS.ByteString where
  toPathPiece bs = T.decodeUtf8 bs
  fromPathPiece t = Just $ T.encodeUtf8 t

data ObjPiece = ObjPiece String [FilePath]
                deriving (Show, Eq, Ord, Read)

mkObjPiece :: String -> ObjPiece
mkObjPiece = flip ObjPiece []

instance PathMultiPiece ObjPiece where
  toPathMultiPiece (ObjPiece ref paths) = map T.pack (ref:paths)
  fromPathMultiPiece (x:xs) = Just $ ObjPiece (T.unpack x) (map T.unpack xs)
  fromPathMultiPiece _      = Nothing

-- Set up i18n messages. See the message folder.
mkMessage "Gitolist" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype GitolistRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Gitolist = GitolistRoute
-- * Creates the value resourcesGitolist which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Gitolist. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the GitolistRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Gitolist" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Gitolist where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal (const $ Left ()) base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

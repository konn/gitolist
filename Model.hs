{-# LANGUAGE TypeSynonymInstances #-}
module Model where

import Prelude
import Yesod
import Yesod.Auth.HashDB hiding (User, UserGeneric(..), UniqueUser)
import Data.Text (Text)
import Database.Persist.Quasi
import Database.Persist.MongoDB
import Language.Haskell.TH.Syntax
import GitUtils (UserName)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist MkPersistSettings { mpsBackend = ConT ''Action }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
  userPasswordHash = Just . userPassword
  userPasswordSalt = userSalt
  setSaltAndPasswordHash s h u = u { userSalt = Just s
                                   , userPassword = h
                                   }

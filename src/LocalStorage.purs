-- TODO
-- stored in LS:
--   schema version for bms, and bms. used by badge (can it not contact native messaging?)
--   (not here yet) staged bookmarks. this makes sense

module LocalStorage where

import Prelude

import Bookmark (LocalBookmark)
import Config (bookmarkSchemaVer)
import Control.Applicative.Custom (ensure)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (fromFoldable)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Either.Custom (fromRight)
import Data.Foldable (class Foldable)
import Data.Functor.Custom ((>#>))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Types (Predicate)

data LocalStorageKey
    = Bookmarks
    | BookmarksSchemaVersion

key :: LocalStorageKey -> String
key Bookmarks = "bookmarks"
key BookmarksSchemaVersion = "bookmarksSchemaVersion"

type LocalStorageState =
    { bookmarks :: Maybe (Array LocalBookmark)
    , bookmarksSchemaVersion :: Maybe Int
    }

empty :: LocalStorageState
empty =
    { bookmarks: Nothing
    , bookmarksSchemaVersion: Nothing
    }

foreign import getImpl :: Array String -> Effect (Promise Json)

get :: forall f. Foldable f => f LocalStorageKey -> Aff LocalStorageState
get = fromFoldable >#> key >>> getImpl >>> toAffE >#> (decodeJson >>> fromRight empty)

foreign import setImpl :: LocalStorageState -> Effect (Promise Unit)

set :: LocalStorageState -> Aff Unit
set = setImpl >>> toAffE

getBookmarks :: Aff (Maybe (NonEmptyArray LocalBookmark))
getBookmarks = get ks <#> (ensure validSchema >=> _.bookmarks >=> fromArray)
    where
        ks :: Array LocalStorageKey
        ks = [ Bookmarks, BookmarksSchemaVersion ]

        validSchema :: Predicate LocalStorageState
        validSchema = _.bookmarksSchemaVersion >#> (_ == bookmarkSchemaVer) >>> fromMaybe false

setBookmarks :: forall f. Foldable f => f LocalBookmark -> Aff Unit
setBookmarks xs = set { bookmarks: Just (fromFoldable xs), bookmarksSchemaVersion: Just bookmarkSchemaVer }


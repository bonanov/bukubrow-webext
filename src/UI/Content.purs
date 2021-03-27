module UI.Content (content) where

import Prelude

import Bookmark (RemoteBookmark, local)
import Bukubrow (HostFailure)
import Capability.RemoteData (class RemoteData, checkConnection, getRemoteBookmarks)
import Data.Array (length)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import UI.Components.Bookmark (bookmark, Output)
import UI.Components.Onboarding (onboarding)

type Slots =
    ( onboardingSlot :: H.Slot (Const Void) Void Unit
    , bookmarkSlot :: H.Slot (Const Void) Output Int
    )

data Bookmarks
    = Unbegun
    | Fetched (Either HostFailure (Array RemoteBookmark))

type State =
    { bookmarks :: Bookmarks
    }

data Action
    = Init

initialState :: State
initialState =
    { bookmarks: Unbegun
    }

content :: forall q i o m. RemoteData m => H.Component HH.HTML q i o m
content = H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handler
        , initialize = Just Init
        }
    }

handler :: forall i o m. RemoteData m => Action -> H.HalogenM State Action i o m Unit
handler = case _ of
    Init -> do
       status <- checkConnection
       bms <- getRemoteBookmarks
       H.modify_ \s -> s { bookmarks = Fetched $ const (fromMaybe [] bms) <$> status }

render :: forall m. State -> H.ComponentHTML Action Slots m
render s = case s.bookmarks of
    Unbegun     -> HH.div_ []
    Fetched res -> HH.div_
        [ case res of
            Left _    -> HH.slot (SProxy :: _ "onboardingSlot") unit onboarding unit (const Nothing)
            Right bms -> HH.div_ $ mapWithIndex (\i rembm ->
                HH.slot (SProxy :: _ "bookmarkSlot") i bookmark { bookmark: local rembm } (\output -> Nothing)
            ) bms
        ]


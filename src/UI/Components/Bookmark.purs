module UI.Components.Bookmark (bookmark, Output(..)) where

import Prelude

import Bookmark (LocalBookmark, Link (..), unLink)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Tag as Tag
import UI.Utils (thenRender)

type Input =
    { bookmark :: LocalBookmark
    }

type State =
    { bookmark :: LocalBookmark
    }

data Output
    = LinkClicked

bookmark :: forall q m. H.Component HH.HTML q Input Output m
bookmark = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }

initialState :: Input -> State
initialState = identity

render :: forall a m. State -> H.ComponentHTML a () m
render { bookmark: bm } =
    let isBookmarklet = case bm.url of
            BookmarkletLink _ -> true
            _                 -> false

    in HH.div [ HP.class_ $ ClassName "bm" ]
        [ HH.div_
            [ HH.header_
                [ HH.h1 [ HP.class_ $ ClassName "bm-name" ]
                    [ HH.text "TODO spaced badge"
                    , HH.text bm.title
                    , HH.text "TODO &nbsp;"
                    ]
                ]
            , HH.ul [ HP.class_ $ ClassName "bm-tags" ]
                [ HH.text $ "TODO list of tags: " <> intercalate ", " (Tag.toString <$> bm.tags)
                ]
            , not (null bm.desc) `thenRender` \_ ->
                HH.p [ HP.class_ $ ClassName "bm-desc" ]
                    [ HH.text bm.desc
                    ]
            , not isBookmarklet `thenRender` \_ ->
                HH.h2 [ HP.class_ $ ClassName "bm-url" ]
                    [ HH.text $ unLink bm.url
                    ]
            ]
        , HH.div [ HP.class_ $ ClassName "bm-ctrls" ]
            [ HH.div_
                [ HH.text "TODO icon btns"
                ]
            , HH.text "TODO ctrl tooltip"
            ]
        , HH.text "TODO bmlet graphic icon"
        ]


module UI.Utils where

import Prelude
import Halogen.HTML as HH

-- TODO
-- thunked f for performance given strictness
-- see also: https://github.com/thomashoneyman/purescript-halogen-realworld/blob/main/src/Component/HTML/Utils.purs
thenRender :: forall a b. Boolean -> (Unit -> HH.HTML a b) -> HH.HTML a b
thenRender false _ = HH.text ""
thenRender true f = f unit


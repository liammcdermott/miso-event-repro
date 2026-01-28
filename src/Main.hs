{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
import Miso.Html
import Miso.Lens (Lens, lens)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

newtype ParentModel = ParentModel
  { _parentSomeState :: Maybe ()
  }
  deriving (Eq, Show)

data ChildModel = ChildModel
  { _childSomeState :: Maybe ()
  , _childClicks :: Int
  }
  deriving (Eq, Show)

data ParentAction
  = ToggleParent
  deriving (Eq, Show)

data ChildAction
  = ChildClicked
  deriving (Eq, Show)

parentSomeState :: Lens ParentModel (Maybe ())
parentSomeState = lens _parentSomeState $ \m v -> m { _parentSomeState = v }

childSomeState :: Lens ChildModel (Maybe ())
childSomeState = lens _childSomeState $ \m v -> m { _childSomeState = v }

initialParentModel :: ParentModel
initialParentModel = ParentModel
  { _parentSomeState = Nothing
  }

initialChildModel :: ChildModel
initialChildModel = ChildModel
  { _childSomeState = Nothing
  , _childClicks = 0
  }

main :: IO ()
main = startApp defaultEvents app

app :: App ParentModel ParentAction
app = component initialParentModel updateParent viewParent

updateParent :: ParentAction -> Effect ROOT ParentModel ParentAction
updateParent ToggleParent =
  modify $ \m ->
    let next = case _parentSomeState m of
          Nothing -> Just ()
          Just _ -> Nothing
    in m { _parentSomeState = next }

viewParent :: ParentModel -> View ParentModel ParentAction
viewParent m =
  div_ []
    [ div_ [] [ text "Toggle the parent state, then click inside the child component." ]
    , button_ [ onClick ToggleParent ] [ text "Toggle child.someState (parent -> child binding)" ]
    , div_ [] [ text $ "parent.someState = " <> parentLabel ]
    , div_ [] [ text "Child component:" ]
    , "child-component" +> childComponent
    ]
  where
    parentLabel = case _parentSomeState m of
      Nothing -> "Nothing"
      Just _ -> "Just ()"

childComponent :: Component ParentModel ChildModel ChildAction
childComponent =
  (component initialChildModel updateChild viewChild)
    { bindings = [ parentSomeState --> childSomeState ]
    }

updateChild :: ChildAction -> Effect ParentModel ChildModel ChildAction
updateChild ChildClicked =
  modify $ \m -> m { _childClicks = _childClicks m + 1 }

viewChild :: ChildModel -> View ChildModel ChildAction
viewChild m =
  div_ []
    (case _childSomeState m of
      Nothing ->
        []
      Just _ ->
        [ div_ [ onClick ChildClicked ] [ text "hello world, click me" ]
        , div_ [] [ text $ "child clicks = " <> ms (show (_childClicks m)) ]
        ]
    )

-- If we switch out @viewChild@ for this it works:
-- viewChild :: ChildModel -> View ChildModel ChildAction
-- viewChild m =
--   div_ []
--     (case _childSomeState m of
--       Just _ ->
--         []
--       Nothing ->
--         [ div_ [ onClick ChildClicked ] [ text "hello world, click me" ]
--         , div_ [] [ text $ "child clicks = " <> ms (show (_childClicks m)) ]
--         ]
--     )
-- The only difference seems to be which branch executes first.

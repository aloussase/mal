module Mal.Editor.TextEditor
(
    TextEditor
  , empty
  , new
  , getContents
  , setContents
)
where

import           Prelude       hiding (getContents)

import           Control.Monad
import           Data.GI.Base  hiding (new)
import           Data.Maybe    (fromJust)
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified GI.Gio        as Gio
import qualified GI.Gtk        as Gtk

type TextEditor = Gtk.TextView

-- | Create a new empty TextEditor.
empty :: IO TextEditor
empty = do
  textView <- Gtk.textViewNew
  Gtk.textViewSetAcceptsTab textView True
  Gtk.textViewSetMonospace textView True
  Gtk.widgetSetVexpand textView True
  gutter <- createGutter textView
  Gtk.textViewSetGutter textView Gtk.TextWindowTypeLeft $ Just gutter

  pure textView

-- TODO: Make gutter scroll with text editor.
-- TODO: Make gutter line numbers align with editor lines.
createGutter :: TextEditor -> IO Gtk.ListView
createGutter textEditor = do
  model <- Gtk.stringListNew $ Just ["1"]
  selectionModel <- Gtk.noSelectionNew $ Just model
  listFactory <- Gtk.signalListItemFactoryNew
  listView <- Gtk.listViewNew (Just selectionModel) (Just listFactory)

  textBuffer <- Gtk.textViewGetBuffer textEditor
  void $ Gtk.onTextBufferChanged textBuffer $ do
    contents <- getContents textEditor
    nitems <-  Gtk.listViewGetModel listView
                >>= castTo Gtk.NoSelection  . fromJust
                >>= Gtk.noSelectionGetModel . fromJust
                >>= Gio.listModelGetNItems  . fromJust
    model <- getModel listView
    let linums = map (T.pack . show) [1 .. T.count "\n" contents + 1]
    Gtk.stringListSplice model 0 nitems (Just linums)

  void $ Gtk.onSignalListItemFactorySetup listFactory $ \listItem -> do
    listItemLabel <- Gtk.labelNew $ Just ""
    Gtk.labelSetSingleLineMode listItemLabel True
    Gtk.widgetSetValign listItemLabel Gtk.AlignCenter
    Gtk.listItemSetChild listItem $ Just listItemLabel

  void $ Gtk.onSignalListItemFactoryBind listFactory $ \listItem -> do
    pos <- Gtk.listItemGetPosition listItem
    model <- getModel listView
    Just linum <- Gtk.stringListGetString model pos
    Just listItemLabel <- castTo Gtk.Label . fromJust =<< Gtk.listItemGetChild listItem
    Gtk.labelSetText listItemLabel linum
    Gtk.listItemSetChild listItem $ Just listItemLabel

  pure listView

  where
    getModel listView = do
      Just selectionModel <- castTo Gtk.NoSelection . fromJust =<< Gtk.listViewGetModel listView
      Just stringListModel <- castTo Gtk.StringList . fromJust =<< Gtk.noSelectionGetModel selectionModel
      pure stringListModel

-- | Create a new TextEditor with the given contents.
new :: Text -> IO TextEditor
new contents = do
  textEditor <- empty
  setContents textEditor contents
  pure textEditor

-- | Set the contents of the provided @TextEditor@.
setContents :: TextEditor -> Text -> IO ()
setContents textEditor contents = do
  textBuffer <- Gtk.textViewGetBuffer textEditor
  Gtk.textBufferSetText textBuffer contents $ fromIntegral (T.length contents)

-- | Get the text contents of the provided @TextEditor@.
getContents :: TextEditor -> IO Text
getContents textView = do
  textBuffer <- Gtk.textViewGetBuffer textView
  start <- Gtk.textBufferGetStartIter textBuffer
  end <- Gtk.textBufferGetEndIter textBuffer
  Gtk.textBufferGetText textBuffer start end False

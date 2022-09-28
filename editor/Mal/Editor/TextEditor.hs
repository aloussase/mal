module Mal.Editor.TextEditor
(
    TextEditor
  , empty
  , new
  , getContents
  , setContents
)
where

import           Prelude   hiding (getContents)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified GI.Gtk    as Gtk

type TextEditor = Gtk.TextView

-- | Create a new empty TextEditor.
empty :: IO TextEditor
empty = do
  textView <- Gtk.textViewNew
  Gtk.textViewSetAcceptsTab textView True
  Gtk.textViewSetMonospace textView True
  Gtk.widgetSetVexpand textView True
  pure textView

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

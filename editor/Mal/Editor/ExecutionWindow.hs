module Mal.Editor.ExecutionWindow where

import qualified GI.Gtk as Gtk

new :: IO (Gtk.TextView, Gtk.Notebook)
new = do
  textView <- Gtk.textViewNew
  Gtk.textViewSetEditable textView False
  Gtk.textViewSetMonospace textView True

  scrolledWindow <- Gtk.scrolledWindowNew
  Gtk.scrolledWindowSetChild scrolledWindow $ Just textView

  notebook <- Gtk.notebookNew
  tabTitle <- Gtk.labelNew $ Just "Execution Output"
  _ <- Gtk.notebookAppendPage notebook scrolledWindow (Just tabTitle)

  pure (textView, notebook)


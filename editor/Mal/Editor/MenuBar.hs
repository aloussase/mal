module Mal.Editor.MenuBar
(
  new
)
where

import           Mal.Editor.Actions

import qualified GI.Gio             as Gio
import qualified GI.Gtk             as Gtk

type MenuBar = Gtk.PopoverMenuBar

new :: IO MenuBar
new = do
  menu <- Gio.menuNew

  fileMenu <- createFileMenu
  helpMenu <- createHelpMenu
  runMenu <- createRunMenu

  Gio.menuAppendSubmenu menu (Just "File") fileMenu
  Gio.menuAppendSubmenu menu (Just "Run") runMenu
  Gio.menuAppendSubmenu menu (Just "Help") helpMenu

  Gtk.popoverMenuBarNewFromModel (Just menu)

createRunMenu :: IO Gio.Menu
createRunMenu = do
  runMenu <- Gio.menuNew
  Gio.menuAppend runMenu (Just "Run") (Just "app.run-code")
  pure runMenu

createHelpMenu :: IO Gio.Menu
createHelpMenu = do
  helpMenu <- Gio.menuNew
  aboutItem <- Gio.menuItemNew (Just "About") Nothing
  Gio.menuAppendItem helpMenu aboutItem
  pure helpMenu

createFileMenu :: IO Gio.Menu
createFileMenu = do
  fileMenu <- Gio.menuNew
  Gio.menuAppend fileMenu (Just "Open File") Nothing
  Gio.menuAppend fileMenu (Just "New file") Nothing
  Gio.menuAppend fileMenu (Just "Save") Nothing
  Gio.menuAppend fileMenu (Just "Exit") (Just $ toActionName AppQuit)
  pure fileMenu

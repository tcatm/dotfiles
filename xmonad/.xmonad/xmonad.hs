{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.RotSlaves
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle as MT
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.XUtils (fi)

import DBus.Client
import System.Taffybar.XMonadLog
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Foreign.C.Types (CLong)

import Control.Monad

import Data.Monoid
import Data.Maybe
import Data.Traversable (traverse)
import Data.List

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "x-terminal-emulator"

main = do
    let pp = defaultPP
    client <- connectSession
    xmonad $ pagerHints $ myConfig {
      startupHook = do
                  startupHook myConfig
                  -- This must happen after ewmh config has been activated!
                  setFullscreenSupported
      ,
      logHook = do
                  logHook myConfig
                  dbusLogWithPP client . namedScratchpadFilterOutWorkspacePP $ myPP
    }

-- Statusbar
myPP = taffybarPP { ppCurrent = return ""
                , ppVisible = return ""
                , ppHidden = return ""
                , ppHiddenNoWindows = return "" 
                , ppTitle = taffybarColor "#ffca00" "" . shorten 100
                , ppLayout = return ""
                , ppExtras =  [ logMinimized ]
                }

-- FIXME: actually filter for minimized windows
logMinimized :: Logger
logMinimized = withWindowSet $ \ws -> filterM isMinimized (W.index ws) >>= mapM windowName >>= return . Just . intercalate " <span fgcolor=\"#aaaaaa\">/</span> " . map (wrap "<span fgcolor=\"#ff0000\">" "</span>")
  where
    windowName = fmap show . getName

isMinimized :: Window -> X Bool
isMinimized win = do
      wmstate <- getAtom "_NET_WM_STATE"
      mini <- getAtom "_NET_WM_STATE_HIDDEN"
      wstate <- fromMaybe [] `fmap` getProp32 wmstate win
      return $ fromIntegral mini `elem` wstate

myWorkspaces = ["1:com", "2:web", "3", "4", "5", "6", "7", "8", "9",  "0", "NSP", "full"]

doShiftAndView ws = mconcat $ fmap (\y -> y ws) [\x -> ask >> doF . W.view $ x, doShift]

myManageHook = (composeAll
               [ className =? "Wicd-client.py" --> doFloat
               , className =? "Qalculate" --> doFloat
               , className =? "Pavucontrol" --> doFloat
               , className =? "Do" --> doIgnore
               , className =? "Wine" --> doFloat
               , className =? "Xmessage"  --> doFloat
               , className =? "Gimp" --> doShiftAndView "0"
               , className =? "Wireshark" --> doShiftAndView "0"
               , className =? "VirtualBox" --> doShiftAndView "full"
               , checkDialog --> doFloat
               , (fmap or $ mapM (currentWs =?) ["1:com", "2:web", "0", "NSP", "full"]) <&&> className =? "Roxterm" --> doShiftAndView "3"
               ])
               <+> namedScratchpadManageHook scratchpads

          where role = stringProperty "WM_WINDOW_ROLE"
                icon = stringProperty "WM_ICON_NAME" 
--                doWilderScheiß = do 
--                      w <- gets $ W.hidden . windowset
--                      doShiftAndView . show . W.tag $ head w

newKeys x = M.union (M.fromList (myKeys x)) (keys defaultConfig x)

myKeys conf@(XConfig {XMonad.modMask = modm}) =
             [ ((modm, xK_v ), shellPromptHere myXPConfig)
--             [ ((modm, xK_v ), spawnHere "goa")
--             , ((modm .|. shiftMask, xK_v ), shellPromptHere myXPConfig)
             , ((modm, xK_b), sendMessage ToggleStruts)
             , ((modm, xK_c ), kill)
             , ((modm, xK_F12), xmonadPrompt myXPConfig)
             , ((modm .|. mod1Mask, xK_space ), setLayout $ XMonad.layoutHook conf)
             , ((modm, xK_u), sendMessage MirrorShrink)
             , ((modm, xK_i), sendMessage MirrorExpand)
             , ((modm, xK_g), withFocused toggleBorder)
             , ((modm, xK_s), gridselectWorkspace' defaultGSConfig { gs_cellheight = 60, gs_cellwidth = 400 } W.greedyView)
             , ((modm, xK_k), focusUp)
             , ((modm, xK_j), focusDown)
             , ((modm, xK_a), rotAllUp)
             , ((modm .|. shiftMask,   xK_Right), nextWS)
             , ((modm .|. shiftMask,   xK_Left),  prevWS)
             , ((modm,                 xK_Right), sendMessage $ Go R)
             , ((modm,                 xK_Left ), sendMessage $ Go L)
             , ((modm,                 xK_Up   ), sendMessage $ Go U)
             , ((modm,                 xK_Down ), sendMessage $ Go D)
             , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
             , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
             , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
             , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
             , ((modm,                 xK_f    ), sendMessage $ TL.Toggle "Full")
             , ((modm .|. shiftMask,   xK_w    ), sendMessage $ MT.Toggle REFLECTX)
             , ((modm .|. shiftMask,   xK_e    ), sendMessage $ MT.Toggle REFLECTY)
             , ((modm .|. shiftMask,   xK_r    ), sendMessage $ MT.Toggle MIRROR)

             , ((modm,               xK_m     ), withFocused minimizeWindow)
             , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

             -- scratchpads and similar stuff
             , ((modm, xK_o ), namedScratchpadAction scratchpads "orgmode")
             , ((modm, xK_n ), namedScratchpadAction scratchpads "qalculate")
             , ((modm, xK_p ), namedScratchpadAction scratchpads "jabber")
             , ((modm, xK_F1 ), namedScratchpadAction scratchpads "neo")

             -- spawning things
             , ((modm .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)
             , ((0, 0x1008ffa9), spawn "bin/thinkpad/toggle_touchpad.sh")
             , ((0, 0x1008ff2f), spawn "systemctl suspend")
             -- XF86ScreenSaver
             , ((0, 0x1008ff2d), spawn "~/bin/lock")
             , ((modm, xK_F2), spawn "~/bin/lock")
             , ((modm, xK_Pause), spawn "bin/lockkbd")

             -- Multimedia
             , ((modm, xK_Insert), spawn "bin/mpc prev")
             , ((modm, xK_Home), spawn "bin/mpc toggle")
             , ((modm, xK_Page_Up), spawn "bin/mpc next")
             , ((0, 0x1008ff16), spawn "bin/mpc prev")
             , ((0, 0x1008ff17), spawn "bin/mpc next")
             , ((0, 0x1008ff14), spawn "bin/mpc toggle")

             -- pulseaudio on openrd
             , ((modm .|. shiftMask, xK_Delete), spawn "pactl -s openrd set-sink-volume 0 -- -2%")
             , ((modm .|. shiftMask, xK_Page_Down), spawn "pactl -s openrd set-sink-volume 0 -- +1%")

             , ((modm, xK_Delete), spawn "amixer -c0 -q set Master 5- unmute")
             , ((modm, xK_End), spawn "bin/update_muting.sh")
             , ((modm, xK_Page_Down), spawn "amixer -c0 -q set Master 1.5+ unmute")
             -- XF86AudioLowerVolume
             , ((0, 0x1008ff11), spawn "amixer -c0 -q set Master 5- unmute")
             -- XF86AudioRaiseVolume
             , ((0, 0x1008ff13), spawn "amixer -c0 -q set Master 1.5+ unmute")
             , ((0, 0x1008ff12), spawn "amixer -c29 -q sset Console toggle")


             , ((0, 0x1008ff41), spawn "xset dpms force off")

             , ((modm .|. shiftMask,                 xK_space), layoutSplitScreen 2 (TwoPane (2/5) (3/5)))
             , ((modm .|. shiftMask,                 xK_Tab), layoutSplitScreen 2 (Mirror $ TwoPane (1/2) (1/2)))
             , ((modm .|. controlMask .|. shiftMask, xK_space), rescreen)

             , ((modm, xK_w), viewAndWarp 0)
             , ((modm, xK_e), viewAndWarp 1)
             , ((modm, xK_r), viewAndWarp 2)

             , ((modm, xK_Tab), toggleWS)
             , ((modm .|. mod1Mask, xK_Return), moveTo Next EmptyWS)

             , ((modm, xK_BackSpace), bindOn [("full", focusDown), ("", windows $ W.greedyView "full")])

             -- view (switch to workspace)
             ]++[
              ((m .|. modm .|. mod1Mask, k), windows $ f i)
              | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]

             -- greedy view (move workspace to active screen)
             ]++[
              ((m .|. modm, k), windows $ f i)
              | (i, k) <- zip myWorkspaces $ [xK_1 .. xK_9] ++ [xK_0]
              , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
             ]
          where
            myXPConfig = defaultXPConfig

withTitles l = noFrillsDeco shrinkText myTabConfig l

myLayout =
           boringWindows $
           onWorkspace "full" full $
           avoidStruts $
           trackFloating $
           toggleLayouts full $
           configurableNavigation noNavigateBorders $
           renamed [CutWordsLeft 1] $ minimize $
           smartBorders $
           with_sidebars $
           onWorkspace "1:com" (stack ||| grid) $
           onWorkspace "2:web" (tabs ||| tiled) $
           onWorkspace "0" tabs $
           layouts

         where
           layouts      = tiled |||
                          stack |||
                          grid |||
                          threecol |||
                          tabs

           grid         = renamed [Replace "Grid"] $ GridRatio (4/3)
           threecol     = renamed [Replace "ThreeCol"] $ toggles $ ThreeColMid 1 (3/100) (1/2)
           twopane      = TwoPane (3/100) (1/2) 
           tiled        = renamed [Replace "Tiled"] $ toggles $ resizable
           tabs         = renamed [Replace "Tabs"] $ noBorders (tabbed shrinkText myTabConfig)
           stack        = renamed [Replace "Stack"] $ StackTile 1 (3/100) (4/5)
           full         = noBorders Full
           resizable    = Mirror $ ResizableTall 2 (3/100) (3/5) []

           with_sidebars x = renamed [CutWordsLeft 7] $
                             withIM_abs 240 (Title "Speedbar 1.0") $
                             reflectHoriz $
                             withIM_abs 320 (Title "pino") $
                             withIM_abs 200 (Role "roster") $
                             withIM_abs 400 (Role "mainWindow") $
                             withIM_abs 656 (Title "orgmode") $
                             reflectHoriz x 

           toggles l = mkToggle (single REFLECTX) $
                       mkToggle (single REFLECTY) $
                       mkToggle (single MIRROR) l

-- Main configuration
myConfig = ewmh defaultConfig
           { modMask            = mod4Mask
           , workspaces         = myWorkspaces
           , layoutHook         = myLayout
           , manageHook         = myManageHook <+> manageSpawn <+> manageDocks
           , logHook            = updatePointer (Relative 0.5 0.5)
           , handleEventHook    = fullscreenEventHook
           , keys               = newKeys
           , terminal           = myTerminal
           , borderWidth        = 1
           , normalBorderColor  = "#4d3d00"
           , focusedBorderColor = "#ffca00"
           }

myTabConfig = defaultTheme { inactiveBorderColor = "#336698"
                           , activeTextColor = "#000000"
                           , inactiveTextColor = "#ffffff"
                           , activeColor = "#FFE067"
                           , activeBorderColor = "#FFE067"
                           , inactiveColor = "#336698"
                           , fontName = "xft:Monospace:size=8"
                           , decoHeight = 14
                           }

scratchpads = [ NS "orgmode" "emacs --name orgmode ~/important/org/main.org" (icon =? "orgmode") nonFloating
              , NS "qalculate" "qalculate" (icon =? "Qalculate!") nonFloating
              , NS "jabber" "gajim" (icon =? "Gajim") nonFloating
              , NS "neo" "display ~/Desktop/neo.png" (icon =? "neo.png")
                (customFloating $ W.RationalRect (0) (3/5) (1/1) (2/5))
              ] where icon = stringProperty "WM_ICON_NAME"




-- Helper functions

-- | Check if window is DIALOG window
checkDialog :: Query Bool
checkDialog = ask >>= \w -> liftX $ do
                a <- getAtom "_NET_WM_WINDOW_TYPE"
                dialog <- getAtom "_NET_WM_WINDOW_TYPE_DIALOG"
                mbr <- getProp a w
                case mbr of
                  Just [r] -> return $ elem (fromIntegral r) [dialog]
                  _ -> return False

-- | Helper to read a property
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

viewAndWarp :: Int -> X ()
viewAndWarp n = do
  i <- getScreen $ P n
  whenJust i $ \s -> do
                 ws <- screenWorkspace s
                 whenJust ws $ \w -> windows . W.view $ w

data AddRoster_abs a = AddRoster_abs Rational Property deriving (Read, Show)

instance LayoutModifier AddRoster_abs Window where
  modifyLayout (AddRoster_abs width prop) = applyIM_abs width prop
  modifierDescription _               = "IM"

withIM_abs :: LayoutClass l a => Rational -> Property -> l a -> ModifiedLayout AddRoster_abs l a
withIM_abs width prop = ModifiedLayout $ AddRoster_abs width prop

applyIM_abs :: (LayoutClass l Window) =>
               Rational
            -> Property
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))

applyIM_abs width prop wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' $ stack
    let scr_width = rect_width rect
    let (masterRect, slaveRect) = splitHorizontallyBy (width/(fromIntegral scr_width)) rect
    master <- findM (hasProperty prop) ws
    case master of
        Just w -> do
            let filteredStack = stack >>= W.filter (w /=)
            wrs <- runLayout (wksp {W.stack = filteredStack}) slaveRect
            return ((w, masterRect) : fst wrs, snd wrs)
        Nothing -> runLayout wksp rect

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do { b <- f x; if b then return (Just x) else findM f xs }


setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    p <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ changeProperty32 dpy r a c propModeAppend [fromIntegral p]

fullscreenEventHook' :: Event -> X All
fullscreenEventHook' (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"

  when (typ == wmstate && fi fullsc `elem` dats) $ do
    sendMessage $ TL.Toggle "Full"

  return $ All True

gridselectWorkspace' :: GSConfig WorkspaceId ->
                          (WorkspaceId -> WindowSet -> WindowSet) -> X ()
gridselectWorkspace' conf viewFunc = withWindowSet $ \ws -> do
    let wslist = (filter (((/=) "NSP") . W.tag) (filter (isJust . W.stack) $ W.hidden ws)) ++ map W.workspace (W.current ws : W.visible ws)
    winlist <- sequence . map windowList $ wslist
    let wslist' = zip wslist winlist
    let wss = map (\ws -> concat [(W.tag . fst) ws, " :: ", snd ws]) wslist'
    gridselect conf (zip wss $ map W.tag wslist) >>= flip whenJust (windows . viewFunc)

    where
      windowList = liftM (intercalate ", ") . traverse (fmap show . getName) . W.integrate' . W.stack

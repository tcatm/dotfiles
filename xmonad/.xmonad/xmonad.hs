{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.RotSlaves
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LayoutScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import XMonad.Util.XUtils (fi)

import DBus.Client

import System.Exit
import System.Taffybar.XMonadLog
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Foreign.C.Types (CLong)

import Data.Monoid
import Data.Ratio

import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "x-terminal-emulator"

main = do
    let pp = defaultPP
    client <- connectSession
    xmonad $ pagerHints $ myConfig {
      logHook = logHook myConfig <+> (dbusLogWithPP client $ myPP)
    }

-- Statusbar
myPP = taffybarPP { ppCurrent = return ""
                , ppVisible = return ""
                , ppHidden = return ""
                , ppHiddenNoWindows = return "" 
                , ppTitle = taffybarColor "#ffca00" "" . shorten 100
                , ppLayout = return ""
                }

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9",  "0", "NSP", "full"]

doShiftAndView ws = mconcat $ fmap (\y -> y ws) [\x -> ask >> doF . W.view $ x, doShift]

myManageHook = (composeAll
               [ className =? "Wicd-client.py" --> doFloat
               , className =? "Qalculate" --> doFloat
               , className =? "Pavucontrol" --> doFloat
               , className =? "Do" --> doIgnore
               , className =? "Wine" --> doFloat
               , className =? "Xmessage"  --> doFloat
               , className =? "VirtualBox" --> doShiftAndView "full"
               , checkDialog --> doFloat
               ])
               <+> namedScratchpadManageHook scratchpads

          where role = stringProperty "WM_WINDOW_ROLE"
                icon = stringProperty "WM_ICON_NAME" 

getLayout = withWindowSet $ return . W.layout . W.workspace . W.current

differentTwoPane a b = do
  layout <- fmap description getLayout
  case layout of
    "TwoPane" -> a
    _         -> b

toggleWindow = differentTwoPane
                 (windows $ W.modify' toggleTwoPane)
                 (return ())

windowUp = differentTwoPane
             rotFocusedUp
             (windows $ W.focusUp)

windowDown = differentTwoPane
               rotFocusedDown
               (windows $ W.focusDown)

newKeys = M.fromList . myKeys

myKeys conf@(XConfig {XMonad.modMask = modm}) =
             [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
             , ((modm,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
             , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

             , ((modm,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

             -- modifying the window order
             , ((modm,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
             , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
             , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

             -- resizing the master/slave ratio
             , ((modm,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
             , ((modm,               xK_l     ), sendMessage Expand) -- %! Expand the master area

             -- floating layer support
             , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

             -- increase or decrease number of windows in the master area
             , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
             , ((modm              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

             -- quit, or restart
             , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
             , ((modm              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

             , ((modm, xK_b), sendMessage ToggleStruts)
             , ((modm, xK_c ), kill)
             , ((modm, xK_F12), xmonadPrompt myXPConfig)

             -- move focus up or down the window stack
             , ((modm, xK_k), windowUp)
             , ((modm, xK_j), windowDown)
             , ((modm, xK_Tab), toggleWindow)
             , ((modm, xK_Up),    windowUp)
             , ((modm, xK_Down),  windowDown)

             , ((modm, xK_Right), nextWS)
             , ((modm, xK_Left),  prevWS)

             -- scratchpads and similar stuff
             , ((modm, xK_o ), namedScratchpadAction scratchpads "orgmode")
             , ((modm, xK_n ), namedScratchpadAction scratchpads "qalculate")
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

             , ((modm,                               xK_s), layoutSplitScreen 2 (TwoPane (2/5) (3/5)))
             , ((modm .|. controlMask .|. shiftMask, xK_space), rescreen)

             , ((modm, xK_w), viewAndWarp 0)
             , ((modm, xK_e), viewAndWarp 1)
             , ((modm, xK_r), viewAndWarp 2)

             , ((modm, xK_BackSpace), bindOn [("full", windows W.focusDown), ("", windows $ W.greedyView "full")])

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

myLayout =
           smartBorders $
           onWorkspace "full" Full $
           avoidStruts $
           trackFloating $
           with_sidebars $
           (twopane ||| Full)

         where
           tiled        = Tall 1 (3/100) (1/2)
           twopane      = TwoPane' (3/100) (1/2)

           with_sidebars x = renamed [CutWordsLeft 3] $
                             reflectHoriz $
                             withIM_abs 656 (Title "orgmode") $
                             reflectHoriz x 

-- Main configuration
myConfig = defaultConfig
           { startupHook        = ewmhDesktopsStartup <+> setFullscreenSupported
           , modMask            = mod4Mask
           , workspaces         = myWorkspaces
           , layoutHook         = myLayout
           , manageHook         = myManageHook <+> manageSpawn <+> manageDocks
           , logHook            = ewmhDesktopsLogHook <+> updatePointer (Relative 0.5 0.5)
           , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHook'
           , keys               = newKeys
           , terminal           = myTerminal
           , borderWidth        = 1
           , normalBorderColor  = "#4d3d00"
           , focusedBorderColor = "#ffca00"
           }

scratchpads = [ NS "orgmode" "emacs --name orgmode ~/important/org/main.org" (icon =? "orgmode") nonFloating
              , NS "qalculate" "qalculate" (icon =? "Qalculate!") nonFloating
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

toggleTwoPane :: (Eq a) => W.Stack a -> W.Stack a
toggleTwoPane s@(W.Stack t [] []) = s
toggleTwoPane s@(W.Stack t  l  r)
  | l == []     = W.Stack (head r) [t] (tail r)
  | otherwise   = W.Stack (head l) [] $ t:((tail l) ++ r)

ewmhDesktopsEventHook' :: Event -> X All
ewmhDesktopsEventHook' e = handle e >> return (All True)
  where
    handle :: Event -> X ()
    handle (ClientMessageEvent {
                   ev_window = w,
                   ev_message_type = mt,
                   ev_data = d
           }) = withWindowSet $ \s -> do
           sort' <- getSortByIndex
           let ws = sort' $ W.workspaces s

           a_cd <- getAtom "_NET_CURRENT_DESKTOP"
           a_d <- getAtom "_NET_WM_DESKTOP"
           a_aw <- getAtom "_NET_ACTIVE_WINDOW"
           a_cw <- getAtom "_NET_CLOSE_WINDOW"
           a_ignore <- mapM getAtom ["XMONAD_TIMER"]
           if  mt == a_cd then do
                   let n = head d
                   if 0 <= n && fi n < length ws then
                           windows $ W.view (W.tag (ws !! fi n))
                     else  trace $ "Bad _NET_CURRENT_DESKTOP with data[0]="++show n
            else if mt == a_d then do
                   let n = head d
                   if 0 <= n && fi n < length ws then
                           windows $ W.shiftWin (W.tag (ws !! fi n)) w
                     else  trace $ "Bad _NET_DESKTOP with data[0]="++show n
            else if mt == a_aw then do
                   windows $ W.swapMaster . W.focusWindow w
            else if mt == a_cw then do
                   killWindow w
            else if mt `elem` a_ignore then do
               return ()
            else do
              -- The Message is unknown to us, but that is ok, not all are meant
              -- to be handled by the window manager
              return ()
    handle _ = return ()


data TwoPane' a =
    TwoPane' Rational Rational
    deriving ( Show, Read )

instance LayoutClass TwoPane' a where
    doLayout (TwoPane' _ split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (W.up st) of
                              (master:_) -> [(master,left),(W.focus st,right)]
                              [] -> case W.down st of
                                      (next:_) -> [(W.focus st,left),(next,right)]
                                      [] -> [(W.focus st, rect)]
              where
                (left, right) = f rect split rect
                f (Rectangle _ _ w h)
                  | (w%h) <= (4%3) = splitVerticallyBy
                  | otherwise      = splitHorizontallyBy

    handleMessage (TwoPane' delta split) x =
        return $ case fromMessage x of
                   Just Shrink -> Just (TwoPane' delta (split - delta))
                   Just Expand -> Just (TwoPane' delta (split + delta))
                   _           -> Nothing

    description _ = "TwoPane"

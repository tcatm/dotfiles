{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdateFocus
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.Place
import XMonad.Layout.BoringWindows hiding (Replace, focusUp, focusDown)
import XMonad.Layout.Column
import XMonad.Layout.FixedColumn
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.HintedGrid
import XMonad.Layout.IM
import XMonad.Layout.LayoutScreens
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.TrackFloating
import XMonad.Layout.TwoPane
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.WindowProperties

import DBus.Client

import System.Exit
import System.Taffybar.XMonadLog
import System.Taffybar.Hooks.PagerHints (pagerHints)

import Foreign.C.Types (CLong)

import Control.Monad

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid

import qualified Data.Map as M
import qualified XMonad.StackSet as W
import qualified XMonad.Layout.Groups as G

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
                , ppTitle = return ""
                , ppLayout = return ""
                , ppExtras =  [ logWindows 30 ]
                }

data WindowState = Focused | Visible | Hidden

logWindows :: Int -> Logger
logWindows len = withWindowSet $ \ws -> do
                                let stack = W.stack . W.workspace . W.current $ ws
                                case stack of
                                  Just stack -> do
                                    focused <- liftM (\s -> (Focused, s)) . windowName . W.focus $ stack
                                    ups <- mapM (markState Visible) . W.up $ stack
                                    downs <- mapM (markState Visible) . W.down $ stack
                                    return . Just . intercalate " " $ map prettify $ reverse ups ++ focused : downs
                                  Nothing -> return Nothing

  where
    windowName = fmap show . getName
    markState def w = do
                        name <- windowName w
                        minimized <- isMinimized w
                        if minimized then
                          return (Hidden, name)
                        else
                          return (def, name)

    grey = taffybarColor "#666666" ""
    colorize color = wrap (grey "[ ") (grey " ]") . taffybarColor color ""
    prettify (s, w) = case s of
                        Focused -> colorize "#ffca00" . shorten len $ w
                        Visible -> colorize "#aaaaaa" . shorten len $ w
                        Hidden  -> colorize "#ff0030" . shorten len $ w

myWorkspaces = (splitEvery 1 "αβγδεζηθικ") ++ ["Λ", "Ω"]

doShiftAndView ws = mconcat $ fmap (\y -> y ws) [\x -> ask >> doF . W.view $ x, doShift]

killAll = withWindowSet $ mapM_ killWindow . W.index

myManageHook = (composeAll
               [ className =? "Wicd-client.py" --> doFloat
               , className =? "Qalculate" --> doFloat
               , className =? "Pavucontrol" --> doFloat
               , className =? "Do" --> doIgnore
               , className =? "Wine" --> doFloat
               , className =? "Xmessage"  --> doFloat
               , className =? "VirtualBox" --> doShiftAndView "full"
               , className =? "Cssh" --> (placeHook (fixed (1/2, 0)) <+> doFloat)
               , checkDialog --> doFloat
               ])
               <+> namedScratchpadManageHook scratchpads

          where role = stringProperty "WM_WINDOW_ROLE"
                icon = stringProperty "WM_ICON_NAME"

newKeys = M.fromList . myKeys

myMouse (XConfig {XMonad.modMask = modMask}) = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> mouseMoveWindow w >> (windows W.shiftMaster))

    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)

    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myKeys conf@(XConfig {XMonad.modMask = modm}) =
             [ ((modm,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
             , ((modm .|. shiftMask, xK_space ), sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout) -- %! Rotate through the available layout algorithms
             , ((modm,               xK_f     ), sendMessage $ TL.Toggle "Full")
             , ((modm .|. shiftMask, xK_BackSpace ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

             , ((modm,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

             -- modifying the window order
             , ((modm,               xK_Return), swapMaster >> refresh) -- %! Swap the focused window and the master window

             -- resizing the master/slave ratio
             , ((modm,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
             , ((modm,               xK_l     ), sendMessage Expand) -- %! Expand the master area

             -- minimizing windows
             , ((modm,               xK_m     ), withFocused minimizeWindow)
             , ((modm .|. shiftMask, xK_m     ), sendMessage RestoreNextMinimizedWin)

             -- floating layer support
             , ((modm,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

             -- increase or decrease number of windows in the master area
             , ((modm              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
             , ((modm              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

             -- quit, or restart
             , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
             , ((modm              , xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

             , ((modm, xK_b), sendMessage ToggleStruts)
             , ((modm              , xK_c), kill)
             , ((modm .|. shiftMask, xK_c), killAll >> (setLayout $ XMonad.layoutHook conf))

             , ((modm, xK_F12), xmonadPrompt myXPConfig)
             , ((modm .|. shiftMask, xK_F12), inputPrompt defaultXPConfig "prog" ?+ ((flip restart) False))

             -- move focus
             , ((modm, xK_k), focusUp)
             , ((modm, xK_j), focusDown)
             , ((modm .|. shiftMask, xK_k), swapUp >> refresh)
             , ((modm .|. shiftMask, xK_j), swapDown >> refresh)

             , ((modm,               xK_Left ), prevWS)
             , ((modm,               xK_Right), nextWS)
             , ((modm .|. shiftMask, xK_Left ), shiftToPrev >> prevWS)
             , ((modm .|. shiftMask, xK_Right), shiftToNext >> nextWS)
             , ((modm,               xK_Up   ), focusGroupUp)
             , ((modm,               xK_Down ), focusGroupDown)
             , ((modm .|. shiftMask, xK_Up   ), moveToGroupUp False)
             , ((modm .|. shiftMask, xK_Down ), moveToGroupDown False)
             , ((modm              , xK_Tab  ), focusGroupDown)
             , ((modm .|. shiftMask, xK_Tab  ), moveToGroupDown True)

             -- scratchpads and similar stuff
             , ((modm, xK_p ), namedScratchpadAction scratchpads "gajim")
             , ((modm, xK_o ), namedScratchpadAction scratchpads "orgmode")
             , ((modm, xK_n ), namedScratchpadAction scratchpads "qalculate")

             -- spawning things
             , ((modm .|. shiftMask, xK_Return), spawnHere $ XMonad.terminal conf)
             , ((0, 0x1008ffa9), spawn "bin/thinkpad/toggle_touchpad.sh")
             , ((0, 0x1008ff2f), spawn "systemctl suspend")
             -- XF86ScreenSaver
             , ((0, 0x1008ff2d), spawn "xset s activate")
             , ((modm, xK_F2), spawn "xset s activate")
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
             , ((modm, xK_End), spawn "amixer -c29 set Console toggle")
             , ((modm, xK_Page_Down), spawn "amixer -c0 -q set Master 1.5+ unmute")
             -- XF86AudioLowerVolume
             , ((0, 0x1008ff11), spawn "amixer -c0 -q set Master 5- unmute")
             -- XF86AudioRaiseVolume
             , ((0, 0x1008ff13), spawn "amixer -c0 -q set Master 1.5+ unmute")
             -- XF86AudioMute
             --, ((0, 0x1008ff12), spawn "amixer -c29 -q set Console toggle")
             -- XF86AudioMicMute
             , ((0, 0x1008ffb2), spawn "amixer -q set Capture toggle")


             , ((0, 0x1008ff41), spawn "xset dpms force off")
             , ((0, 0x1008ff03), spawn "xbacklight -time 0 - 7")
             , ((0, 0x1008ff02), spawn "xbacklight -time 0 + 7")

             --, ((modm,                               xK_s), layoutSplitScreen 2 (TwoPane (2/5) (3/5)))
             , ((modm,                               xK_s), layoutSplitScreen 2 (TwoPane (1/2) (1/2)))
             , ((modm .|. controlMask .|. shiftMask, xK_space), rescreen)

             , ((modm, xK_w), viewScreen 0)
             , ((modm, xK_e), viewScreen 1)
             , ((modm, xK_r), viewScreen 2)

             , ((modm, xK_BackSpace), bindOn [("full", windows W.focusDown), ("", windows $ W.greedyView "full")])

             ]

             -- view (switch to workspace)
             ++ [ ((m .|. modm .|. mod1Mask, k), windows $ f i)
                | (i, k) <- zip myWorkspaces $ k1k0
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
                ]

             -- greedy view (move workspace to active screen)
             ++ [ ((m .|. modm, k), windows $ f i)
                | (i, k) <- zip myWorkspaces $ k1k0
                , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
                ]

          where
            myXPConfig = defaultXPConfig
            k1k0 = [xK_1..xK_9] ++ [xK_0]

myLayout =
           onWorkspace "Ω" full $
           boringWindows $
           renamed [CutWordsLeft 1] $ minimize $
           avoidStruts $
           (flip G.group) (Full ||| Mirror (Column 1.41) ||| Mirror (Column 1)) $
           smartBorders $
           trackFloating $
           with_sidebars $
           toggleLayouts full $
           renamed [CutWordsLeft 2] $ smartSpacing 3 $
           (Tall 1 (3/100) (1/2) ||| grid ||| code)

         where
           grid         = renamed [Replace "Grid"] $ GridRatio (2/3) True
           full         = noBorders Full
           code         = magnifiercz' 1.41 $ FixedColumn 1 20 90 10

           with_sidebars = renamed [CutWordsLeft 5]
                         . reflectHoriz
                         . withIM (1/2) (Title "orgmode")
                         . withIM (1/2) (ClassName "Sonata")
                         . withIM (1/3) (Role "roster")
                         . reflectHoriz

tabTheme = defaultTheme { fontName = "xft:Roboto Regular:size=9"
                        , inactiveBorderColor = "#231f20"
                        , activeBorderColor = "#231f20"
                        , decoHeight = 26
                        , activeColor = "#231f20"
                        , inactiveColor = "#231f20"
                        , activeTextColor = "#ffca00"
                        , inactiveTextColor = "#aaaaaa"
                        }

-- Main configuration
myConfig = defaultConfig
           { startupHook        = ewmhDesktopsStartup <+> setFullscreenSupported <+> adjustEventInput
           , modMask            = mod4Mask
           , workspaces         = myWorkspaces
           , layoutHook         = myLayout
           , manageHook         = myManageHook <+> manageSpawn <+> manageDocks
           , logHook            = ewmhDesktopsLogHookCustom namedScratchpadFilterOutWorkspace <+> updatePointer (Relative 0.95 0.95)
           , handleEventHook    = fullscreenEventHook <+> ewmhDesktopsEventHookCustom namedScratchpadFilterOutWorkspace
           , mouseBindings      = myMouse
           , keys               = newKeys
           , terminal           = myTerminal
           , borderWidth        = 1
           , normalBorderColor  = "#4d3d00"
           , focusedBorderColor = "#ffca00"
           , focusFollowsMouse  = True
           }

scratchpads = [ NS "orgmode" "emacs --name orgmode ~/important/org/main.org --execute '(org-agenda-list)'" (icon =? "orgmode") nonFloating
              , NS "gajim" "gajim" (propertyToQuery $ Role "roster") nonFloating
              , NS "qalculate" "qalculate" (icon =? "Qalculate!") nonFloating
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

setFullscreenSupported :: X ()
setFullscreenSupported = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    p <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ changeProperty32 dpy r a c propModeAppend [fromIntegral p]

isMinimized :: Window -> X Bool
isMinimized win = do
      wmstate <- getAtom "_NET_WM_STATE"
      mini <- getAtom "_NET_WM_STATE_HIDDEN"
      wstate <- fromMaybe [] `fmap` getProp32 wmstate win
      return $ fromIntegral mini `elem` wstate


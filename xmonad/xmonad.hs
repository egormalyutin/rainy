{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Layout.Gaps
import XMonad.Layout.MouseResizableTile
import XMonad.Layout
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import Data.Default
import Data.Monoid
import qualified Data.Map as M
import System.Exit
import XMonad.Hooks.InsertPosition
import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import qualified XMonad.StackSet as W

startup :: X ()
startup = do
    spawn "killall picom xwinwrap polybar feh"

    spawn "rm /tmp/xmonad-workspace-log"
    spawn "sleep 0.3 && mkfifo /tmp/xmonad-workspace-log"

    -- urxvt config
    spawn "sleep 0.3 && xrdb -load ~/rainy/xresources"

    -- composite manager
    spawn "sleep 0.3 && picom --config ~/rainy/compton.conf"

    -- image wallpaper
    spawn "sleep 0.3 && feh --bg-scale ~/rainy/bg-blur.png"

    -- live wallpaper
    spawn $ "sleep 0.3 && xwinwrap -ni -fdt -sh rectangle -un -b -nf -ovr" ++ 
            " -fs -- mpv -wid WID --no-config --keepaspect=no --loop" ++
            " --no-border --vd-lavc-fast --x11-bypass-compositor=no" ++
            " --gapless-audio=yes --aid=no --vo=xv --hwdec=auto" ++
            " --really-quiet --pause" ++
            " --input-ipc-server=/tmp/mpv-bg-socket ~/rainy/bg.mp4"

    -- default pointer
    spawn "sleep 0.3 && xsetroot -cursor_name left_ptr"

    -- bar
    spawn "sleep 0.3 && polybar -r -c ~/rainy/polybar-config bar"

gap :: Int
gap = 6

fi = fromIntegral

mrt = mouseResizableTile { draggerType = FixedDragger (fi gap) (fi gap) }
applyGaps = gaps $ zip [U, D, R, L] $ repeat gap

layout = avoidStruts (applyGaps mrt)
         ||| avoidStruts (applyGaps mrt { isMirrored = True })
         ||| avoidStruts (applyGaps Full)
         ||| Full

reload :: X ()
reload = spawn $ "if type xmonad; then xmonad --recompile" ++
    " && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

lWorkspaces = ["base", "www", "chaos"]
rWorkspaces = ["music"]
cWorkspaces = show <$> [length lWorkspaces + 1.. 9 - length rWorkspaces]

myWorkspaces :: [String]
myWorkspaces = lWorkspaces ++ cWorkspaces ++ rWorkspaces

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((modm,               xK_Return), spawn $ terminal conf)
    , ((modm,               xK_d     ), spawn "~/rainy/dmenu.sh")
    , ((modm .|. shiftMask, xK_b     ), spawn "chromium")
    , ((modm .|. shiftMask, xK_c     ), spawn "~/rainy/bg-toggle.sh")

    , ((modm .|. shiftMask, xK_q     ), kill)

    , ((modm,               xK_t     ), sendMessage NextLayout)

    , ((modm,               xK_Right ), windows W.focusDown)
    , ((modm,               xK_Left  ), windows W.focusUp  )

    , ((modm .|. shiftMask, xK_Right ), windows W.swapDown)
    , ((modm .|. shiftMask, xK_Left  ), windows W.swapUp)

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    , ((modm .|. shiftMask, xK_space ), withFocused $ windows . W.sink)

    , ((modm,               xK_i     ), sendMessage (IncMasterN 1))
    , ((modm,               xK_p     ), sendMessage (IncMasterN (-1)))

    , ((modm .|. shiftMask, xK_e     ), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_r     ), reload)

    , ((modm,               xK_b     ), sendMessage ToggleStruts)

    , ((0, xF86XK_AudioLowerVolume   ), spawn "amixer -q sset Master 2%-")
    , ((0, xF86XK_AudioRaiseVolume   ), spawn "amixer -q sset Master 2%+")
    , ((0, xF86XK_AudioMute          ), spawn "amixer set Master toggle")

    , ((0, xF86XK_MonBrightnessDown  ), spawn "xbacklight -20%")
    , ((0, xF86XK_MonBrightnessUp    ), spawn "xbacklight +20%")
    ]
    ++
    [((modm .|. m, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    where modm = modMask conf


isEmpty ws tg = any (\w -> (W.tag w == tg) && (W.stack w == Nothing)) ws

lFmt curr ws tg | curr == tg    = "%{B#2e3133} " ++ tg ++ " %{B-} "
                | otherwise     = tg ++ " "

cFmt curr ws tg | curr == tg    = "%{B#2e3133} " ++ tg ++ " %{B-} "
                | isEmpty ws tg = ""
                | otherwise     = tg ++ " "

rFmt curr ws tg | curr == tg && isEmpty ws tg =
                    "%{B#2e3133}%{F#41535b} " ++ tg ++ " %{B-}%{F-} "

                | curr == tg =
                    "%{B#2e3133}%{F#55b5db} " ++ tg ++ " %{B-}%{F-} "

                | isEmpty ws tg =
                    "%{F#41535b}" ++ tg ++ "%{F-} "

                | otherwise =
                    "%{F#55b5db}" ++ tg ++ "%{F-} "

eventLogHook = do
    winset <- gets windowset
    let curr = W.currentTag winset
    let ws = W.workspaces winset

    let lWsStr = join $ map (lFmt curr ws) lWorkspaces
    let cWsStr = join $ map (cFmt curr ws) cWorkspaces
    let rWsStr = join $ map (rFmt curr ws) rWorkspaces

    let wsStr = if cWsStr /= ""
                    then lWsStr ++ "%{F#41535b}/%{F-} "
                          ++ cWsStr ++ "%{F#41535b}/%{F-} " ++ rWsStr
                    else lWsStr ++ rWsStr

    io $ appendFile "/tmp/xmonad-workspace-log" (wsStr ++ "\n")

main :: IO ()
main = do
    xmonad . ewmh . docks $ def
        { terminal = "urxvt"
        , modMask = mod4Mask
        , startupHook = startup
        , borderWidth = 0
        , layoutHook = layout
        , keys = myKeys
        , workspaces = myWorkspaces
        , manageHook = insertPosition Below Newer
        , logHook = eventLogHook
        , handleEventHook = handleEventHook def <+> fullscreenEventHook
        }

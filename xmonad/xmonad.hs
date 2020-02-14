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

startup :: X ()
startup = do
    -- urxvt config
    spawn "xrdb -load ~/rainy/xresources"

    -- composite manager
    spawn "picom --config ~/rainy/compton.conf"

    -- image wallpaper
    spawn "feh --bg-scale ~/rainy/bg-blur.png"

    -- live wallpaper
    spawn $ "xwinwrap -ni -fdt -sh rectangle -un -b -nf -ovr -fs -- mpv" ++
            " --wid WID --no-config --keepaspect=no --loop --no-border" ++
            " --vd-lavc-fast --x11-bypass-compositor=no" ++
            " --gapless-audio=yes --aid=no --vo=xv --hwdec=auto" ++
            " --really-quiet --pause" ++
            " --input-ipc-server=/tmp/mpv-bg-socket ~/rainy/bg.mp4"

    -- default pointer
    spawn "xsetroot -cursor_name left_ptr"

    -- bar
    spawn "polybar -r -c ~/rainy/polybar-config bar"

gap :: Int
gap = 6

fi = fromIntegral

mrt = mouseResizableTile { draggerType = FixedDragger (fi gap) (fi gap) }
applyGaps = gaps $ zip [U, D, R, L] $ repeat gap

layout = avoidStruts (applyGaps mrt)
         ||| avoidStruts (applyGaps mrt { isMirrored = True })
         ||| Full

reload :: X ()
reload = spawn $ "if type xmonad; then xmonad --recompile" ++
    " && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myWorkspaces :: [String]
myWorkspaces = ["base", "www"] ++ (show <$> [3..9]) ++ ["music"]

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((modm,               xK_Return), spawn $ terminal conf)
    , ((modm,               xK_d     ), spawn "~/rainy/dmenu.sh")
    , ((modm .|. shiftMask, xK_b     ), spawn "chromium")

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
    ]
    ++
    [((modm .|. m, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

    where modm = modMask conf

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
        , handleEventHook = handleEventHook def <+> fullscreenEventHook
        }

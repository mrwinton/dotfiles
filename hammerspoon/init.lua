-- All credit to Teo Ljungberg who wrote this config that I have subsequently
-- shamelessly stolen! SRC: https://github.com/teoljungberg/dotfiles

functions = require "functions"

hs.window.animationDuration = 0

hs.hotkey.bind({"ctrl", "alt"}, "r", functions.reloadConfig)

hs.hotkey.bind({"cmd", "alt"}, "right", functions.moveLeft)
hs.hotkey.bind({"cmd", "alt"}, "left", functions.moveRight)
hs.hotkey.bind({"cmd", "alt"}, "up", functions.moveUp)
hs.hotkey.bind({"cmd", "alt"}, "down", functions.moveDown)

hs.hotkey.bind({"cmd", "alt", "shift"}, "left", functions.resizeLeft)
hs.hotkey.bind({"cmd", "alt", "shift"}, "right", functions.resizeRight)
hs.hotkey.bind({"cmd", "alt", "shift"}, "up", functions.resizeUp)
hs.hotkey.bind({"cmd", "alt", "shift"}, "down", functions.resizeDown)

hs.hotkey.bind({"ctrl", "alt"}, "left", functions.moveWindowLeftHalfScreen)
hs.hotkey.bind({"ctrl", "alt"}, "right", functions.moveWindowRightHalfScreen)
hs.hotkey.bind({"ctrl", "alt"}, "return", functions.fullScreenWindow)
hs.hotkey.bind({"ctrl", "alt", "shift"}, "return", functions.centralizeWindow)

hs.hotkey.bind({"ctrl", "alt", "cmd"}, "left", functions.moveWindowToWestDisplay)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "right", functions.moveWindowToEastDisplay)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "up", functions.moveWindowToNorthDisplay)
hs.hotkey.bind({"ctrl", "alt", "cmd"}, "down", functions.moveWindowToSouthDisplay)

-- The bundle ID is fetched with the following AppleScript:
--
-- `osascript -e 'id of app "NAME OF APP"'`
--
local function applicationShortcutToBundleMapping()
  return {
    [1] = nil,
    [2] = nil,
    [3] = nil,
    [4] = nil,
    [5] = nil,
    [6] = "com.apple.safari",
    [7] = "com.spotify.client",
    [8] = "com.tinyspeck.slackmacgap",
    [9] = "com.apple.mail",
    [0] = "com.brave.Browser",
    ["o"] = "net.kovidgoyal.kitty",
    ["p"] = "org.gnu.Emacs",
  }
end
for key, bundleID in pairs(applicationShortcutToBundleMapping()) do
  if bundleID then
    hs.hotkey.bind({"ctrl", "alt"}, tostring(key), functions.toggleApplication(bundleID))
  end
end

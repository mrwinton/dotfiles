-- Credit to:
-- - Teo Ljungberg, https://github.com/teoljungberg/dotfiles
-- - Steve Purcell, https://github.com/purcell/dot-hammerspoon

local utils = require "utils"
local leader = {"ctrl", "alt"}

hs.window.animationDuration = 0

hs.hotkey.bind(leader, "q", utils.moveTopLeft)
hs.hotkey.bind(leader, "w", utils.moveTop)
hs.hotkey.bind(leader, "e", utils.moveTopRight)
hs.hotkey.bind(leader, "a", utils.moveLeft)
hs.hotkey.bind(leader, "s", utils.maximize)
hs.hotkey.bind(leader, "d", utils.moveRight)
hs.hotkey.bind(leader, "z", utils.moveBottomLeft)
hs.hotkey.bind(leader, "x", utils.moveBottom)
hs.hotkey.bind(leader, "c", utils.moveBottomRight)
hs.hotkey.bind(leader, "/", utils.moveWindow)

hs.hotkey.bind(leader, "1", utils.toggleApplication("org.gnu.Emacs"))
hs.hotkey.bind(leader, "2", utils.toggleApplication("net.kovidgoyal.Kitty"))
hs.hotkey.bind(leader, "3", utils.toggleApplication("com.spotify.client"))
hs.hotkey.bind(leader, "m", utils.toggleApplication("com.apple.mail"))
hs.hotkey.bind(leader, ",", utils.toggleApplication("com.tinyspeck.slackmacgap"))
hs.hotkey.bind(leader, ".", utils.toggleApplication("com.brave.Browser"))

hs.hotkey.bindSpec({leader, "\\"}, function ()
    hs.task.new("/bin/sh", nil, {
                  "-l",
                  "-c",
                  "emacsclient --eval '(emacs-everywhere)'"
    }):start()
end)

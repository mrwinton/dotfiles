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

password = utils.appID('/Applications/1Password.app')
dia = utils.appID('/Applications/Dia.app')
beekeeper = utils.appID('/Applications/Beekeeper Studio.app')
emacs = 'org.gnu.Emacs' -- Use bundle ID, emacs-plus installed via homebrew
ghostty = utils.appID('/Applications/Ghostty.app')
mail = 'com.apple.mail' -- Use bundle ID, mail app is not located in `/Applications/`
safari = utils.appID('/Applications/Safari.app')
spotify = utils.appID('/Applications/Spotify.app')

hs.hotkey.bind(leader, "1", utils.toggleApplication(emacs))
hs.hotkey.bind(leader, "2", utils.toggleApplication(ghostty))
hs.hotkey.bind(leader, "3", utils.toggleApplication(dia))
hs.hotkey.bind(leader, "4", utils.toggleApplication(safari))
hs.hotkey.bind(leader, "5", utils.toggleApplication(mail))
hs.hotkey.bind(leader, ",", utils.toggleApplication(beekeeper))
hs.hotkey.bind(leader, ".", utils.toggleApplication(password))
hs.hotkey.bind(leader, "'", utils.toggleApplication(spotify))

hs.hotkey.bindSpec({leader, "\\"}, function ()
    hs.task.new("/bin/zsh", nil, {
                  "-l",
                  "-c",
                  "emacsclient --eval '(emacs-everywhere)'"
    }):start()
end)

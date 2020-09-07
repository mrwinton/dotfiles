pathmunge () {
    if ! echo $PATH | egrep -q "(^|:)$1($|:)" ; then
        if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
        else
            PATH=$1:$PATH
        fi
    fi
}
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
pathmunge /usr/local/bin
pathmunge /usr/local/sbin
pathmunge /usr/local/opt/libxml2/bin after
pathmunge /sbin after
pathmunge $HOME/bin after
pathmunge $HOME/.bin after
pathmunge $HOME/.cask/bin after
pathmunge $HOME/.local/bin after

export PATH

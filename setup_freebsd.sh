pkg_add -r bash
pkg_add -r git
pkg_add -r vim
pkg_add -r portupgrade
pkg_add -r tmux
pkg_add -r ghc
pkg_add -r go
portsnap fetch
portsnap extract
chsh -s /usr/local/bin/bash axiomiety
#pkg_add -r wget TODO: doesn't work because of portconf
vidcontrol -f iso-thin-8x16.fnt
vidcontrol -i mode VESA_800x600

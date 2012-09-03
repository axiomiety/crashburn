pkg_add -r bash
pkg_add -r git
pkg_add -r vim
pkg_add -r portupgrade
portsnap fetch
portsnap extract
chsh -s /usr/local/bin/bash axiomiety
#pkg_add -r wget TODO: doesn't work because of portconf

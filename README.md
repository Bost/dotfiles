```bash
DOTF=/path/to/dotfiles
cd ~
ln -s $DOTF/.gitconfig
# ln -s $DOTF/.proton         # see https://github.com/dvcrn/proton
ln -s $DOTF/emacs/.spacemacs
ln -s $DOTF/.SpaceVim.d

mv ~/.emacs.d ~/.emacs.d.backup
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
cd ~/.emacs.d
git remote add github https://github.com/Bost/spacemacs.git
git fetch github cycle
git checkout cycle
git rebase develop cycle

# curl -sLf https://spacevim.org/install.sh | bash
```

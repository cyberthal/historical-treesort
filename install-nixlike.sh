#!/bin/bash

# find path to repo
SCRIPT_DIR="$(cd "$( dirname "${BASH_SOURCE[0]}")" && pwd )"

# symlink treesort directory
rm -f ~/.emacs.d/private/treesort
ln -s $SCRIPT_DIR/ ~/.emacs.d/private/treesort

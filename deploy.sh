#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

rm -rf ~/.emacs.d/private/cyborganize
ln -s $SCRIPT_DIR ~/.emacs.d/private/


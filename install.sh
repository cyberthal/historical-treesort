#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LINK_NAME="cyborganize"

ln -sf $SCRIPT_DIR ~/.emacs.d/private/$LINK_NAME

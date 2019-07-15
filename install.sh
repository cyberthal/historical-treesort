#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

LINK_NAME="cyborganize"

mkdir ~/Spacemacs/
rm -f ~/Spacemacs/$LINK_NAME
ln -s $SCRIPT_DIR ~/Spacemacs/$LINK_NAME

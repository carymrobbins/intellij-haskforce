#!/bin/bash

IDEA_VERSION=13.1.2
IDEA_TAR=ideaIC-${IDEA_VERSION}.tar.gz
IDEA_DIR=idea-IC-135.690

if [ -f ~/$IDEA_TAR ]; then
    echo "Copying existing IDEA archive."
    cp ~/$IDEA_TAR .
else
    echo "Downloading IDEA archive."
    wget http://download.jetbrains.com/idea/$IDEA_TAR -P ~
    echo "Copying IDEA archive."
    cp ~/$IDEA_TAR .
fi

echo "Removing existing IDEA installation."
rm -rf idea-IC-*

echo "Installing IDEA to idea-IC/"
tar zxf $IDEA_TAR
rm -rf $IDEA_TAR
mv idea-IC-* idea-IC

echo "Creating build.properties file for ant."
echo "idea.home=$(pwd)/idea-IC" > build.properties

echo "Cloning parser helper."
git clone https://github.com/pjonsson/parser-helper
cd parser-helper
cabal install

if [ -z $(which parser-helper) ]; then
    echo "Could not find parser-helper on the path."
    echo "Current path: $PATH"
    echo "Contents of ~/.cabal/bin"
    ls ~/.cabal/bin
    exit 1
fi

echo "Starting ant build."
ant

#!/bin/bash

set -e

GHC_VER=7.6.3
CABAL_VER=1.20
IDEA_VERSION=13.1.2
IDEA_TAR=ideaIC-${IDEA_VERSION}.tar.gz

echo "Installing ghc $GHC_VER and cabal-install $CABAL_VER"
travis_retry sudo add-apt-repository -y ppa:hvr/ghc
travis_retry sudo apt-get update
travis_retry sudo apt-get install cabal-install-$CABAL_VER ghc-$GHC_VER

export PATH=/opt/ghc/$GHC_VER/bin:/opt/cabal/$CABAL_VER/bin:$PATH

if [ -z $(which cabal) ]; then
    echo "Could not find cabal on the path."
    exit 1
fi

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

echo "Install parser helper."
git clone https://github.com/pjonsson/parser-helper
cd parser-helper
cabal sandbox init
cabal update
cabal install
export PATH=$(pwd)/.cabal-sandbox/bin:$PATH
cd ..

if [ -z $(which parser-helper) ]; then
    echo "Could not find parser-helper on the path."
    exit 1
fi

echo "Starting ant build."
ant

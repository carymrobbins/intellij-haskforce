#!/bin/bash

set -e

echo_loud () {
    echo "$(tput setaf 2 && tput bold)$@$(tput sgr 0)"
}

echo_bad () {
    echo "$(tput setaf 1 && tput bold)$@$(tput sgr 0)"
}

DIR=$(dirname $0)
PROJECT_DIR=$DIR/..
GEN_DIR=$PROJECT_DIR/gen
BNF_FILE=$PROJECT_DIR/src/com/haskforce/Haskell.bnf
FLEX_FILE=$PROJECT_DIR/src/com/haskforce/highlighting/_HaskellSyntaxHighlightingLexer.flex

if ([ ! -f $DIR/.jflex ] || [ ! -f $DIR/.jflex-skeleton ] ||
    [ ! -f $DIR/.lib ] || [ ! -f $DIR/.grammar-kit ]); then
    echo_bad "Must set paths in $DIR/.flex, $DIR/.jflex-skeleton, $DIR/.lib, $DIR/.grammar-kit"
    echo_bad "Note that these paths must be absolute - e.g. use /Users/crobbins instead of ~"
    echo_bad "Something like (but may be different for you):"
    echo "  $ echo /Users/crobbins/build/idea/tools/lexer/jflex-1.4/bin/jflex > $DIR/.jflex"
    echo "  $ echo /Users/crobbins/build/idea/tools/lexer/idea-flex.skeleton > $DIR/.jflex-skeleton"
    echo "  $ echo /Users/crobbins/Library/Application Support/IdeaIC13/GrammarKit/lib/grammar-kit.jar > $DIR/.grammar-kit"
    echo "  $ echo /Applications/IntelliJ IDEA 13 CE.app/lib > $DIR/.lib"
    exit 1
fi

JFLEX="$(cat $DIR/.jflex)"
JFLEX_SKELETON="$(cat $DIR/.jflex-skeleton)"
LIB="$(cat $DIR/.lib)"
GRAMMAR_KIT="$(cat $DIR/.grammar-kit)"

echo_loud "Deleting old gen directory ..."
rm -rf $PROJECT_DIR/gen

echo_loud "Running JFlex ..."
$JFLEX --skel $JFLEX_SKELETON --charat --nobak $FLEX_FILE

echo_loud "Generating parser code from BNF ..."
java -cp "$GRAMMAR_KIT:$LIB/*:$PROJECT_DIR/out/production/HaskForce/com/*" org.intellij.grammar.Main $GEN_DIR $BNF_FILE

echo_loud "Success!"

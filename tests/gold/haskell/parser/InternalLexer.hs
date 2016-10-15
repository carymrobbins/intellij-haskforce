{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Exts.Annotated.InternalLexer
-- Copyright   :  (c) The GHC Team, 1997-2000
--                (c) Niklas Broberg, 2004-2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Lexer for Haskell, with some extensions.
--
-----------------------------------------------------------------------------

-- ToDo: Introduce different tokens for decimal, octal and hexadecimal (?)
-- ToDo: FloatTok should have three parts (integer part, fraction, exponent) (?)
-- ToDo: Use a lexical analyser generator (lx?)

module Language.Haskell.Exts.InternalLexer (Token(..), showToken, lexer, topLexer) where

import Language.Haskell.Exts.ParseMonad
import Language.Haskell.Exts.SrcLoc hiding (loc)
import Language.Haskell.Exts.Comments
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.ExtScheme

import Prelude hiding (id, exponent)
import Data.Char
import Data.Ratio
import Data.List (intercalate, isPrefixOf)
import Control.Monad (when)

-- import Debug.Trace (trace)

data Token
        = VarId String
        | QVarId (String,String)
        | IDupVarId (String)        -- duplicable implicit parameter
        | ILinVarId (String)        -- linear implicit parameter
        | ConId String
        | QConId (String,String)
        | DVarId [String]       -- to enable varid's with '-' in them
        | VarSym String
        | ConSym String
        | QVarSym (String,String)
        | QConSym (String,String)
        | IntTok (Integer, String)
        | FloatTok (Rational, String)
        | Character (Char, String)
        | StringTok (String, String)
        | IntTokHash (Integer, String)        -- 1#
        | WordTokHash (Integer, String)       -- 1##
        | FloatTokHash (Rational, String)     -- 1.0#
        | DoubleTokHash (Rational, String)    -- 1.0##
        | CharacterHash (Char, String)        -- c#
        | StringHash (String, String)         -- "Hello world!"#

-- Symbols

        | LeftParen
        | RightParen
        | LeftHashParen
        | RightHashParen
        | SemiColon
        | LeftCurly
        | RightCurly
        | VRightCurly           -- a virtual close brace
        | LeftSquare
        | RightSquare
        | ParArrayLeftSquare -- [:
        | ParArrayRightSquare -- :]
        | Comma
        | Underscore
        | BackQuote

-- Reserved operators

        | Dot           -- reserved for use with 'forall x . x'
        | DotDot
        | Colon
        | QuoteColon
        | DoubleColon
        | Equals
        | Backslash
        | Bar
        | LeftArrow
        | RightArrow
        | At
        | Tilde
        | DoubleArrow
        | Minus
        | Exclamation
        | Star
        | LeftArrowTail         -- >-
        | RightArrowTail        -- -<
        | LeftDblArrowTail      -- >>-
        | RightDblArrowTail     -- -<<

-- Template Haskell
        | THExpQuote            -- [| or [e|
        | THPatQuote            -- [p|
        | THDecQuote            -- [d|
        | THTypQuote            -- [t|
        | THCloseQuote          -- |]
        | THIdEscape (String)   -- dollar x
        | THParenEscape         -- dollar (
        | THVarQuote            -- 'x (but without the x)
        | THTyQuote             -- ''T (but without the T)
        | THQuasiQuote (String,String)  -- [$...|...]

-- HaRP
        | RPGuardOpen       -- (|
        | RPGuardClose      -- |)
        | RPCAt             -- @:

-- Hsx
        | XCodeTagOpen      -- <%
        | XCodeTagClose     -- %>
        | XStdTagOpen       -- <
        | XStdTagClose      -- >
        | XCloseTagOpen     -- </
        | XEmptyTagClose    -- />
        | XChildTagOpen     -- <%> (note that close doesn't exist, it's XCloseTagOpen followed by XCodeTagClose)
        | XPCDATA String
        | XRPatOpen             -- <[
        | XRPatClose            -- ]>

-- Pragmas

        | PragmaEnd                     -- #-}
        | RULES
        | INLINE Bool
        | INLINE_CONLIKE
        | SPECIALISE
        | SPECIALISE_INLINE Bool
        | SOURCE
        | DEPRECATED
        | WARNING
        | SCC
        | GENERATED
        | CORE
        | UNPACK
        | OPTIONS (Maybe String,String)
--        | CFILES  String
--        | INCLUDE String
        | LANGUAGE
        | ANN
        | MINIMAL
        | NO_OVERLAP
        | OVERLAP
        | INCOHERENT

-- Reserved Ids

        | KW_As
        | KW_By         -- transform list comprehensions
        | KW_Case
        | KW_Class
        | KW_Data
        | KW_Default
        | KW_Deriving
        | KW_Do
        | KW_MDo
        | KW_Else
        | KW_Family     -- indexed type families
        | KW_Forall     -- universal/existential types
        | KW_Group      -- transform list comprehensions
        | KW_Hiding
        | KW_If
        | KW_Import
        | KW_In
        | KW_Infix
        | KW_InfixL
        | KW_InfixR
        | KW_Instance
        | KW_Let
        | KW_Module
        | KW_NewType
        | KW_Of
        | KW_Proc       -- arrows
        | KW_Rec        -- arrows
        | KW_Then
        | KW_Type
        | KW_Using      -- transform list comprehensions
        | KW_Where
        | KW_Qualified

                -- FFI
        | KW_Foreign
        | KW_Export
        | KW_Safe
        | KW_Unsafe
        | KW_Threadsafe
        | KW_Interruptible
        | KW_StdCall
        | KW_CCall
        | KW_CPlusPlus
        | KW_DotNet
        | KW_Jvm
        | KW_Js
        | KW_CApi

        | EOF
        deriving (Eq,Show)

reserved_ops :: [(String,(Token, Maybe ExtScheme))]
reserved_ops = [
 ( "..", (DotDot,       Nothing) ),
 ( ":",  (Colon,        Nothing) ),
 ( "::", (DoubleColon,  Nothing) ),
 ( "=",  (Equals,       Nothing) ),
 ( "\\", (Backslash,    Nothing) ),
 ( "|",  (Bar,          Nothing) ),
 ( "<-", (LeftArrow,    Nothing) ),
 ( "->", (RightArrow,   Nothing) ),
 ( "@",  (At,           Nothing) ),
 ( "@:", (RPCAt,        Just (Any [RegularPatterns])) ),
 ( "~",  (Tilde,        Nothing) ),
 ( "=>", (DoubleArrow,  Nothing) ),
 ( "*",  (Star,         Just (Any [KindSignatures])) ),
 -- Parallel arrays
 ( "[:", (ParArrayLeftSquare,   Just (Any [ParallelArrays])) ),
 ( ":]", (ParArrayRightSquare,  Just (Any [ParallelArrays])) ),
 -- Arrows notation
 ( "-<",  (LeftArrowTail,       Just (Any [Arrows])) ),
 ( ">-",  (RightArrowTail,      Just (Any [Arrows])) ),
 ( "-<<", (LeftDblArrowTail,    Just (Any [Arrows])) ),
 ( ">>-", (RightDblArrowTail,   Just (Any [Arrows])) ),
 -- Unicode notation
 ( "\x2190",    (LeftArrow,     Just (Any  [UnicodeSyntax])) ),
 ( "\x2192",    (RightArrow,    Just (Any  [UnicodeSyntax])) ),
 ( "\x21d2",    (DoubleArrow,   Just (Any  [UnicodeSyntax])) ),
 ( "\x2237",    (DoubleColon,   Just (Any  [UnicodeSyntax])) ),
 ( "\x2919",    (LeftArrowTail,     Just (All [UnicodeSyntax, Arrows])) ),
 ( "\x291a",    (RightArrowTail,    Just (All [UnicodeSyntax, Arrows])) ),
 ( "\x291b",    (LeftDblArrowTail,  Just (All [UnicodeSyntax, Arrows])) ),
 ( "\x291c",    (RightDblArrowTail, Just (All [UnicodeSyntax, Arrows])) ),
 ( "\x2605",    (Star,              Just (All [UnicodeSyntax, KindSignatures])) ),
 ( "\x2200",    (KW_Forall,         Just (All [UnicodeSyntax, ExplicitForAll])) )
 ]

special_varops :: [(String,(Token, Maybe ExtScheme))]
special_varops = [
 -- the dot is only a special symbol together with forall, but can still be used as function composition
 ( ".",  (Dot,          Just (Any [ExplicitForAll, ExistentialQuantification])) ),
 ( "-",  (Minus,        Nothing) ),
 ( "!",  (Exclamation,  Nothing) )
 ]

reserved_ids :: [(String,(Token, Maybe ExtScheme))]
reserved_ids = [
 ( "_",         (Underscore,    Nothing) ),
 ( "by",        (KW_By,         Just (Any [TransformListComp])) ),
 ( "case",      (KW_Case,       Nothing) ),
 ( "class",     (KW_Class,      Nothing) ),
 ( "data",      (KW_Data,       Nothing) ),
 ( "default",   (KW_Default,    Nothing) ),
 ( "deriving",  (KW_Deriving,   Nothing) ),
 ( "do",        (KW_Do,         Nothing) ),
 ( "else",      (KW_Else,       Nothing) ),
 ( "family",    (KW_Family,     Just (Any [TypeFamilies])) ),        -- indexed type families
 ( "forall",    (KW_Forall,     Just (Any [ExplicitForAll, ExistentialQuantification])) ),    -- universal/existential quantification
 ( "group",     (KW_Group,      Just (Any [TransformListComp])) ),
 ( "if",        (KW_If,         Nothing) ),
 ( "import",    (KW_Import,     Nothing) ),
 ( "in",        (KW_In,         Nothing) ),
 ( "infix",     (KW_Infix,      Nothing) ),
 ( "infixl",    (KW_InfixL,     Nothing) ),
 ( "infixr",    (KW_InfixR,     Nothing) ),
 ( "instance",  (KW_Instance,   Nothing) ),
 ( "let",       (KW_Let,        Nothing) ),
 ( "mdo",       (KW_MDo,        Just (Any [RecursiveDo])) ),
 ( "module",    (KW_Module,     Nothing) ),
 ( "newtype",   (KW_NewType,    Nothing) ),
 ( "of",        (KW_Of,         Nothing) ),
 ( "proc",      (KW_Proc,       Just (Any [Arrows])) ),
 ( "rec",       (KW_Rec,        Just (Any [Arrows, RecursiveDo, DoRec])) ),
 ( "then",      (KW_Then,       Nothing) ),
 ( "type",      (KW_Type,       Nothing) ),
 ( "using",     (KW_Using,      Just (Any [TransformListComp])) ),
 ( "where",     (KW_Where,      Nothing) ),

-- FFI
 ( "foreign",   (KW_Foreign,    Just (Any [ForeignFunctionInterface])) )
 ]


special_varids :: [(String,(Token, Maybe ExtScheme))]
special_varids = [
 ( "as",        (KW_As,         Nothing) ),
 ( "qualified", (KW_Qualified,  Nothing) ),
 ( "hiding",    (KW_Hiding,     Nothing) ),

-- FFI
 ( "export",        (KW_Export,        Just (Any [ForeignFunctionInterface])) ),
 ( "safe",          (KW_Safe,          Just (Any [ForeignFunctionInterface, SafeImports, Safe, Trustworthy])) ),
 ( "unsafe",        (KW_Unsafe,        Just (Any [ForeignFunctionInterface])) ),
 ( "threadsafe",    (KW_Threadsafe,    Just (Any [ForeignFunctionInterface])) ),
 ( "interruptible", (KW_Interruptible, Just (Any [InterruptibleFFI])) ),
 ( "stdcall",       (KW_StdCall,       Just (Any [ForeignFunctionInterface])) ),
 ( "ccall",         (KW_CCall,         Just (Any [ForeignFunctionInterface])) ),
 ( "cplusplus",     (KW_CPlusPlus,     Just (Any [ForeignFunctionInterface])) ),
 ( "dotnet",        (KW_DotNet,        Just (Any [ForeignFunctionInterface])) ),
 ( "jvm",           (KW_Jvm,           Just (Any [ForeignFunctionInterface])) ),
 ( "js",            (KW_Js,            Just (Any [ForeignFunctionInterface])) ),
 ( "capi",          (KW_CApi,          Just (Any [CApiFFI])) )
 ]

pragmas :: [(String,Token)]
pragmas = [
 ( "rules",             RULES           ),
 ( "inline",            INLINE True     ),
 ( "noinline",          INLINE False    ),
 ( "notinline",         INLINE False    ),
 ( "specialise",        SPECIALISE      ),
 ( "specialize",        SPECIALISE      ),
 ( "source",            SOURCE          ),
 ( "deprecated",        DEPRECATED      ),
 ( "warning",           WARNING         ),
 ( "ann",               ANN             ),
 ( "scc",               SCC             ),
 ( "generated",         GENERATED       ),
 ( "core",              CORE            ),
 ( "unpack",            UNPACK          ),
 ( "language",          LANGUAGE        ),
 ( "minimal",           MINIMAL         ),
 ( "no_overlap",        NO_OVERLAP      ),
 ( "overlap",           OVERLAP         ),
 ( "incoherent",        INCOHERENT      ),
 ( "options",           OPTIONS undefined ) -- we'll tweak it before use - promise!
-- ( "cfiles",            CFILES  undefined ), -- same here...
-- ( "include",           INCLUDE undefined )  -- ...and here!
 ]

isIdent, isHSymbol, isPragmaChar :: Char -> Bool
isIdent   c = isAlpha c || isDigit c || c == '\'' || c == '_'

isHSymbol c = c `elem` ":!#%&*./?@\\-" || ((isSymbol c || isPunctuation c) && not (c `elem` "(),;[]`{}_\"'"))

isPragmaChar c = isAlphaNum c || c == '_'

matchChar :: Char -> String -> Lex a ()
matchChar c msg = do
    s <- getInput
    if null s || head s /= c then fail msg else discard 1

-- The top-level lexer.
-- We need to know whether we are at the beginning of the line to decide
-- whether to insert layout tokens.

lexer :: (Loc Token -> P a) -> P a
lexer = runL topLexer

topLexer :: Lex a (Loc Token)
topLexer = do
    b <- pullCtxtFlag
    if b then -- trace (show cf ++ ": " ++ show VRightCurly) $
              -- the lex context state flags that we must do an empty {} - UGLY
              setBOL >> getSrcLocL >>= \l -> return (Loc (mkSrcSpan l l) VRightCurly)
     else do
        bol <- checkBOL
        (bol', ws) <- lexWhiteSpace bol
        -- take care of whitespace in PCDATA
        ec <- getExtContext
        case ec of
         -- if there was no linebreak, and we are lexing PCDATA,
         -- then we want to care about the whitespace.
         -- We don't bother to test for XmlSyntax, since we
         -- couldn't end up in ChildCtxt otherwise.
         Just ChildCtxt | not bol' && ws -> getSrcLocL >>= \l -> return $ Loc (mkSrcSpan l l) $ XPCDATA " "
         _ -> do startToken
                 sl <- getSrcLocL
                 t <- if bol' then lexBOL    -- >>= \t -> trace ("BOL: " ++ show t) (return t)
                              else lexToken  -- >>= \t -> trace (show t) (return t)
                 el <- getSrcLocL
                 return $ Loc (mkSrcSpan sl el) t

lexWhiteSpace :: Bool -> Lex a (Bool, Bool)
lexWhiteSpace bol = do
    s <- getInput
    ignL <- ignoreLinePragmasL
    case s of
        -- If we find a recognised pragma, we don't want to treat it as a comment.
        '{':'-':'#':rest | isRecognisedPragma rest -> return (bol, False)
                         | isLinePragma rest && not ignL -> do
                            (l, fn) <- lexLinePragma
                            setSrcLineL l
                            setLineFilenameL fn
                            lexWhiteSpace True
        '{':'-':_ -> do
            loc <- getSrcLocL
            discard 2
            (bol1, c) <- lexNestedComment bol ""
            loc2 <- getSrcLocL
            pushComment $ Comment True (mkSrcSpan loc loc2) (reverse c)
            (bol2, _) <- lexWhiteSpace bol1
            return (bol2, True)
        '-':'-':s1 | all (== '-') (takeWhile isHSymbol s1) -> do
            loc    <- getSrcLocL
            discard 2
            dashes <- lexWhile (== '-')
            rest   <- lexWhile (/= '\n')
            s' <- getInput
            loc2 <- getSrcLocL
            let com = Comment False (mkSrcSpan loc loc2) $ dashes ++ rest
            case s' of
                [] -> pushComment com >> return (False, True)
                _ -> do
                    pushComment com
                    lexNewline
                    lexWhiteSpace_ True
                    return (True, True)
        '\n':_ -> do
            lexNewline
            lexWhiteSpace_ True
            return (True, True)
        '\t':_ -> do
            lexTab
            (bol', _) <- lexWhiteSpace bol
            return (bol', True)
        c:_ | isSpace c -> do
            discard 1
            (bol', _) <- lexWhiteSpace bol
            return (bol', True)
        _ -> return (bol, False)

-- | lexWhiteSpace without the return value.
lexWhiteSpace_ :: Bool -> Lex a ()
lexWhiteSpace_ bol =  do _ <- lexWhiteSpace bol
                         return ()

isRecognisedPragma, isLinePragma :: String -> Bool
isRecognisedPragma str = let pragma = takeWhile isPragmaChar . dropWhile isSpace $ str
                          in case lookupKnownPragma pragma of
                              Nothing -> False
                              _       -> True

isLinePragma str = let pragma = map toLower . takeWhile isAlphaNum . dropWhile isSpace $ str
                    in case pragma of
                        "line"  -> True
                        _       -> False

lexLinePragma :: Lex a (Int, String)
lexLinePragma = do
    discard 3   -- {-#
    lexWhile_ isSpace
    discard 4   -- LINE
    lexWhile_ isSpace
    i <- lexWhile isDigit
    when (null i) $ fail "Improperly formatted LINE pragma"
    lexWhile_ isSpace
    matchChar '"' "Improperly formatted LINE pragma"
    fn <- lexWhile (/= '"')
    matchChar '"' "Impossible - lexLinePragma"
    lexWhile_ isSpace
    mapM_ (flip matchChar "Improperly formatted LINE pragma") "#-}"
    lexNewline
    return (read i, fn)

lexNestedComment :: Bool -> String -> Lex a (Bool, String)
lexNestedComment bol str = do
    s <- getInput
    case s of
        '-':'}':_ -> discard 2 >> return (bol, str)
        '{':'-':_ -> do
            discard 2
            (bol', c) <- lexNestedComment bol ("-{" ++ str) -- rest of the subcomment
            lexNestedComment bol' ("}-" ++ c  ) -- rest of this comment
        '\t':_    -> lexTab >> lexNestedComment bol ('\t':str)
        '\n':_    -> lexNewline >> lexNestedComment True ('\n':str)
        c:_       -> discard 1 >> lexNestedComment bol (c:str)
        []        -> fail "Unterminated nested comment"

-- When we are lexing the first token of a line, check whether we need to
-- insert virtual semicolons or close braces due to layout.

lexBOL :: Lex a Token
lexBOL = do
    pos <- getOffside
    -- trace ("Off: " ++ (show pos)) $ do
    case pos of
        LT -> do
                -- trace "layout: inserting '}'\n" $
            -- Set col to 0, indicating that we're still at the
            -- beginning of the line, in case we need a semi-colon too.
            -- Also pop the context here, so that we don't insert
            -- another close brace before the parser can pop it.
            setBOL
            popContextL "lexBOL"
            return VRightCurly
        EQ ->
            -- trace "layout: inserting ';'\n" $
            return SemiColon
        GT -> lexToken

lexToken :: Lex a Token
lexToken = do
    ec <- getExtContext
    -- we don't bother to check XmlSyntax since we couldn't
    -- have ended up in a non-Nothing context if it wasn't
    -- enabled.
    case ec of
     Just HarpCtxt     -> lexHarpToken
     Just TagCtxt      -> lexTagCtxt
     Just CloseTagCtxt -> lexCloseTagCtxt
     Just ChildCtxt    -> lexChildCtxt
     Just CodeTagCtxt  -> lexCodeTagCtxt
     _         -> lexStdToken


lexChildCtxt :: Lex a Token
lexChildCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '<':'%':'>':_ -> do discard 3
                            pushExtContextL ChildCtxt
                            return XChildTagOpen
        '<':'%':_ -> do discard 2
                        pushExtContextL CodeTagCtxt
                        return XCodeTagOpen
        '<':'/':_ -> do discard 2
                        popExtContextL "lexChildCtxt"
                        pushExtContextL CloseTagCtxt
                        return XCloseTagOpen
        '<':'[':_ -> do discard 2
                        pushExtContextL HarpCtxt
                        return XRPatOpen
        '<':_     -> do discard 1
                        pushExtContextL TagCtxt
                        return XStdTagOpen
        _     -> lexPCDATA


lexPCDATA :: Lex a Token
lexPCDATA = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        [] -> return EOF
        _  -> case s of
            '\n':_ -> do
                x <- lexNewline >> lexPCDATA
                case x of
                 XPCDATA p -> return $ XPCDATA $ '\n':p
                 EOF -> return EOF
                 _ -> fail $ "lexPCDATA: unexpected token: " ++ show x
            '<':_ -> return $ XPCDATA ""
            _ -> do let pcd = takeWhile (\c -> c `notElem` "<\n") s
                        l = length pcd
                    discard l
                    x <- lexPCDATA
                    case x of
                     XPCDATA pcd' -> return $ XPCDATA $ pcd ++ pcd'
                     EOF -> return EOF
                     _ -> fail $ "lexPCDATA: unexpected token: " ++ show x


lexCodeTagCtxt :: Lex a Token
lexCodeTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '%':'>':_ -> do discard 2
                        popExtContextL "lexCodeTagContext"
                        return XCodeTagClose
        _     -> lexStdToken

lexCloseTagCtxt :: Lex a Token
lexCloseTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '%':'>':_ -> do discard 2
                        popExtContextL "lexCloseTagCtxt"
                        return XCodeTagClose
        '>':_     -> do discard 1
                        popExtContextL "lexCloseTagCtxt"
                        return XStdTagClose
        _     -> lexStdToken

lexTagCtxt :: Lex a Token
lexTagCtxt = do
    -- if we ever end up here, then XmlSyntax must be on.
    s <- getInput
    case s of
        '/':'>':_ -> do discard 2
                        popExtContextL "lexTagCtxt: Empty tag"
                        return XEmptyTagClose
        '>':_     -> do discard 1
                        popExtContextL "lexTagCtxt: Standard tag"
                        pushExtContextL ChildCtxt
                        return XStdTagClose
        _     -> lexStdToken

lexHarpToken :: Lex a Token
lexHarpToken = do
    -- if we ever end up here, then RegularPatterns must be on.
    s <- getInput
    case s of
        ']':'>':_ -> do discard 2
                        popExtContextL "lexHarpToken"
                        return XRPatClose
        _     -> lexStdToken

lexStdToken :: Lex a Token
lexStdToken = do
    s <- getInput
    exts <- getExtensionsL
    let intHash = lexHash IntTok IntTokHash (Right WordTokHash)
    case s of
        [] -> return EOF

        '0':c:d:_ | toLower c == 'o' && isOctDigit d -> do
                        discard 2
                        (n, str) <- lexOctal
                        con <- intHash
                        return (con (n, '0':c:str))
                  | toLower c == 'b' && isBinDigit d && BinaryLiterals `elem` exts -> do
                        discard 2
                        (n, str) <- lexBinary
                        con <- intHash
                        return (con (n, '0':c:str))
                  | toLower c == 'x' && isHexDigit d -> do
                        discard 2
                        (n, str) <- lexHexadecimal
                        con <- intHash
                        return (con (n, '0':c:str))

        -- implicit parameters
        '?':c:_ | isLower c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ IDupVarId id

        '%':c:_ | isLower c && ImplicitParams `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ ILinVarId id
        -- end implicit parameters

        -- harp
--        '(':'|':c:_  | isHSymbol c -> discard 1 >> return LeftParen
        '(':'|':c:_ | RegularPatterns `elem` exts && not (isHSymbol c) ->
                     do discard 2
                        return RPGuardOpen
        '|':')':_ | RegularPatterns `elem` exts ->
                     do discard 2
                        return RPGuardClose
        {- This is handled by the reserved_ops above.
        '@':':':_ | RegularPatterns `elem` exts ->
                     do discard 2
                        return RPCAt -}

        -- template haskell
        '[':'|':_ | TemplateHaskell `elem` exts -> do
                discard 2
                return THExpQuote

        '[':c:'|':_ | c == 'e' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THExpQuote
                    | c == 'p' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THPatQuote
                    | c == 'd' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THDecQuote
                    | c == 't' && TemplateHaskell `elem` exts -> do
                        discard 3
                        return THTypQuote
        '[':'$':c:_ | isLower c && QuasiQuotes `elem` exts ->
                        discard 2 >> lexQuasiQuote c

        '[':c:s' | isLower c && QuasiQuotes `elem` exts && case dropWhile isIdent s' of { '|':_ -> True;_->False} ->
                        discard 1 >> lexQuasiQuote c
                 | isUpper c && QuasiQuotes `elem` exts ->
                        discard 1 >> lexQuasiQuote c

        '|':']':_ | TemplateHaskell `elem` exts -> do
                        discard 2
                        return THCloseQuote

        '$':c:_ | isLower c && TemplateHaskell `elem` exts -> do
                        discard 1
                        id <- lexWhile isIdent
                        return $ THIdEscape id
                | c == '(' && TemplateHaskell `elem` exts -> do
                        discard 2
                        return THParenEscape
        -- end template haskell

        -- hsx
        '<':'%':c:_ | XmlSyntax `elem` exts ->
                        case c of
                         '>' -> do discard 3
                                   pushExtContextL ChildCtxt
                                   return XChildTagOpen
                         _   -> do discard 2
                                   pushExtContextL CodeTagCtxt
                                   return XCodeTagOpen
        '<':c:_ | isAlpha c && XmlSyntax `elem` exts -> do
                        discard 1
                        pushExtContextL TagCtxt
                        return XStdTagOpen
        -- end hsx

        '(':'#':c:_ | UnboxedTuples `elem` exts && not (isHSymbol c) -> discard 2 >> return LeftHashParen

        '#':')':_ | UnboxedTuples `elem` exts -> discard 2 >> return RightHashParen

        -- pragmas

        '{':'-':'#':_ -> saveExtensionsL >> discard 3 >> lexPragmaStart

        '#':'-':'}':_ -> restoreExtensionsL >> discard 3 >> return PragmaEnd

        -- Parallel arrays

        '[':':':_ | ParallelArrays `elem` exts -> discard 2 >> return ParArrayLeftSquare

        ':':']':_ | ParallelArrays `elem` exts -> discard 2 >> return ParArrayRightSquare

        c:_ | isDigit c -> lexDecimalOrFloat

            | isUpper c -> lexConIdOrQual ""

            | isLower c || c == '_' -> do
                    idents <- lexIdents
                    case idents of
                     [ident] -> case lookup ident (reserved_ids ++ special_varids) of
                                 Just (keyword, scheme) ->
                                    -- check if an extension keyword is enabled
                                    if isEnabled scheme exts
                                     then flagKW keyword >> return keyword
                                     else return $ VarId ident
                                 Nothing -> return $ VarId ident
                     _ -> return $ DVarId idents

            | isHSymbol c -> do
                    sym <- lexWhile isHSymbol
                    return $ case lookup sym (reserved_ops ++ special_varops) of
                              Just (t , scheme) ->
                                -- check if an extension op is enabled
                                if isEnabled scheme exts
                                 then t
                                 else case c of
                                        ':' -> ConSym sym
                                        _   -> VarSym sym
                              Nothing -> case c of
                                          ':' -> ConSym sym
                                          _   -> VarSym sym

            | otherwise -> do
                    discard 1
                    case c of

                        -- First the special symbols
                        '(' ->  return LeftParen
                        ')' ->  return RightParen
                        ',' ->  return Comma
                        ';' ->  return SemiColon
                        '[' ->  return LeftSquare
                        ']' ->  return RightSquare
                        '`' ->  return BackQuote
                        '{' -> do
                            pushContextL NoLayout
                            return LeftCurly
                        '}' -> do
                            popContextL "lexStdToken"
                            return RightCurly

                        '\'' -> lexCharacter
                        '"' ->  lexString

                        _ ->    fail ("Illegal character \'" ++ show c ++ "\'\n")

      where lexIdents :: Lex a [String]
            lexIdents = do
                ident <- lexWhile isIdent
                s <- getInput
                exts <- getExtensionsL
                case s of
                 -- This is the only way we can get more than one ident in the list
                 -- and it requires XmlSyntax to be on.
                 '-':c:_ | XmlSyntax `elem` exts && isAlpha c -> do
                        discard 1
                        idents <- lexIdents
                        return $ ident : idents
                 '#':_ | MagicHash `elem` exts -> do
                        discard 1
                        return [ident ++ "#"]
                 _ -> return [ident]

            lexQuasiQuote :: Char -> Lex a Token
            lexQuasiQuote c = do
                -- We've seen and dropped [$ already
                ident <- lexQuoter
                matchChar '|' "Malformed quasi-quote quoter"
                body <- lexQQBody
                return $ THQuasiQuote (ident, body)
                  where lexQuoter
                         | isLower c = lexWhile isIdent
                         | otherwise = do
                            qualThing <- lexConIdOrQual ""
                            case qualThing of
                                QVarId (s1,s2) -> return $ s1 ++ '.':s2
                                QVarSym (s1, s2) -> return $ s1 ++ '.':s2
                                _                -> fail "Malformed quasi-quote quoter"

            lexQQBody :: Lex a String
            lexQQBody = do
                s <- getInput
                case s of
                  '\\':']':_ -> do discard 2
                                   str <- lexQQBody
                                   return (']':str)
                  '\\':'|':_ -> do discard 2
                                   str <- lexQQBody
                                   return ('|':str)
                  '|':']':_  -> discard 2 >> return ""
                  '|':_ -> do discard 1
                              str <- lexQQBody
                              return ('|':str)
                  ']':_ -> do discard 1
                              str <- lexQQBody
                              return (']':str)
                  '\\':_ -> do discard 1
                               str <- lexQQBody
                               return ('\\':str)
                  '\n':_ -> do lexNewline
                               str <- lexQQBody
                               return ('\n':str)
                  []     -> fail "Unexpected end of input while lexing quasi-quoter"
                  _ -> do str <- lexWhile (not . (`elem` "\\|\n"))
                          rest <- lexQQBody
                          return (str++rest)

-- Underscores are used in some pragmas. Options pragmas are a special case
-- with our representation: the thing after the underscore is a parameter.
-- Strip off the parameters to option pragmas by hand here, everything else
-- sits in the pragmas map.
lookupKnownPragma :: String -> Maybe Token
lookupKnownPragma s =
    case map toLower s of
      x | "options_" `isPrefixOf` x -> Just $ OPTIONS (Just $ drop 8 s, undefined)
        | "options" == x            -> Just $ OPTIONS (Nothing, undefined)
        | otherwise                 -> lookup x pragmas

lexPragmaStart :: Lex a Token
lexPragmaStart = do
    lexWhile_ isSpace
    pr <- lexWhile isPragmaChar
    case lookupKnownPragma pr of
     Just (INLINE True) -> do
            s <- getInput
            case map toLower s of
             ' ':'c':'o':'n':'l':'i':'k':'e':_  -> do
                      discard 8
                      return INLINE_CONLIKE
             _ -> return $ INLINE True
     Just SPECIALISE -> do
            s <- getInput
            case dropWhile isSpace $ map toLower s of
             'i':'n':'l':'i':'n':'e':_ -> do
                      lexWhile_ isSpace
                      discard 6
                      return $ SPECIALISE_INLINE True
             'n':'o':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile_ isSpace
                        discard 8
                        return $ SPECIALISE_INLINE False
             'n':'o':'t':'i':'n':'l':'i':'n':'e':_ -> do
                        lexWhile_ isSpace
                        discard 9
                        return $ SPECIALISE_INLINE False
             _ -> return SPECIALISE

     Just (OPTIONS opt) ->     -- see, I promised we'd mask out the 'undefined'
            case fst opt of
             Just opt' -> do
                rest <- lexRawPragma
                return $ OPTIONS (Just opt', rest)
             Nothing -> do
                            s <- getInput
                            case s of
                                x:_ | isSpace x -> do
                                    rest <- lexRawPragma
                                    return $ OPTIONS (Nothing, rest)
                                _  -> fail "Malformed Options pragma"
     Just RULES -> do -- Rules enable ScopedTypeVariables locally.
            addExtensionL ScopedTypeVariables
            return RULES
{-     Just (CFILES _) -> do
            rest <- lexRawPragma
            return $ CFILES rest
     Just (INCLUDE _) -> do
            rest <- lexRawPragma
            return $ INCLUDE rest -}
     Just p ->  return p

     _      -> fail "Internal error: Unrecognised recognised pragma"
                  -- do rawStr <- lexRawPragma
                  -- return $ PragmaUnknown (pr, rawStr) -- no support for unrecognized pragmas, treat as comment
                  -- discard 3 -- #-}
                  -- topLexer -- we just discard it as a comment for now and restart -}

lexRawPragma :: Lex a String
lexRawPragma = lexRawPragmaAux
 where lexRawPragmaAux = do
        rpr <- lexWhile (/='#')
        s <- getInput
        case s of
         '#':'-':'}':_  -> return rpr
         "" -> fail "End-of-file inside pragma"
         _ -> do
            discard 1
            rpr' <- lexRawPragma
            return $ rpr ++ '#':rpr'

lexDecimalOrFloat :: Lex a Token
lexDecimalOrFloat = do
    ds <- lexWhile isDigit
    rest <- getInput
    exts <- getExtensionsL
    case rest of
        ('.':d:_) | isDigit d -> do
                discard 1
                frac <- lexWhile isDigit
                let num = parseInteger 10 (ds ++ frac)
                    decimals = toInteger (length frac)
                (exponent, estr) <- do
                    rest2 <- getInput
                    case rest2 of
                        'e':_ -> lexExponent
                        'E':_ -> lexExponent
                        _     -> return (0,"")
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((num%1) * 10^^(exponent - decimals), ds ++ '.':frac ++ estr)
        e:_ | toLower e == 'e' -> do
                (exponent, estr) <- lexExponent
                con <- lexHash FloatTok FloatTokHash (Right DoubleTokHash)
                return $ con ((parseInteger 10 ds%1) * 10^^exponent, ds ++ estr)
        '#':'#':_ | MagicHash `elem` exts -> discard 2 >> return (WordTokHash (parseInteger 10 ds, ds))
        '#':_     | MagicHash `elem` exts -> discard 1 >> return (IntTokHash  (parseInteger 10 ds, ds))
        _         ->              return (IntTok      (parseInteger 10 ds, ds))

    where
    lexExponent :: Lex a (Integer, String)
    lexExponent = do
        (e:r) <- getInput
        discard 1   -- 'e' or 'E'
        case r of
         '+':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (n, e:'+':str)
         '-':d:_ | isDigit d -> do
            discard 1
            (n, str) <- lexDecimal
            return (negate n, e:'-':str)
         d:_ | isDigit d -> lexDecimal >>= \(n,str) -> return (n, e:str)
         _ -> fail "Float with missing exponent"

lexHash :: (b -> Token) -> (b -> Token) -> Either String (b -> Token) -> Lex a (b -> Token)
lexHash a b c = do
    exts <- getExtensionsL
    if MagicHash `elem` exts
     then do
        r <- getInput
        case r of
         '#':'#':_ -> case c of
                       Right c' -> discard 2 >> return c'
                       Left s  -> fail s
         '#':_     -> discard 1 >> return b
         _         ->              return a
     else return a

lexConIdOrQual :: String -> Lex a Token
lexConIdOrQual qual = do
        con <- lexWhile isIdent
        let conid | null qual = ConId con
                  | otherwise = QConId (qual,con)
            qual' | null qual = con
                  | otherwise = qual ++ '.':con
        just_a_conid <- alternative (return conid)
        rest <- getInput
        exts <- getExtensionsL
        case rest of
          '.':c:_
             | isLower c || c == '_' -> do  -- qualified varid?
                    discard 1
                    ident <- lexWhile isIdent
                    s <- getInput
                    exts' <- getExtensionsL
                    ident' <- case s of
                               '#':_ | MagicHash `elem` exts' -> discard 1 >> return (ident ++ "#")
                               _ -> return ident
                    case lookup ident' reserved_ids of
                       -- cannot qualify a reserved word
                       Just (_,scheme) | isEnabled scheme exts'  -> just_a_conid
                       _ -> return (QVarId (qual', ident'))

             | isUpper c -> do      -- qualified conid?
                    discard 1
                    lexConIdOrQual qual'

             | isHSymbol c -> do    -- qualified symbol?
                    discard 1
                    sym <- lexWhile isHSymbol
                    exts' <- getExtensionsL
                    case lookup sym reserved_ops of
                        -- cannot qualify a reserved operator
                        Just (_,scheme) | isEnabled scheme exts' -> just_a_conid
                        _        -> return $ case c of
                                              ':' -> QConSym (qual', sym)
                                              _   -> QVarSym (qual', sym)

          '#':cs
            | null cs ||
              not (isHSymbol $ head cs) &&
              not (isIdent $ head cs) && MagicHash `elem` exts -> do
                discard 1
                case conid of
                 ConId con' -> return $ ConId $ con' ++ "#"
                 QConId (q,con') -> return $ QConId (q,con' ++ "#")
                 _ -> fail $ "lexConIdOrQual: unexpected token: " ++ show conid
          _ ->  return conid -- not a qualified thing

lexCharacter :: Lex a Token
lexCharacter = do   -- We need to keep track of not only character constants but also TH 'x and ''T
        -- We've seen ' so far
        s <- getInput
        exts <- getExtensionsL
        case s of
         '\'':_ | TemplateHaskell `elem` exts -> discard 1 >> return THTyQuote
         '\\':_ -> do
                    (c,raw) <- lexEscape
                    matchQuote
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, '\\':raw))
         c:'\'':_ -> do
                    discard 2
                    con <- lexHash Character CharacterHash
                            (Left "Double hash not available for character literals")
                    return (con (c, [c]))
         _ | any (`elem` exts) [TemplateHaskell, DataKinds] -> return THVarQuote
         _ -> fail "Improper character constant or misplaced \'"

    where matchQuote = matchChar '\'' "Improperly terminated character constant"


lexString :: Lex a Token
lexString = loop ("","")
    where
    loop (s,raw) = do
        r <- getInput
        exts <- getExtensionsL
        case r of
            '\\':'&':_ -> do
                    discard 2
                    loop (s, '&':'\\':raw)
            '\\':c:_ | isSpace c -> do
                        discard 1
                        wcs <- lexWhiteChars
                        matchChar '\\' "Illegal character in string gap"
                        loop (s, '\\':reverse wcs ++ '\\':raw)
                     | otherwise -> do
                        (ce, str) <- lexEscape
                        loop (ce:s, reverse str ++ '\\':raw)
            '"':'#':_ | MagicHash `elem` exts -> do
                        discard 2
                        return (StringHash (reverse s, reverse raw))
            '"':_ -> do
                discard 1
                return (StringTok (reverse s, reverse raw))
            c:_ | c /= '\n' -> do
                discard 1
                loop (c:s, c:raw)
            _ ->   fail "Improperly terminated string"

    lexWhiteChars :: Lex a String
    lexWhiteChars = do
        s <- getInput
        case s of
            '\n':_ -> do
                    lexNewline
                    wcs <- lexWhiteChars
                    return $ '\n':wcs
            '\t':_ -> do
                    lexTab
                    wcs <- lexWhiteChars
                    return $ '\t':wcs
            c:_ | isSpace c -> do
                    discard 1
                    wcs <- lexWhiteChars
                    return $ c:wcs
            _ -> return ""

lexEscape :: Lex a (Char, String)
lexEscape = do
    discard 1
    r <- getInput
    case r of

-- Production charesc from section B.2 (Note: \& is handled by caller)

        'a':_           -> discard 1 >> return ('\a', "a")
        'b':_           -> discard 1 >> return ('\b', "b")
        'f':_           -> discard 1 >> return ('\f', "f")
        'n':_           -> discard 1 >> return ('\n', "n")
        'r':_           -> discard 1 >> return ('\r', "r")
        't':_           -> discard 1 >> return ('\t', "t")
        'v':_           -> discard 1 >> return ('\v', "v")
        '\\':_          -> discard 1 >> return ('\\', "\\")
        '"':_           -> discard 1 >> return ('\"', "\"")
        '\'':_          -> discard 1 >> return ('\'', "\'")

-- Production ascii from section B.2

        '^':c:_         -> discard 2 >> cntrl c
        'N':'U':'L':_   -> discard 3 >> return ('\NUL', "NUL")
        'S':'O':'H':_   -> discard 3 >> return ('\SOH', "SOH")
        'S':'T':'X':_   -> discard 3 >> return ('\STX', "STX")
        'E':'T':'X':_   -> discard 3 >> return ('\ETX', "ETX")
        'E':'O':'T':_   -> discard 3 >> return ('\EOT', "EOT")
        'E':'N':'Q':_   -> discard 3 >> return ('\ENQ', "ENQ")
        'A':'C':'K':_   -> discard 3 >> return ('\ACK', "ACK")
        'B':'E':'L':_   -> discard 3 >> return ('\BEL', "BEL")
        'B':'S':_       -> discard 2 >> return ('\BS',  "BS")
        'H':'T':_       -> discard 2 >> return ('\HT',  "HT")
        'L':'F':_       -> discard 2 >> return ('\LF',  "LF")
        'V':'T':_       -> discard 2 >> return ('\VT',  "VT")
        'F':'F':_       -> discard 2 >> return ('\FF',  "FF")
        'C':'R':_       -> discard 2 >> return ('\CR',  "CR")
        'S':'O':_       -> discard 2 >> return ('\SO',  "SO")
        'S':'I':_       -> discard 2 >> return ('\SI',  "SI")
        'D':'L':'E':_   -> discard 3 >> return ('\DLE', "DLE")
        'D':'C':'1':_   -> discard 3 >> return ('\DC1', "DC1")
        'D':'C':'2':_   -> discard 3 >> return ('\DC2', "DC2")
        'D':'C':'3':_   -> discard 3 >> return ('\DC3', "DC3")
        'D':'C':'4':_   -> discard 3 >> return ('\DC4', "DC4")
        'N':'A':'K':_   -> discard 3 >> return ('\NAK', "NAK")
        'S':'Y':'N':_   -> discard 3 >> return ('\SYN', "SYN")
        'E':'T':'B':_   -> discard 3 >> return ('\ETB', "ETB")
        'C':'A':'N':_   -> discard 3 >> return ('\CAN', "CAN")
        'E':'M':_       -> discard 2 >> return ('\EM',  "EM")
        'S':'U':'B':_   -> discard 3 >> return ('\SUB', "SUB")
        'E':'S':'C':_   -> discard 3 >> return ('\ESC', "ESC")
        'F':'S':_       -> discard 2 >> return ('\FS',  "FS")
        'G':'S':_       -> discard 2 >> return ('\GS',  "GS")
        'R':'S':_       -> discard 2 >> return ('\RS',  "RS")
        'U':'S':_       -> discard 2 >> return ('\US',  "US")
        'S':'P':_       -> discard 2 >> return ('\SP',  "SP")
        'D':'E':'L':_   -> discard 3 >> return ('\DEL', "DEL")

-- Escaped numbers

        'o':c:_ | isOctDigit c -> do
                    discard 1
                    (n, raw) <- lexOctal
                    n' <- checkChar n
                    return (n', 'o':raw)
        'x':c:_ | isHexDigit c -> do
                    discard 1
                    (n, raw) <- lexHexadecimal
                    n' <- checkChar n
                    return (n', 'x':raw)
        c:_ | isDigit c -> do
                    (n, raw) <- lexDecimal
                    n' <- checkChar n
                    return (n', raw)

        _       -> fail "Illegal escape sequence"

    where
    checkChar n | n <= 0x10FFFF = return (chr (fromInteger n))
    checkChar _                 = fail "Character constant out of range"

-- Production cntrl from section B.2

    cntrl :: Char -> Lex a (Char, String)
    cntrl c | c >= '@' && c <= '_' = return (chr (ord c - ord '@'), '^':c:[])
    cntrl _                        = fail "Illegal control character"

-- assumes at least one octal digit
lexOctal :: Lex a (Integer, String)
lexOctal = do
    ds <- lexWhile isOctDigit
    return (parseInteger 8 ds, ds)

-- assumes at least one binary digit
lexBinary :: Lex a (Integer, String)
lexBinary = do
    ds <- lexWhile isBinDigit
    return (parseInteger 2 ds, ds)

-- assumes at least one hexadecimal digit
lexHexadecimal :: Lex a (Integer, String)
lexHexadecimal = do
    ds <- lexWhile isHexDigit
    return (parseInteger 16 ds, ds)

-- assumes at least one decimal digit
lexDecimal :: Lex a (Integer, String)
lexDecimal = do
    ds <- lexWhile isDigit
    return (parseInteger 10 ds, ds)

-- Stolen from Hugs's Prelude
parseInteger :: Integer -> String -> Integer
parseInteger radix ds =
    foldl1 (\n d -> n * radix + d) (map (toInteger . digitToInt) ds)

flagKW :: Token -> Lex a ()
flagKW t =
  when (t `elem` [KW_Do, KW_MDo]) $ do
       exts <- getExtensionsL
       when (NondecreasingIndentation `elem` exts) flagDo

-- | Selects ASCII binary digits, i.e. @\'0\'@..@\'1\'@.
isBinDigit :: Char -> Bool
isBinDigit c =  c >= '0' && c <= '1'
------------------------------------------------------------------
-- "Pretty" printing for tokens

showToken :: Token -> String
showToken t = case t of
  VarId s           -> s
  QVarId (q,s)      -> q ++ '.':s
  IDupVarId s       -> '?':s
  ILinVarId s       -> '%':s
  ConId s           -> s
  QConId (q,s)      -> q ++ '.':s
  DVarId ss         -> intercalate "-" ss
  VarSym s          -> s
  ConSym s          -> s
  QVarSym (q,s)     -> q ++ '.':s
  QConSym (q,s)     -> q ++ '.':s
  IntTok (_, s)         -> s
  FloatTok (_, s)       -> s
  Character (_, s)      -> '\'':s ++ "'"
  StringTok (_, s)      -> '"':s ++ "\""
  IntTokHash (_, s)     -> s ++ "#"
  WordTokHash (_, s)    -> s ++ "##"
  FloatTokHash (_, s)   -> s ++ "#"
  DoubleTokHash (_, s)  -> s ++ "##"
  CharacterHash (_, s)  -> '\'':s ++ "'#"
  StringHash (_, s)     -> '"':s ++ "\"#"
  LeftParen         -> "("
  RightParen        -> ")"
  LeftHashParen     -> "(#"
  RightHashParen    -> "#)"
  SemiColon         -> ";"
  LeftCurly         -> "{"
  RightCurly        -> "}"
  VRightCurly       -> "virtual }"
  LeftSquare        -> "["
  RightSquare       -> "]"
  ParArrayLeftSquare -> "[:"
  ParArrayRightSquare -> ":]"
  Comma             -> ","
  Underscore        -> "_"
  BackQuote         -> "`"
  QuoteColon        -> "':"
  Dot               -> "."
  DotDot            -> ".."
  Colon             -> ":"
  DoubleColon       -> "::"
  Equals            -> "="
  Backslash         -> "\\"
  Bar               -> "|"
  LeftArrow         -> "<-"
  RightArrow        -> "->"
  At                -> "@"
  Tilde             -> "~"
  DoubleArrow       -> "=>"
  Minus             -> "-"
  Exclamation       -> "!"
  Star              -> "*"
  LeftArrowTail     -> ">-"
  RightArrowTail    -> "-<"
  LeftDblArrowTail  -> ">>-"
  RightDblArrowTail -> "-<<"
  THExpQuote        -> "[|"
  THPatQuote        -> "[p|"
  THDecQuote        -> "[d|"
  THTypQuote        -> "[t|"
  THCloseQuote      -> "|]"
  THIdEscape s      -> '$':s
  THParenEscape     -> "$("
  THVarQuote        -> "'"
  THTyQuote         -> "''"
  THQuasiQuote (n,q) -> "[$" ++ n ++ "|" ++ q ++ "]"
  RPGuardOpen       -> "(|"
  RPGuardClose      -> "|)"
  RPCAt             -> "@:"
  XCodeTagOpen      -> "<%"
  XCodeTagClose     -> "%>"
  XStdTagOpen       -> "<"
  XStdTagClose      -> ">"
  XCloseTagOpen     -> "</"
  XEmptyTagClose    -> "/>"
  XPCDATA s         -> "PCDATA " ++ s
  XRPatOpen         -> "<["
  XRPatClose        -> "]>"
  PragmaEnd         -> "#-}"
  RULES             -> "{-# RULES"
  INLINE b          -> "{-# " ++ if b then "INLINE" else "NOINLINE"
  INLINE_CONLIKE    -> "{-# " ++ "INLINE CONLIKE"
  SPECIALISE        -> "{-# SPECIALISE"
  SPECIALISE_INLINE b -> "{-# SPECIALISE " ++ if b then "INLINE" else "NOINLINE"
  SOURCE            -> "{-# SOURCE"
  DEPRECATED        -> "{-# DEPRECATED"
  WARNING           -> "{-# WARNING"
  SCC               -> "{-# SCC"
  GENERATED         -> "{-# GENERATED"
  CORE              -> "{-# CORE"
  UNPACK            -> "{-# UNPACK"
  OPTIONS (mt,_)    -> "{-# OPTIONS" ++ maybe "" (':':) mt ++ " ..."
--  CFILES  s         -> "{-# CFILES ..."
--  INCLUDE s         -> "{-# INCLUDE ..."
  LANGUAGE          -> "{-# LANGUAGE"
  ANN               -> "{-# ANN"
  MINIMAL           -> "{-# MINIMAL"
  NO_OVERLAP        -> "{-# NO_OVERLAP"
  OVERLAP           -> "{-# OVERLAP"
  INCOHERENT        -> "{-# INCOHERENT"
  KW_As         -> "as"
  KW_By         -> "by"
  KW_Case       -> "case"
  KW_Class      -> "class"
  KW_Data       -> "data"
  KW_Default    -> "default"
  KW_Deriving   -> "deriving"
  KW_Do         -> "do"
  KW_MDo        -> "mdo"
  KW_Else       -> "else"
  KW_Family     -> "family"
  KW_Forall     -> "forall"
  KW_Group      -> "group"
  KW_Hiding     -> "hiding"
  KW_If         -> "if"
  KW_Import     -> "import"
  KW_In         -> "in"
  KW_Infix      -> "infix"
  KW_InfixL     -> "infixl"
  KW_InfixR     -> "infixr"
  KW_Instance   -> "instance"
  KW_Let        -> "let"
  KW_Module     -> "module"
  KW_NewType    -> "newtype"
  KW_Of         -> "of"
  KW_Proc       -> "proc"
  KW_Rec        -> "rec"
  KW_Then       -> "then"
  KW_Type       -> "type"
  KW_Using      -> "using"
  KW_Where      -> "where"
  KW_Qualified  -> "qualified"
  KW_Foreign    -> "foreign"
  KW_Export     -> "export"
  KW_Safe       -> "safe"
  KW_Unsafe     -> "unsafe"
  KW_Threadsafe -> "threadsafe"
  KW_Interruptible -> "interruptible"
  KW_StdCall    -> "stdcall"
  KW_CCall      -> "ccall"
  XChildTagOpen -> "<%>"
  KW_CPlusPlus  -> "cplusplus"
  KW_DotNet     -> "dotnet"
  KW_Jvm        -> "jvm"
  KW_Js         -> "js"
  KW_CApi       -> "capi"

  EOF           -> "EOF"

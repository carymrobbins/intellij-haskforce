package com.haskforce.tooling.ghcPkg

import java.io.{FileInputStream, InputStream}

import com.haskforce.test.AssertMixin
import com.intellij.testFramework.UsefulTestCase

class GhcPkgDumpParserTest extends UsefulTestCase with AssertMixin {

  private def testName: String = getTestName(false)

  protected def fixtureInputStream(): InputStream = {
    new FileInputStream(s"tests/gold/codeInsight/ghc-pkg-dump/$testName.txt")
  }

  def test00001(): Unit = {
    val is = fixtureInputStream()
    val pkgs = CachedPkgs.fromIterator(GhcPkgDumpParser.parse(is).iterator)
    assertPkgNames(pkgs, """
      generic-override generic-override-aeson aeson-pretty aeson vector
      attoparsec cmdargs scientific time-compat th-abstraction base-compat
      uuid-types tagged integer-logarithms dlist base-orphans
      unordered-containers hashable random primitive text stm process filepath
      pretty mtl ghc-prim unix rts hpc terminfo bytestring ghc array base
      containers ghc-compact ghc-boot haskeline time xhtml Cabal parsec
      integer-gmp deepseq ghc-boot-th binary transformers ghc-heap libiserv
      template-haskell directory ghci
    """)
    assertExposedModules(pkgs, "generic-override", "0.0.0.1", """
      Data.Override Data.Override.Internal
    """)
    assertExposedModules(pkgs, "containers", "0.6.0.1", """
      Data.Containers.ListUtils Data.Graph Data.IntMap
      Data.IntMap.Internal Data.IntMap.Internal.Debug Data.IntMap.Lazy
      Data.IntMap.Merge.Lazy Data.IntMap.Merge.Strict Data.IntMap.Strict
      Data.IntSet Data.IntSet.Internal Data.Map Data.Map.Internal
      Data.Map.Internal.Debug Data.Map.Lazy Data.Map.Merge.Lazy
      Data.Map.Merge.Strict Data.Map.Strict Data.Map.Strict.Internal
      Data.Sequence Data.Sequence.Internal Data.Sequence.Internal.Sorting
      Data.Set Data.Set.Internal Data.Tree
      Utils.Containers.Internal.BitQueue
      Utils.Containers.Internal.BitUtil
      Utils.Containers.Internal.StrictPair
    """)
    assertExposedModules(pkgs, "parsec", "3.1.13.0", """
      Text.Parsec Text.Parsec.ByteString Text.Parsec.ByteString.Lazy
      Text.Parsec.Char Text.Parsec.Combinator Text.Parsec.Error
      Text.Parsec.Expr Text.Parsec.Language Text.Parsec.Perm
      Text.Parsec.Pos Text.Parsec.Prim Text.Parsec.String
      Text.Parsec.Text Text.Parsec.Text.Lazy Text.Parsec.Token
      Text.ParserCombinators.Parsec Text.ParserCombinators.Parsec.Char
      Text.ParserCombinators.Parsec.Combinator
      Text.ParserCombinators.Parsec.Error
      Text.ParserCombinators.Parsec.Expr
      Text.ParserCombinators.Parsec.Language
      Text.ParserCombinators.Parsec.Perm
      Text.ParserCombinators.Parsec.Pos
      Text.ParserCombinators.Parsec.Prim
      Text.ParserCombinators.Parsec.Token
    """)
    assertExposedModules(pkgs, "deepseq", "1.4.4.0", """
      Control.DeepSeq
    """)
    assertExposedModules(pkgs, "base", "4.12.0.0", """
      Control.Applicative Control.Arrow Control.Category
      Control.Concurrent Control.Concurrent.Chan Control.Concurrent.MVar
      Control.Concurrent.QSem Control.Concurrent.QSemN Control.Exception
      Control.Exception.Base Control.Monad Control.Monad.Fail
      Control.Monad.Fix Control.Monad.IO.Class Control.Monad.Instances
      Control.Monad.ST Control.Monad.ST.Lazy Control.Monad.ST.Lazy.Safe
      Control.Monad.ST.Lazy.Unsafe Control.Monad.ST.Safe
      Control.Monad.ST.Strict Control.Monad.ST.Unsafe Control.Monad.Zip
      Data.Bifoldable Data.Bifunctor Data.Bitraversable Data.Bits
      Data.Bool Data.Char Data.Coerce Data.Complex Data.Data Data.Dynamic
      Data.Either Data.Eq Data.Fixed Data.Foldable Data.Function
      Data.Functor Data.Functor.Classes Data.Functor.Compose
      Data.Functor.Const Data.Functor.Contravariant Data.Functor.Identity
      Data.Functor.Product Data.Functor.Sum Data.IORef Data.Int Data.Ix
      Data.Kind Data.List Data.List.NonEmpty Data.Maybe Data.Monoid
      Data.Ord Data.Proxy Data.Ratio Data.STRef Data.STRef.Lazy
      Data.STRef.Strict Data.Semigroup Data.String Data.Traversable
      Data.Tuple Data.Type.Bool Data.Type.Coercion Data.Type.Equality
      Data.Typeable Data.Unique Data.Version Data.Void Data.Word
      Debug.Trace Foreign Foreign.C Foreign.C.Error Foreign.C.String
      Foreign.C.Types Foreign.Concurrent Foreign.ForeignPtr
      Foreign.ForeignPtr.Safe Foreign.ForeignPtr.Unsafe Foreign.Marshal
      Foreign.Marshal.Alloc Foreign.Marshal.Array Foreign.Marshal.Error
      Foreign.Marshal.Pool Foreign.Marshal.Safe Foreign.Marshal.Unsafe
      Foreign.Marshal.Utils Foreign.Ptr Foreign.Safe Foreign.StablePtr
      Foreign.Storable GHC.Arr GHC.Base GHC.ByteOrder GHC.Char GHC.Clock
      GHC.Conc GHC.Conc.IO GHC.Conc.Signal GHC.Conc.Sync
      GHC.ConsoleHandler GHC.Constants GHC.Desugar GHC.Enum
      GHC.Environment GHC.Err GHC.Event GHC.Exception GHC.Exception.Type
      GHC.ExecutionStack GHC.ExecutionStack.Internal GHC.Exts
      GHC.Fingerprint GHC.Fingerprint.Type GHC.Float
      GHC.Float.ConversionUtils GHC.Float.RealFracMethods GHC.Foreign
      GHC.ForeignPtr GHC.GHCi GHC.Generics GHC.IO GHC.IO.Buffer
      GHC.IO.BufferedIO GHC.IO.Device GHC.IO.Encoding
      GHC.IO.Encoding.CodePage GHC.IO.Encoding.Failure
      GHC.IO.Encoding.Iconv GHC.IO.Encoding.Latin1 GHC.IO.Encoding.Types
      GHC.IO.Encoding.UTF16 GHC.IO.Encoding.UTF32 GHC.IO.Encoding.UTF8
      GHC.IO.Exception GHC.IO.FD GHC.IO.Handle GHC.IO.Handle.FD
      GHC.IO.Handle.Internals GHC.IO.Handle.Lock GHC.IO.Handle.Text
      GHC.IO.Handle.Types GHC.IO.IOMode GHC.IO.Unsafe GHC.IOArray
      GHC.IORef GHC.Int GHC.List GHC.MVar GHC.Maybe GHC.Natural GHC.Num
      GHC.OldList GHC.OverloadedLabels GHC.Pack GHC.Profiling GHC.Ptr
      GHC.RTS.Flags GHC.Read GHC.Real GHC.Records GHC.ResponseFile GHC.ST
      GHC.STRef GHC.Show GHC.Stable GHC.StableName GHC.Stack
      GHC.Stack.CCS GHC.Stack.Types GHC.StaticPtr GHC.Stats GHC.Storable
      GHC.TopHandler GHC.TypeLits GHC.TypeNats GHC.Unicode GHC.Weak
      GHC.Word Numeric Numeric.Natural Prelude System.CPUTime
      System.Console.GetOpt System.Environment System.Environment.Blank
      System.Exit System.IO System.IO.Error System.IO.Unsafe System.Info
      System.Mem System.Mem.StableName System.Mem.Weak
      System.Posix.Internals System.Posix.Types System.Timeout
      Text.ParserCombinators.ReadP Text.ParserCombinators.ReadPrec
      Text.Printf Text.Read Text.Read.Lex Text.Show Text.Show.Functions
      Type.Reflection Type.Reflection.Unsafe Unsafe.Coerce
    """)
  }

  def test00002(): Unit = {
    val is = fixtureInputStream()
    val pkgs = CachedPkgs.fromIterator(GhcPkgDumpParser.parse(is).iterator)
    assertPkgNames(pkgs, """
      base aeson-qq aeson binary
    """)
    assertExposedModules(pkgs, "base", "4.13.0.0", """
      Control.Applicative Control.Arrow Control.Category
      Control.Concurrent Control.Concurrent.Chan Control.Concurrent.MVar
      Control.Concurrent.QSem Control.Concurrent.QSemN Control.Exception
      Control.Exception.Base Control.Monad Control.Monad.Fail
      Control.Monad.Fix Control.Monad.IO.Class Control.Monad.Instances
      Control.Monad.ST Control.Monad.ST.Lazy Control.Monad.ST.Lazy.Safe
      Control.Monad.ST.Lazy.Unsafe Control.Monad.ST.Safe
      Control.Monad.ST.Strict Control.Monad.ST.Unsafe Control.Monad.Zip
      Data.Bifoldable Data.Bifunctor Data.Bitraversable Data.Bits
      Data.Bool Data.Char Data.Coerce Data.Complex Data.Data Data.Dynamic
      Data.Either Data.Eq Data.Fixed Data.Foldable Data.Function
      Data.Functor Data.Functor.Classes Data.Functor.Compose
      Data.Functor.Const Data.Functor.Contravariant Data.Functor.Identity
      Data.Functor.Product Data.Functor.Sum Data.IORef Data.Int Data.Ix
      Data.Kind Data.List Data.List.NonEmpty Data.Maybe Data.Monoid
      Data.Ord Data.Proxy Data.Ratio Data.STRef Data.STRef.Lazy
      Data.STRef.Strict Data.Semigroup Data.String Data.Traversable
      Data.Tuple Data.Type.Bool Data.Type.Coercion Data.Type.Equality
      Data.Typeable Data.Unique Data.Version Data.Void Data.Word
      Debug.Trace Foreign Foreign.C Foreign.C.Error Foreign.C.String
      Foreign.C.Types Foreign.Concurrent Foreign.ForeignPtr
      Foreign.ForeignPtr.Safe Foreign.ForeignPtr.Unsafe Foreign.Marshal
      Foreign.Marshal.Alloc Foreign.Marshal.Array Foreign.Marshal.Error
      Foreign.Marshal.Pool Foreign.Marshal.Safe Foreign.Marshal.Unsafe
      Foreign.Marshal.Utils Foreign.Ptr Foreign.Safe Foreign.StablePtr
      Foreign.Storable GHC.Arr GHC.Base GHC.ByteOrder GHC.Char GHC.Clock
      GHC.Conc GHC.Conc.IO GHC.Conc.Signal GHC.Conc.Sync
      GHC.ConsoleHandler GHC.Constants GHC.Desugar GHC.Enum
      GHC.Environment GHC.Err GHC.Event GHC.Exception GHC.Exception.Type
      GHC.ExecutionStack GHC.ExecutionStack.Internal GHC.Exts
      GHC.Fingerprint GHC.Fingerprint.Type GHC.Float
      GHC.Float.ConversionUtils GHC.Float.RealFracMethods GHC.Foreign
      GHC.ForeignPtr GHC.GHCi GHC.GHCi.Helpers GHC.Generics GHC.IO
      GHC.IO.Buffer GHC.IO.BufferedIO GHC.IO.Device GHC.IO.Encoding
      GHC.IO.Encoding.CodePage GHC.IO.Encoding.Failure
      GHC.IO.Encoding.Iconv GHC.IO.Encoding.Latin1 GHC.IO.Encoding.Types
      GHC.IO.Encoding.UTF16 GHC.IO.Encoding.UTF32 GHC.IO.Encoding.UTF8
      GHC.IO.Exception GHC.IO.FD GHC.IO.Handle GHC.IO.Handle.FD
      GHC.IO.Handle.Internals GHC.IO.Handle.Lock GHC.IO.Handle.Text
      GHC.IO.Handle.Types GHC.IO.IOMode GHC.IO.Unsafe GHC.IOArray
      GHC.IORef GHC.Int GHC.List GHC.MVar GHC.Maybe GHC.Natural GHC.Num
      GHC.OldList GHC.OverloadedLabels GHC.Pack GHC.Profiling GHC.Ptr
      GHC.RTS.Flags GHC.Read GHC.Real GHC.Records GHC.ResponseFile GHC.ST
      GHC.STRef GHC.Show GHC.Stable GHC.StableName GHC.Stack
      GHC.Stack.CCS GHC.Stack.Types GHC.StaticPtr GHC.Stats GHC.Storable
      GHC.TopHandler GHC.TypeLits GHC.TypeNats GHC.Unicode GHC.Weak
      GHC.Word Numeric Numeric.Natural Prelude System.CPUTime
      System.Console.GetOpt System.Environment System.Environment.Blank
      System.Exit System.IO System.IO.Error System.IO.Unsafe System.Info
      System.Mem System.Mem.StableName System.Mem.Weak
      System.Posix.Internals System.Posix.Types System.Timeout
      Text.ParserCombinators.ReadP Text.ParserCombinators.ReadPrec
      Text.Printf Text.Read Text.Read.Lex Text.Show Text.Show.Functions
      Type.Reflection Type.Reflection.Unsafe Unsafe.Coerce
    """)
    assertExposedModules(pkgs, "aeson-qq", "0.8.3", """
      Data.Aeson.QQ
    """)
    assertExposedModules(pkgs, "aeson", "1.4.7.1", """
      Data.Aeson Data.Aeson.Encode Data.Aeson.Encoding
      Data.Aeson.Encoding.Internal Data.Aeson.Internal
      Data.Aeson.Internal.Time Data.Aeson.Parser
      Data.Aeson.Parser.Internal Data.Aeson.QQ.Simple Data.Aeson.TH
      Data.Aeson.Text Data.Aeson.Types
    """)
  }

  private def words(s: String): Array[String] = s.trim.split("\\s+")

  private def assertPkgNames(
    pkgs: CachedPkgs,
    names: String
  ): Unit = {
    assertEquals(
      words(names).toSet,
      pkgs.toMap.keySet
    )
  }

  private def assertExposedModules(
    pkgs: CachedPkgs,
    name: String,
    version: String,
    exposedModules: String
  ): Unit = {
    assertExposedModules(pkgs, name, version, words(exposedModules).toList)
  }

  private def assertExposedModules(
    pkgs: CachedPkgs,
    name: String,
    version: String,
    exposedModules: List[String]
  ): Unit = {
    assertEquals(
      exposedModules,
      pkgs.named(name).getOrElse {
        throw new AssertionError(s"Package '$name' not found")
      }.versioned(version).getOrElse {
        throw new AssertionError(s"Package '$name' with version '$version' not found")
      }.exposedModules
    )
  }
}

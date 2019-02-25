package com.haskforce.utils.parser.j;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;
import scala.util.Either;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

// TODO: Playing around with a pure Java impl.
@SuppressWarnings("WeakerAccess")
public class PsiParsec<A> {

  public static final PsiParsec<Void> pVoid = pure(null);
  public static final PsiParsec<Boolean> pFalse = pure(false);
  public static final PsiParsec<Boolean> pTrue = pure(true);

  public static <A> PsiParsec<A> mk(Function<PsiBuilder, A> f) {
    return new PsiParsec<>(f);
  }

  private static Void TheVoid = null;

  public static <A> PsiParsec<A> fromSupplier(Supplier<A> s) {
    return mk(_b -> s.get());
  }

  public static <A> PsiParsec<Void> fromRunnable(Runnable r) {
    return mk(_b -> { r.run() ; return TheVoid; });
  }

  public static PsiParsec<Void> fromConsumer(Consumer<PsiBuilder> c) {
    return mk(b -> { c.accept(b) ; return null; });
  }

  public static <A> PsiParsec<A> pure(A a) {
    return new PsiParsec<>(_b -> a);
  }

  public static <A> PsiParsec<A> markStart(PsiParsec<Marked<A>> p) {
    return Internal.mark.flatMap(marker ->
      p.flatMap(res ->
        fromRunnable(() -> res.markResult.matchV(
          done -> marker.done(done.elementType),
          collapse -> marker.collapse(collapse.elementType),
          error -> marker.error(error.message)
        )).as(res.parseResult)
      )
    );
  }

  public static PsiParsec<Marked<Void>> markDone(IElementType el) {
    return pure(Marked(MarkResult.Done(el), TheVoid));
  }

  public static PsiParsec<Marked<Void>> markCollapse(IElementType el) {
    return pure(Marked(MarkResult.Collapse(el), TheVoid));
  }

  public static PsiParsec<Marked<Void>> markError(String message) {
    return pure(Marked(MarkResult.Error(message), TheVoid));
  }

  public static PsiParsec<ASTNode> getTreeBuilt = mk(PsiBuilder::getTreeBuilt);

  public static PsiParsec<Boolean> eof = mk(PsiBuilder::eof);

  public static PsiParsec<Void> advanceLexer = fromConsumer(PsiBuilder::advanceLexer);

  public static PsiParsec<Void> remapAdvance(IElementType el) {
    return fromConsumer(b -> b.remapCurrentToken(el)).then(advanceLexer);
  }

  public static PsiParsec<Optional<IElementType>> getTokenType =
    mk(b -> Optional.ofNullable(b.getTokenType()));

  public static PsiParsec<Void> error(String message) {
    return fromConsumer(b -> b.error(message));
  }

  public static PsiParsec<Optional<IElementType>> lookAhead(int n) {
    return mk(b -> Optional.ofNullable(b.lookAhead(n)));
  }

  public static PsiParsec<Void> withTokenType(Function<IElementType, PsiParsec<Void>> f) {
    return getTokenType.flatMap(oel ->
      oel.map(f).orElse(error("Unexpected end of input"))
    );
  }

  public static PsiParsec<Void> expectTokenAdvance(IElementType el) {
    return withTokenType(el0 ->
      el0 == el ? advanceLexer : error("Expected " + el.toString())
    );
  }

  public static PsiParsec<Void> expectTokenOneOfAdvance(IElementType... els) {
    return withTokenType(el0 ->
      ArrayUtil.contains(el0, els)
        ? advanceLexer
        : error("Expected one of: " + StringUtil.join(els, IElementType::toString, ", "))
    );
  }

  public static PsiParsec<Void> expectAdvance(PsiParsec<Boolean> p, String message) {
    return p.flatMap(b -> error(message).when(!b));
  }

  public static PsiParsec<Boolean> maybeTokenAdvance(IElementType el) {
    return getTokenType.flatMap(oel ->
      oel.filter(el0 -> el0 == el)
        .map(_el -> advanceLexer.then(pTrue))
        .orElse(pFalse)
    );
  }

  public static PsiParsec<Boolean> maybeTokenOneOfAdvance(IElementType... els) {
    return getTokenType.flatMap(oel ->
      oel.filter(el0 -> ArrayUtil.contains(el0, els))
        .map(_el -> advanceLexer.then(pTrue))
        .orElse(pFalse)
    );
  }

  public static PsiParsec<Boolean> maybeAdvance(PsiParsec<Boolean> p) {
    return p.flatMap(b -> advanceLexer.when(b).then(pure(b)));
  }

  public static PsiParsec<Void> advanceWhile(PsiParsec<Boolean> p, PsiParsec<Void> b) {
    b.flatMap()
  }

  public static PsiParsec<Void> pwhile(PsiParsec<Boolean> p, PsiParsec<Void> b) {
    p.flatMap(r ->
    );
  }

  public static <A, B> PsiParsec<B> tailRecM(Function<A, PsiParsec<Either<A, B>>> f) {
    // TODO
  }

  public static PsiParsec<Void> when(Boolean t, PsiParsec<Void> x) {
    return t ? x : pVoid;
  }

  public static PsiParsec<Void> whenM(PsiParsec<Boolean> t, PsiParsec<Void> x) {
    return t.flatMap(r -> when(r, x));
  }

  public static PIf pif(PsiParsec<Boolean> _if) {
    return new PIf(_if);
  }

  public static class PIf {
    final PsiParsec<Boolean> _if;
    PIf(PsiParsec<Boolean> _if) {
      this._if = _if;
    }

    public <A> PThen<A> pthen(PsiParsec<A> _then) {
      return new PThen<>(_if, _then);
    }
  }

  public static class PThen<A> {
    final PsiParsec<Boolean> _if;
    final PsiParsec<A> _then;
    PThen(PsiParsec<Boolean> _if, PsiParsec<A> _then) {
      this._if = _if;
      this._then = _then;
    }

    public PsiParsec<A> pelse(PsiParsec<A> _else) {
      return _if.flatMap(t -> t ? _then : _else);
    }
  }

  private final Function<PsiBuilder, A> _func;

  PsiParsec(Function<PsiBuilder, A> f) {
    this._func = f;
  }

  public <B> PsiParsec<B> map(Function<A, B> f) {
    return new PsiParsec<>(b -> f.apply(_func.apply(b)));
  }

  public <B> PsiParsec<B> as(B b) {
    return new PsiParsec<>(_builder -> b);
  }

  public <B> PsiParsec<B> flatMap(Function<A, PsiParsec<B>> f) {
    return new PsiParsec<>(b -> f.apply(_func.apply(b)).run(b));
  }

  public <B> PsiParsec<B> then(PsiParsec<B> p) {
    return flatMap(_b -> p);
  }

  public A run(PsiBuilder builder) {
    return _func.apply(builder);
  }

  public PsiParsec<Void> void_() {
    return as(TheVoid);
  }

  public PsiParsec<Void> when(Boolean t) {
    return when(t, this.void_());
  }

  public PsiParsec<Void> whenM(PsiParsec<Boolean> t) {
    return whenM(t, this.void_());
  }


  static <A> Marked<A> Marked(MarkResult markResult, A parseResult) {
    return new Marked<>(markResult, parseResult);
  }

  public static class Marked<A> {
    public final MarkResult markResult;
    public final A parseResult;
    Marked(MarkResult markResult, A parseResult) {
      this.markResult = markResult;
      this.parseResult = parseResult;
    }
  }

  public static class MarkResult {

    public static <A> A match(
      MarkResult r,
      Function<Done, A> onDone,
      Function<Collapse, A> onCollapse,
      Function<Error, A> onError
    ) {
      if (r instanceof Done) return onDone.apply((Done)r);
      if (r instanceof Collapse) return onCollapse.apply((Collapse)r);
      if (r instanceof Error) return onError.apply((Error)r);
      throw new RuntimeException("Impossible case for MarkResult: " + r.getClass());
    }

    public static void matchV(
      MarkResult r,
      Consumer<Done> onDone,
      Consumer<Collapse> onCollapse,
      Consumer<Error> onError
    ) {
      if (r instanceof Done) onDone.accept((Done)r);
      if (r instanceof Collapse) onCollapse.accept((Collapse)r);
      if (r instanceof Error) onError.accept((Error)r);
      throw new RuntimeException("Impossible case for MarkResult: " + r.getClass());
    }

    public <A> A match(
      Function<Done, A> onDone,
      Function<Collapse, A> onCollapse,
      Function<Error, A> onError
    ) {
      return match(this, onDone, onCollapse, onError);
    }

    public void matchV(
      Consumer<Done> onDone,
      Consumer<Collapse> onCollapse,
      Consumer<Error> onError
    ) {
      matchV(this, onDone, onCollapse, onError);
    }

    static Done Done(IElementType el) { return new Done(el); }
    public static class Done extends MarkResult {
      public final IElementType elementType;
      Done(IElementType elementType) {
        this.elementType = elementType;
      }
    }
    static Collapse Collapse(IElementType el) { return new Collapse(el); }
    public static class Collapse extends MarkResult {
      public final IElementType elementType;
      Collapse(IElementType elementType) {
        this.elementType = elementType;
      }
    }
    static Error Error(String message) { return new Error(message); }
    public static class Error extends MarkResult {
      public final String message;
      Error(String message) {
        this.message = message;
      }
    }
  }

  static abstract class Internal {
    public static final PsiParsec<PsiBuilder.Marker> mark = mk(PsiBuilder::mark);
  }
}

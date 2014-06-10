package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes declarations.
 */
public class DeclTopTypeDeserializer implements JsonDeserializer<DeclTopType> {
    @Override
    public DeclTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("TypeDecl")) != null) {
            TypeDecl typeDecl = new TypeDecl();
            typeDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(1), DeclHeadTopType.class);
            typeDecl.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typeDecl;
        } else if ((stuff = objType.getAsJsonArray("TypeFamDecl")) != null) {
            TypeFamDecl typeFamDecl = new TypeFamDecl();
            typeFamDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeFamDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(1), DeclHeadTopType.class);
            typeFamDecl.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(2), KindTopType.class);
            return typeFamDecl;
        } else if ((stuff = objType.getAsJsonArray("DataDecl")) != null) {
            DataDecl dataDecl = new DataDecl();
            dataDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dataDecl.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            dataDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ContextTopType.class);
            dataDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(3), DeclHeadTopType.class);
            dataDecl.qualConDecls = jsonDeserializationContext.deserialize(stuff.get(4), QualConDecl[].class);
            dataDecl.deriving = jsonDeserializationContext.deserialize(stuff.get(5), Deriving.class);
            return dataDecl;
        } else if ((stuff = objType.getAsJsonArray("GDataDecl")) != null) {
            GDataDecl gDataDecl = new GDataDecl();
            gDataDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            gDataDecl.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            gDataDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ContextTopType.class);
            gDataDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(3), DeclHeadTopType.class);
            gDataDecl.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(4), KindTopType.class);
            gDataDecl.gadtDecls = jsonDeserializationContext.deserialize(stuff.get(5), GadtDecl[].class);
            gDataDecl.derivingMaybe = jsonDeserializationContext.deserialize(stuff.get(6), Deriving.class);
            return gDataDecl;
        } else if ((stuff = objType.getAsJsonArray("DataFamDecl")) != null) {
            DataFamDecl dataFamDecl = new DataFamDecl();
            dataFamDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dataFamDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            dataFamDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(2), DeclHeadTopType.class);
            dataFamDecl.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(3), KindTopType.class);
            return dataFamDecl;
        } else if ((stuff = objType.getAsJsonArray("TypeInsDecl")) != null) {
            TypeInsDecl typeInsDecl = new TypeInsDecl();
            typeInsDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeInsDecl.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            typeInsDecl.t2 = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typeInsDecl;
        } else if ((stuff = objType.getAsJsonArray("DataInsDecl")) != null) {
            DataInsDecl dataInsDecl = new DataInsDecl();
            dataInsDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dataInsDecl.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            dataInsDecl.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            dataInsDecl.qualConDecls = jsonDeserializationContext.deserialize(stuff.get(3), QualConDecl[].class);
            dataInsDecl.derivingMaybe = jsonDeserializationContext.deserialize(stuff.get(4), Deriving.class);
            return dataInsDecl;
        } else if ((stuff = objType.getAsJsonArray("GDataInsDecl")) != null) {
            GDataInsDecl gDataInsDecl = new GDataInsDecl();
            gDataInsDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            gDataInsDecl.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            gDataInsDecl.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            gDataInsDecl.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(3), KindTopType.class);
            gDataInsDecl.gadtDecls = jsonDeserializationContext.deserialize(stuff.get(4), GadtDecl[].class);
            gDataInsDecl.derivingMaybe = jsonDeserializationContext.deserialize(stuff.get(5), Deriving.class);
            return gDataInsDecl;
        } else if ((stuff = objType.getAsJsonArray("ClassDecl")) != null) {
            ClassDecl classDecl = new ClassDecl();
            classDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            classDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            classDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(2), DeclHeadTopType.class);
            classDecl.funDeps = jsonDeserializationContext.deserialize(stuff.get(3), FunDep[].class);
            classDecl.classDecls = jsonDeserializationContext.deserialize(stuff.get(4), ClassDecl[].class);
            return classDecl;
        } else if ((stuff = objType.getAsJsonArray("InstDecl")) != null) {
            InstDecl instDecl = new InstDecl();
            instDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            instDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            instDecl.instHead = jsonDeserializationContext.deserialize(stuff.get(2), InstHeadTopType.class);
            instDecl.instDecls = jsonDeserializationContext.deserialize(stuff.get(3), InstDeclTopType[].class);
            return instDecl;
        } else if ((stuff = objType.getAsJsonArray("DerivDecl")) != null) {
            DerivDecl derivDecl = new DerivDecl();
            derivDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            derivDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            derivDecl.instHead = jsonDeserializationContext.deserialize(stuff.get(2), InstHeadTopType.class);
            return derivDecl;
        } else if ((stuff = objType.getAsJsonArray("InfixDecl")) != null) {
            InfixDecl infixDecl = new InfixDecl();
            infixDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            infixDecl.assoc = jsonDeserializationContext.deserialize(stuff.get(1), AssocTopType.class);
            infixDecl.intMaybe = jsonDeserializationContext.deserialize(stuff.get(2), Integer.class);
            infixDecl.ops = jsonDeserializationContext.deserialize(stuff.get(3), OpTopType[].class);
            return infixDecl;
        } else if ((stuff = objType.getAsJsonArray("DefaultDecl")) != null) {
            DefaultDecl defaultDecl = new DefaultDecl();
            defaultDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            defaultDecl.types = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType[].class);
            return defaultDecl;
        } else if ((stuff = objType.getAsJsonArray("SpliceDecl")) != null) {
            SpliceDecl spliceDecl = new SpliceDecl();
            spliceDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            spliceDecl.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return spliceDecl;
        } else if ((stuff = objType.getAsJsonArray("TypeSig")) != null) {
            TypeSig typeSig = new TypeSig();
            typeSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeSig.names = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType[].class);
            typeSig.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typeSig;
        } else if ((stuff = objType.getAsJsonArray("FunBind")) != null) {
            FunBind funBind = new FunBind();
            funBind.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            funBind.match = jsonDeserializationContext.deserialize(stuff.get(1), MatchTopType[].class);
            return funBind;
        } else if ((stuff = objType.getAsJsonArray("PatBind")) != null) {
            PatBind patBind = new PatBind();
            patBind.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            patBind.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            patBind.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            patBind.rhs = jsonDeserializationContext.deserialize(stuff.get(3), RhsTopType.class);
            patBind.binds = jsonDeserializationContext.deserialize(stuff.get(4), BindsTopType.class);
            return patBind;
        } else if ((stuff = objType.getAsJsonArray("ForImp")) != null) {
            ForImp forImp = new ForImp();
            forImp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            forImp.callConv = jsonDeserializationContext.deserialize(stuff.get(1), CallConvTopType.class);
            forImp.safety = jsonDeserializationContext.deserialize(stuff.get(2), SafetyTopType.class);
            forImp.s = jsonDeserializationContext.deserialize(stuff.get(3), String.class);
            forImp.name = jsonDeserializationContext.deserialize(stuff.get(4), NameTopType.class);
            forImp.type = jsonDeserializationContext.deserialize(stuff.get(5), TypeTopType.class);
            return forImp;
        } else if ((stuff = objType.getAsJsonArray("ForExp")) != null) {
            ForExp forExp = new ForExp();
            forExp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            forExp.callConv = jsonDeserializationContext.deserialize(stuff.get(1), CallConvTopType.class);
            forExp.stringMaybe = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            forExp.name = jsonDeserializationContext.deserialize(stuff.get(3), NameTopType.class);
            forExp.type = jsonDeserializationContext.deserialize(stuff.get(4), TypeTopType.class);
            return forExp;
        } else if ((stuff = objType.getAsJsonArray("RulePragmaDecl")) != null) {
            RulePragmaDecl rulePragmaDecl = new RulePragmaDecl();
            rulePragmaDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rulePragmaDecl.rules = jsonDeserializationContext.deserialize(stuff.get(1), Rule[].class);
            return rulePragmaDecl;
        } else if ((stuff = objType.getAsJsonArray("DeprPragmaDecl")) != null) {
            DeprPragmaDecl deprPragmaDecl = new DeprPragmaDecl();
            deprPragmaDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            deprPragmaDecl.pragmas = jsonDeserializationContext.deserialize(stuff.get(1), NameStringPair[].class);
            return deprPragmaDecl;
        } else if ((stuff = objType.getAsJsonArray("WarnPragmaDecl")) != null) {
            WarnPragmaDecl warnPragmaDecl = new WarnPragmaDecl();
            warnPragmaDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            warnPragmaDecl.pragmas = jsonDeserializationContext.deserialize(stuff.get(1), NameStringPair[].class);
            return warnPragmaDecl;
        } else if ((stuff = objType.getAsJsonArray("InlineSig")) != null) {
            InlineSig inlineSig = new InlineSig();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            inlineSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            inlineSig.b = g.fromJson(stuff.get(1), boolean.class);
            inlineSig.activationMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ActivationTopType.class);
            inlineSig.qName = jsonDeserializationContext.deserialize(stuff.get(3), QNameTopType.class);
            return inlineSig;
        } else if ((stuff = objType.getAsJsonArray("InlineConlikeSig")) != null) {
            InlineConlikeSig inlineConlikeSig = new InlineConlikeSig();
            inlineConlikeSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            inlineConlikeSig.activationMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ActivationTopType.class);
            inlineConlikeSig.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            return inlineConlikeSig;
        } else if ((stuff = objType.getAsJsonArray("SpecSig")) != null) {
            SpecSig specSig = new SpecSig();
            specSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            specSig.activationMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ActivationTopType.class);
            specSig.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            specSig.types = jsonDeserializationContext.deserialize(stuff.get(3), TypeTopType[].class);
            return specSig;
        } else if ((stuff = objType.getAsJsonArray("SpecInlineSig")) != null) {
            SpecInlineSig specInlineSig = new SpecInlineSig();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            specInlineSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            specInlineSig.b = g.fromJson(stuff.get(1), boolean.class);
            specInlineSig.activationMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ActivationTopType.class);
            specInlineSig.qName = jsonDeserializationContext.deserialize(stuff.get(3), QNameTopType.class);
            specInlineSig.types = jsonDeserializationContext.deserialize(stuff.get(4), TypeTopType[].class);
            return specInlineSig;
        } else if ((stuff = objType.getAsJsonArray("InstSig")) != null) {
            InstSig instSig = new InstSig();
            instSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            instSig.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            instSig.instHead = jsonDeserializationContext.deserialize(stuff.get(2), InstHeadTopType.class);
            return instSig;
        } else if ((stuff = objType.getAsJsonArray("AnnPragma")) != null) {
            AnnPragma annPragma = new AnnPragma();
            annPragma.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            annPragma.annotation = jsonDeserializationContext.deserialize(stuff.get(1), AnnotationTopType.class);
            return annPragma;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

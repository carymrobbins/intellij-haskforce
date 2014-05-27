package com.haskforce.parsing.jsonParser;

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
        if ((stuff = objType.getAsJsonArray("PatBind")) != null) {
            PatBind patBind = new PatBind();
            patBind.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            patBind.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            patBind.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            patBind.rhs = jsonDeserializationContext.deserialize(stuff.get(3), RhsTopType.class);
            patBind.binds = jsonDeserializationContext.deserialize(stuff.get(4), BindsTopType.class);
            return patBind;
        } else if ((stuff = objType.getAsJsonArray("FunBind")) != null) {
            FunBind funBind = new FunBind();
            funBind.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            funBind.match = jsonDeserializationContext.deserialize(stuff.get(1), MatchTopType[].class);
            return funBind;
        } else if ((stuff = objType.getAsJsonArray("ForImp")) != null) {
            ForImp forImp = new ForImp();
            forImp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            forImp.callConv = jsonDeserializationContext.deserialize(stuff.get(1), CallConvTopType.class);
            forImp.safety = jsonDeserializationContext.deserialize(stuff.get(2), SafetyTopType.class);
            forImp.s = jsonDeserializationContext.deserialize(stuff.get(3), String.class);
            forImp.name = jsonDeserializationContext.deserialize(stuff.get(4), NameTopType.class);
            forImp.type = jsonDeserializationContext.deserialize(stuff.get(5), TypeTopType.class);
            return forImp;
        } else if ((stuff = objType.getAsJsonArray("InstDecl")) != null) {
            InstDecl instDecl = new InstDecl();
            instDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            instDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            instDecl.instHead = jsonDeserializationContext.deserialize(stuff.get(2), InstHeadTopType.class);
            instDecl.instDecls = jsonDeserializationContext.deserialize(stuff.get(3), InstDeclTopType[].class);
            return instDecl;
        } else if ((stuff = objType.getAsJsonArray("TypeDecl")) != null) {
            TypeDecl typeDecl = new TypeDecl();
            typeDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(1), DeclHeadTopType.class);
            typeDecl.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typeDecl;
        } else if ((stuff = objType.getAsJsonArray("DataDecl")) != null) {
            DataDecl dataDecl = new DataDecl();
            dataDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dataDecl.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            dataDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ContextTopType.class);
            dataDecl.declHead = jsonDeserializationContext.deserialize(stuff.get(3), DeclHeadTopType.class);
            dataDecl.qualConDecls = jsonDeserializationContext.deserialize(stuff.get(4), QualConDecl[].class);
            dataDecl.deriving = jsonDeserializationContext.deserialize(stuff.get(5), Deriving.class);
            return dataDecl;
        } else if ((stuff = objType.getAsJsonArray("TypeSig")) != null) {
            TypeSig typeSig = new TypeSig();
            typeSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeSig.names = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType[].class);
            typeSig.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typeSig;
        } else if ((stuff = objType.getAsJsonArray("DeprPragmaDecl")) != null) {
            DeprPragmaDecl deprPragmaDecl = new DeprPragmaDecl();
            deprPragmaDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            deprPragmaDecl.pragmas = jsonDeserializationContext.deserialize(stuff.get(1), NameStringPair[].class);
            return deprPragmaDecl;
        } else if ((stuff = objType.getAsJsonArray("SpliceDecl")) != null) {
            SpliceDecl spliceDecl = new SpliceDecl();
            spliceDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            spliceDecl.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return spliceDecl;
        } else if ((stuff = objType.getAsJsonArray("DerivDecl")) != null) {
            DerivDecl derivDecl = new DerivDecl();
            derivDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            derivDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            derivDecl.instHead = jsonDeserializationContext.deserialize(stuff.get(1), InstHeadTopType.class);
            return derivDecl;
        }
        // TODO: Rest of DeclTopType
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

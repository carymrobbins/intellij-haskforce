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
 * Deserializes RPats.
 */
public class RPatTopTypeDeserializer implements JsonDeserializer<RPatTopType> {
    @Override
    public RPatTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("RPOp")) != null) { // TODO: Test.
            RPOp rpOp = new RPOp();
            rpOp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpOp.rpat = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType.class);
            rpOp.rpatOp = jsonDeserializationContext.deserialize(stuff.get(2), RPatOpTopType.class);
            return rpOp;
        } else if ((stuff = objType.getAsJsonArray("RPEither")) != null) { // TODO: Test.
            RPEither rpEither = new RPEither();
            rpEither.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpEither.r1 = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType.class);
            rpEither.r2 = jsonDeserializationContext.deserialize(stuff.get(2), RPatTopType.class);
            return rpEither;
        } else if ((stuff = objType.getAsJsonArray("RPSeq")) != null) { // TODO: Test.
            RPSeq rpSeq = new RPSeq();
            rpSeq.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpSeq.rpats = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType[].class);
            return rpSeq;
        } else if ((stuff = objType.getAsJsonArray("RPGuard")) != null) { // TODO: Test.
            RPGuard rpGuard = new RPGuard();
            rpGuard.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpGuard.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            rpGuard.stmts = jsonDeserializationContext.deserialize(stuff.get(2), StmtTopType[].class);
            return rpGuard;
        } else if ((stuff = objType.getAsJsonArray("RPCAs")) != null) { // TODO: Test.
            RPCAs rpcAs = new RPCAs();
            rpcAs.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpcAs.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            rpcAs.rpat = jsonDeserializationContext.deserialize(stuff.get(2), RPatTopType.class);
            return rpcAs;
        } else if ((stuff = objType.getAsJsonArray("RPAs")) != null) { // TODO: Test.
            RPAs rpcAs = new RPAs();
            rpcAs.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpcAs.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            rpcAs.rpat = jsonDeserializationContext.deserialize(stuff.get(2), RPatTopType.class);
            return rpcAs;
        } else if ((stuff = objType.getAsJsonArray("RPParens")) != null) { // TODO: Test.
            RPParen rpParen = new RPParen();
            rpParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpParen.rpat = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType.class);
            return rpParen;
        } else if ((stuff = objType.getAsJsonArray("RPPat")) != null) { // TODO: Test.
            RPPat rpParen = new RPPat();
            rpParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rpParen.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return rpParen;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

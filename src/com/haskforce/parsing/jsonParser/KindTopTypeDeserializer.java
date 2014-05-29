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
 * Deserializes annotations.
 */
public class KindTopTypeDeserializer implements JsonDeserializer<KindTopType> {
    @Override
    public KindTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("KindStar")) != null) {
            KindStar kindStar = new KindStar();
            kindStar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return kindStar;
        } else if ((stuff = objType.getAsJsonArray("KindBang")) != null) { // TODO: Test.
            KindBang kindBang = new KindBang();
            kindBang.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return kindBang;
        } else if ((stuff = objType.getAsJsonArray("KindFn")) != null) { // TODO: Test.
            KindFn kindFn = new KindFn();
            kindFn.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindFn.k1 = jsonDeserializationContext.deserialize(stuff.get(1), KindTopType.class);
            kindFn.k2 = jsonDeserializationContext.deserialize(stuff.get(2), KindTopType.class);
            return kindFn;
        } else if ((stuff = objType.getAsJsonArray("KindParen")) != null) { // TODO: Test.
            KindParen kindParen = new KindParen();
            kindParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindParen.kind = jsonDeserializationContext.deserialize(stuff.get(1), KindTopType.class);
            return kindParen;
        } else if ((stuff = objType.getAsJsonArray("KindVar")) != null) { // TODO: Test.
            KindVar kindVar = new KindVar();
            kindVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindVar.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return kindVar;
        } else if ((stuff = objType.getAsJsonArray("KindApp")) != null) { // TODO: Test.
            KindApp kindApp = new KindApp();
            kindApp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindApp.k1 = jsonDeserializationContext.deserialize(stuff.get(1), KindTopType.class);
            kindApp.k2 = jsonDeserializationContext.deserialize(stuff.get(2), KindTopType.class);
            return kindApp;
        } else if ((stuff = objType.getAsJsonArray("KindTuple")) != null) { // TODO: Test.
            KindTuple kindTuple = new KindTuple();
            kindTuple.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindTuple.kinds = jsonDeserializationContext.deserialize(stuff.get(1), KindTopType[].class);
            return kindTuple;
        } else if ((stuff = objType.getAsJsonArray("KindList")) != null) { // TODO: Test.
            KindList kindList = new KindList();
            kindList.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindList.kinds = jsonDeserializationContext.deserialize(stuff.get(1), KindTopType[].class);
            return kindList;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.AsstTopType;
import com.haskforce.parsing.srcExtsDatatypes.ContextTopType;
import com.haskforce.parsing.srcExtsDatatypes.CxEmpty;
import com.haskforce.parsing.srcExtsDatatypes.CxParen;
import com.haskforce.parsing.srcExtsDatatypes.CxSingle;
import com.haskforce.parsing.srcExtsDatatypes.CxTuple;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes contexts..
 */
public class ContextTopTypeDeserializer implements JsonDeserializer<ContextTopType> {
    @Override
    public ContextTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("CxSingle")) != null) {
            CxSingle cxSingle = new CxSingle();
            cxSingle.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            cxSingle.asst = jsonDeserializationContext.deserialize(stuff.get(1), AsstTopType.class);
            return cxSingle;
        } else if ((stuff = objType.getAsJsonArray("CxTuple")) != null) {
            CxTuple cxTuple = new CxTuple();
            cxTuple.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            cxTuple.assts = jsonDeserializationContext.deserialize(stuff.get(1), AsstTopType[].class);
            return cxTuple;
        } else if ((stuff = objType.getAsJsonArray("CxParen")) != null) {
            CxParen cxParen = new CxParen();
            cxParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            cxParen.context = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            return cxParen;
        } else if ((stuff = objType.getAsJsonArray("CxEmpty")) != null) {
            CxEmpty cxEmpty = new CxEmpty();
            cxEmpty.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return cxEmpty;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

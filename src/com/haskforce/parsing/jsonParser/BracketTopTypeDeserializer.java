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
 * Deserializes brackets.
 */
public class BracketTopTypeDeserializer implements JsonDeserializer<BracketTopType> {
    @Override
    public BracketTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ExpBracket")) != null) {
            ExpBracket expBracket = new ExpBracket();
            expBracket.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            expBracket.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return expBracket;
        } else if ((stuff = objType.getAsJsonArray("PatBracket")) != null) {
            PatBracket patBracket = new PatBracket();
            patBracket.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            patBracket.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return patBracket;
        } else if ((stuff = objType.getAsJsonArray("TypeBracket")) != null) {
            TypeBracket typeBracket = new TypeBracket();
            typeBracket.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeBracket.type = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            return typeBracket;
        } else if ((stuff = objType.getAsJsonArray("DeclBracket")) != null) {
            DeclBracket declBracket = new DeclBracket();
            declBracket.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            declBracket.decls = jsonDeserializationContext.deserialize(stuff.get(1), DeclTopType[].class);
            return declBracket;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

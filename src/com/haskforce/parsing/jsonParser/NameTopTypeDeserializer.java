package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Ident;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.Symbol;

import java.lang.reflect.Type;

/**
 * Deserializes names.
 */
public class NameTopTypeDeserializer implements JsonDeserializer<NameTopType> {
    @Override
    public NameTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Ident")) != null) {
            Ident id = new Ident();
            id.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            id.name = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return id;
        } else if ((stuff = objType.getAsJsonArray("Symbol")) != null) {
            Symbol id = new Symbol();
            id.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            id.symbol = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return id;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

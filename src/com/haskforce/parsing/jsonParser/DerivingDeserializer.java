package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Deriving;
import com.haskforce.parsing.srcExtsDatatypes.InstHeadTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes deriving clauses.
 */
public class DerivingDeserializer implements JsonDeserializer<Deriving> {
    @Override
    public Deriving deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Deriving")) != null) {
            Deriving deriving = new Deriving();
            deriving.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            deriving.instHeads = jsonDeserializationContext.deserialize(stuff.get(1), InstHeadTopType[].class);
            return deriving;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

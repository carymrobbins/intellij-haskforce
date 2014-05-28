package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.FunDep;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes FunDeps.
 */
public class FunDepDeserializer implements JsonDeserializer<FunDep> {
    @Override
    public FunDep deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("FunDep")) != null) {
            FunDep funDep = new FunDep();
            funDep.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            funDep.fromNames = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType[].class);
            funDep.toNames = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType[].class);
            return funDep;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ActivationTopType;
import com.haskforce.parsing.srcExtsDatatypes.ActiveFrom;
import com.haskforce.parsing.srcExtsDatatypes.ActiveUntil;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes Activations.
 */
public class ActivationTopTypeDeserializer implements JsonDeserializer<ActivationTopType> {
    @Override
    public ActivationTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        Gson g = new Gson(); // TODO: Remove with 1.7.
        if ((stuff = objType.getAsJsonArray("ActiveFrom")) != null) {
            ActiveFrom activeFrom = new ActiveFrom();
            activeFrom.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            activeFrom.anInt = g.fromJson(stuff.get(1), int.class);
            return activeFrom;
        } else if ((stuff = objType.getAsJsonArray("ActiveUntil")) != null) {
            ActiveUntil activeUntil = new ActiveUntil();
            activeUntil.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            activeUntil.anInt = g.fromJson(stuff.get(1), int.class);
            return activeUntil;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

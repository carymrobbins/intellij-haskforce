package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.RPatTopType;

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
        // TODO: Implement RPat.
        /*
        if ((stuff = objType.getAsJsonArray("AssocNone")) != null) { // TODO: Test.
            AssocNone assocNone = new AssocNone();
            assocNone.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return assocNone;
        }
        */
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

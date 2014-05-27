package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Alt;
import com.haskforce.parsing.srcExtsDatatypes.BindsTopType;
import com.haskforce.parsing.srcExtsDatatypes.GuardedAltsTopType;
import com.haskforce.parsing.srcExtsDatatypes.PatTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes case alternatives.
 */
public class AltDeserializer implements JsonDeserializer<Alt> {
    @Override
    public Alt deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Alt")) != null) {
            Alt alt = new Alt();
            alt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            alt.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            alt.guardedAlts = jsonDeserializationContext.deserialize(stuff.get(2), GuardedAltsTopType.class);
            alt.bindsMaybe = jsonDeserializationContext.deserialize(stuff.get(3), BindsTopType.class);
            return alt;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

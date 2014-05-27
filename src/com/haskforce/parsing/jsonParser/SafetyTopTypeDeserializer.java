package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.PlayInterruptible;
import com.haskforce.parsing.srcExtsDatatypes.PlayRisky;
import com.haskforce.parsing.srcExtsDatatypes.PlaySafe;
import com.haskforce.parsing.srcExtsDatatypes.SafetyTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes safety clauses.
 */
public class SafetyTopTypeDeserializer implements JsonDeserializer<SafetyTopType> {
    @Override
    public SafetyTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonObject stuff;
        JsonArray stuff2;
        if ((stuff = objType.getAsJsonObject("PlayRisky")) != null) {
            PlayRisky playRisky = new PlayRisky();
            playRisky.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return playRisky;
        } else if ((stuff2 = objType.getAsJsonArray("PlaySafe")) != null) {
            PlaySafe playSafe = new PlaySafe();
            Gson g = new Gson();
            playSafe.srcInfoSpan = jsonDeserializationContext.deserialize(stuff2.get(0), SrcInfoSpan.class);
            playSafe.safeOrThreadSafe = g.fromJson(stuff2.get(1), Boolean.class);
            return playSafe;
        } else if ((stuff = objType.getAsJsonObject("PlayInterruptible")) != null) {
            PlayInterruptible playInterruptible = new PlayInterruptible();
            playInterruptible.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return playInterruptible;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

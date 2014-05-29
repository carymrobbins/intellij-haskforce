package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.GuardedAlt;
import com.haskforce.parsing.srcExtsDatatypes.GuardedAlts;
import com.haskforce.parsing.srcExtsDatatypes.GuardedAltsTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.UnGuardedAlt;

import java.lang.reflect.Type;

/**
 * Deserializes guarded alternatives.
 */
public class GuardedAltsTopTypeDeserializer implements JsonDeserializer<GuardedAltsTopType> {
    @Override
    public GuardedAltsTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("UnGuardedAlt")) != null) { // TODO: Test.
            UnGuardedAlt unGuardedAlt = new UnGuardedAlt();
            unGuardedAlt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            unGuardedAlt.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return unGuardedAlt;
        } else if ((stuff = objType.getAsJsonArray("GuardedAlts")) != null) { // TODO: Test.
            GuardedAlts guardedAlts = new GuardedAlts();
            guardedAlts.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            guardedAlts.alts = jsonDeserializationContext.deserialize(stuff.get(1), GuardedAlt[].class);
            return guardedAlts;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

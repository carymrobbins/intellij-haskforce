package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.GuardedRhs;
import com.haskforce.parsing.srcExtsDatatypes.GuardedRhss;
import com.haskforce.parsing.srcExtsDatatypes.RhsTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.UnGuardedRhs;

import java.lang.reflect.Type;

/**
 * Deserializes right hand sides.
 */
public class RhsTopTypeDeserializer implements JsonDeserializer<RhsTopType> {
    @Override
    public RhsTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("UnGuardedRhs")) != null) {
            UnGuardedRhs rhs = new UnGuardedRhs();
            rhs.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rhs.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return rhs;
        } else if ((stuff = objType.getAsJsonArray("GuardedRhss")) != null) {
            GuardedRhss rhss = new GuardedRhss();
            rhss.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rhss.rhsses = jsonDeserializationContext.deserialize(stuff.get(1), GuardedRhs[].class);
            return rhss;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.GuardedAlt;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.StmtTopType;

import java.lang.reflect.Type;

/**
 * Deserializes guarded alternatives.
 */
public class GuardedAltDeserializer implements JsonDeserializer<GuardedAlt> {
    @Override
    public GuardedAlt deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("GuardedAlt")) != null) {
            GuardedAlt guardedAlt = new GuardedAlt();
            guardedAlt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            guardedAlt.stmts = jsonDeserializationContext.deserialize(stuff.get(1), StmtTopType[].class);
            guardedAlt.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return guardedAlt;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

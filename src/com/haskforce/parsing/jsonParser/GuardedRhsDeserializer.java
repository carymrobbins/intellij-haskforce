package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.GuardedRhs;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.StmtTopType;

import java.lang.reflect.Type;

/**
 * Deserializes guarded rhses.
 */
public class GuardedRhsDeserializer implements JsonDeserializer<GuardedRhs> {
    @Override
    public GuardedRhs deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("GuardedRhs")) != null) {
            GuardedRhs guardedRhs = new GuardedRhs();
            guardedRhs.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            guardedRhs.stmts = jsonDeserializationContext.deserialize(stuff.get(1), StmtTopType[].class);
            guardedRhs.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return guardedRhs;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

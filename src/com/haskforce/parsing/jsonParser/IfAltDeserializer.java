package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.IfAlt;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes if-alternatives.
 */
public class IfAltDeserializer implements JsonDeserializer<IfAlt> {
    @Override
    public IfAlt deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("IfAlt")) != null) {
            IfAlt ifAlt = new IfAlt();
            ifAlt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ifAlt.e1 = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            ifAlt.e2 = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return ifAlt;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

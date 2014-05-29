package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.IPBind;
import com.haskforce.parsing.srcExtsDatatypes.IPNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes implicit bindings.
 */
public class IPBindDeserializer implements JsonDeserializer<IPBind> {
    @Override
    public IPBind deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("IPBind")) != null) {
            IPBind ipBind = new IPBind();
            ipBind.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ipBind.ipName = jsonDeserializationContext.deserialize(stuff.get(1), IPNameTopType.class);
            ipBind.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return ipBind;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

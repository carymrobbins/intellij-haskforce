package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.IPDup;
import com.haskforce.parsing.srcExtsDatatypes.IPLin;
import com.haskforce.parsing.srcExtsDatatypes.IPNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes IPNames.
 */
public class IPNameTopTypeDeserializer implements JsonDeserializer<IPNameTopType> {
    @Override
    public IPNameTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("IPDup")) != null) {
            IPDup ipDup = new IPDup();
            ipDup.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ipDup.name = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return ipDup;
        } else if ((stuff = objType.getAsJsonArray("IPLin")) != null) {
            IPLin ipLin = new IPLin();
            ipLin.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ipLin.name = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return ipLin;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

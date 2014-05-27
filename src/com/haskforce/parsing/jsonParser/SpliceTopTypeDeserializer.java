package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.IdSplice;
import com.haskforce.parsing.srcExtsDatatypes.ParenSplice;
import com.haskforce.parsing.srcExtsDatatypes.SpliceTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes splices.
 */
public class SpliceTopTypeDeserializer implements JsonDeserializer<SpliceTopType> {
    @Override
    public SpliceTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        // TODO: Test SpliceTopType.
        if ((stuff = objType.getAsJsonArray("IdSplice")) != null) {
            IdSplice idSplice = new IdSplice();
            idSplice.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            idSplice.s = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return idSplice;
        } else if ((stuff = objType.getAsJsonArray("ParenSplice")) != null) {
            ParenSplice parenSplice = new ParenSplice();
            parenSplice.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            parenSplice.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return parenSplice;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

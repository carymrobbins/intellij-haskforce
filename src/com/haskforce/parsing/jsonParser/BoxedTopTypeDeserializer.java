package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Boxed;
import com.haskforce.parsing.srcExtsDatatypes.BoxedTopType;
import com.haskforce.parsing.srcExtsDatatypes.Unboxed;

import java.lang.reflect.Type;

/**
 * Deserializes boxed/unboxed.
 */
public class BoxedTopTypeDeserializer implements JsonDeserializer<BoxedTopType> {
    @Override
    public BoxedTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        if (jsonElement.getAsJsonObject().get("Boxed") != null) {
            return new Boxed();
        } else if (jsonElement.getAsJsonObject().get("Unboxed") != null) {
            return new Unboxed();
        }
        throw new JsonParseException("Unexpected JSON object type: " + jsonElement.toString());
    }
}

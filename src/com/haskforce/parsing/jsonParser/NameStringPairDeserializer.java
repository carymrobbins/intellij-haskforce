package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.NameStringPair;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;

import java.lang.reflect.Type;

/**
 * Deserializes name/string pairs.
 */
public class NameStringPairDeserializer implements JsonDeserializer<NameStringPair> {
    @Override
    public NameStringPair deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonArray objType = jsonElement.getAsJsonArray();
        NameStringPair nameStringPair = new NameStringPair();
        nameStringPair.names = jsonDeserializationContext.deserialize(objType.get(0), NameTopType[].class);
        nameStringPair.s = jsonDeserializationContext.deserialize(objType.get(1), String.class);
        return nameStringPair;
    }
}

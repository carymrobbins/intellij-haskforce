package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Comment;
import com.haskforce.parsing.srcExtsDatatypes.ModuleTopType;
import com.haskforce.parsing.srcExtsDatatypes.TopPair;

import java.lang.reflect.Type;

/**
 * Deserializes the top pair returned from parser-helper.
 */
public class TopPairDeserializer implements JsonDeserializer<TopPair> {
    @Override
    public TopPair deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonArray stuff = jsonElement.getAsJsonArray();
        TopPair topPair = new TopPair();
        topPair.moduleType = jsonDeserializationContext.deserialize(stuff.get(0), ModuleTopType.class);
        topPair.comments = jsonDeserializationContext.deserialize(stuff.get(1), Comment[].class);
        return topPair;
    }
}

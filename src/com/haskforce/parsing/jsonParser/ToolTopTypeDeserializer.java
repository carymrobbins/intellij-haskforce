package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.GHC;
import com.haskforce.parsing.srcExtsDatatypes.HADDOCK;
import com.haskforce.parsing.srcExtsDatatypes.HUGS;
import com.haskforce.parsing.srcExtsDatatypes.NHC98;
import com.haskforce.parsing.srcExtsDatatypes.ToolTopType;
import com.haskforce.parsing.srcExtsDatatypes.UnknownTool;
import com.haskforce.parsing.srcExtsDatatypes.YHC;

import java.lang.reflect.Type;

/**
 * Deserializes tools.
 */
public class ToolTopTypeDeserializer implements JsonDeserializer<ToolTopType> {
    @Override
    public ToolTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonArray stuff;
        if (jsonElement.getAsJsonObject().get("GHC") != null) {
            return new GHC();
        } else if (jsonElement.getAsJsonObject().get("HUGS") != null) { // TODO: Test.
            return new HUGS();
        } else if (jsonElement.getAsJsonObject().get("NHC98") != null) { // TODO: Test.
            return new NHC98();
        } else if (jsonElement.getAsJsonObject().get("YHC") != null) { // TODO: Test.
            return new YHC();
        } else if (jsonElement.getAsJsonObject().get("HADDOCK") != null) { // TODO: Test.
            return new HADDOCK();
        } else if ((stuff = jsonElement.getAsJsonObject().getAsJsonArray("UnknownTool")) != null) { // TODO: Test.
            UnknownTool tool = new UnknownTool();
            tool.toolName = jsonDeserializationContext.deserialize(stuff.get(0), String.class);
            return tool;
        }
        throw new JsonParseException("Unexpected JSON object type: " + jsonElement.toString());
    }
}

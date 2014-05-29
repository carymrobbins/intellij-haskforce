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
        if (jsonElement.getAsString().equals("GHC")) { // TODO: Test.
            return new GHC();
        } else if (jsonElement.getAsString().equals("HUGS")) { // TODO: Test.
            return new HUGS();
        } else if (jsonElement.getAsString().equals("NHC98")) { // TODO: Test.
            return new NHC98();
        } else if (jsonElement.getAsString().equals("YHC")) { // TODO: Test.
            return new YHC();
        } else if (jsonElement.getAsString().equals("HADDOCK")) { // TODO: Test.
            return new HADDOCK();
        } else if ((stuff = jsonElement.getAsJsonObject().getAsJsonArray("UnknownTool")) != null) { // TODO: Test.
            UnknownTool tool = new UnknownTool();
            tool.toolName = jsonDeserializationContext.deserialize(stuff.get(0), String.class);
            return tool;
        }
        throw new JsonParseException("Unexpected JSON object type: " + jsonElement.toString());
    }
}

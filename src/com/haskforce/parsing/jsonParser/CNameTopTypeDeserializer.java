package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes Con/Var-Names.
 */
public class CNameTopTypeDeserializer implements JsonDeserializer<CNameTopType> {
    @Override
    public CNameTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("VarName")) != null) {
            VarName varName = new VarName();
            varName.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            varName.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return varName;
        } else if ((stuff = objType.getAsJsonArray("ConName")) != null) {
            ConName conName = new ConName();
            conName.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            conName.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return conName;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

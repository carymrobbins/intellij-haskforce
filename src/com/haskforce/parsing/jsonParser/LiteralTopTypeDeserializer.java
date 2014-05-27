package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.IntLit;
import com.haskforce.parsing.srcExtsDatatypes.LiteralTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.StringLit;

import java.lang.reflect.Type;

/**
 * Deserializes literals.
 */
public class LiteralTopTypeDeserializer implements JsonDeserializer<LiteralTopType> {
    @Override
    public LiteralTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("String")) != null) {
            StringLit stringLit = new StringLit();
            stringLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            stringLit.value = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            stringLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return stringLit;
        } else if ((stuff = objType.getAsJsonArray("Int")) != null) {
            IntLit intLit = new IntLit();
            intLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            intLit.value = jsonDeserializationContext.deserialize(stuff.get(1), Integer.class);
            intLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return intLit;
        }
        // TODO: Deserialize rest of LitTopType.
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

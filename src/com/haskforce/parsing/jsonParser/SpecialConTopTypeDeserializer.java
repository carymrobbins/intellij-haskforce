package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.SpecialConTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.UnitCon;

import java.lang.reflect.Type;

/**
 * Deserializes special constructors.
 */
public class SpecialConTopTypeDeserializer implements JsonDeserializer<SpecialConTopType> {
    @Override
    public SpecialConTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        if (objType.has("UnitCon")) {
            UnitCon unitCon = new UnitCon();
            unitCon.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("UnitCon"), SrcInfoSpan.class);
            return unitCon;
        }
        // TODO: Rest of SpecialConTopType
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

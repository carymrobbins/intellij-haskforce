package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ModuleName;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes module names.
 */
public class ModuleNameDeserializer implements JsonDeserializer<ModuleName> {
    @Override
    public ModuleName deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ModuleName")) != null) {
            ModuleName moduleName = new ModuleName();
            moduleName.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            moduleName.name = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return moduleName;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

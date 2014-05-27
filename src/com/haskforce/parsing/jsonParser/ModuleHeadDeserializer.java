package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExportSpecList;
import com.haskforce.parsing.srcExtsDatatypes.ModuleHead;
import com.haskforce.parsing.srcExtsDatatypes.ModuleName;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.WarningTextTopType;

import java.lang.reflect.Type;

/**
 * Deserializes module heads.
 */
public class ModuleHeadDeserializer implements JsonDeserializer<ModuleHead> {
    @Override
    public ModuleHead deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ModuleHead")) != null) {
            ModuleHead moduleHead = new ModuleHead();
            moduleHead.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            moduleHead.moduleName = jsonDeserializationContext.deserialize(stuff.get(1), ModuleName.class);
            moduleHead.warningTextMaybe = jsonDeserializationContext.deserialize(stuff.get(2), WarningTextTopType[].class);
            moduleHead.exportSpecListMaybe = jsonDeserializationContext.deserialize(stuff.get(3), ExportSpecList[].class);
            return moduleHead;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

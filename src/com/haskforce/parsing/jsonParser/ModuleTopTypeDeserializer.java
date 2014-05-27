package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.DeclTopType;
import com.haskforce.parsing.srcExtsDatatypes.ImportDecl;
import com.haskforce.parsing.srcExtsDatatypes.Module;
import com.haskforce.parsing.srcExtsDatatypes.ModuleHead;
import com.haskforce.parsing.srcExtsDatatypes.ModulePragmaTopType;
import com.haskforce.parsing.srcExtsDatatypes.ModuleTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes modules.
 */
public class ModuleTopTypeDeserializer implements JsonDeserializer<ModuleTopType> {
    @Override
    public ModuleTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Module")) != null) {
            Module module = new Module();
            module.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            module.moduleHeadMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ModuleHead.class);
            module.modulePragmas = jsonDeserializationContext.deserialize(stuff.get(2), ModulePragmaTopType[].class);
            module.importDecls = jsonDeserializationContext.deserialize(stuff.get(3), ImportDecl[].class);
            module.decls = jsonDeserializationContext.deserialize(stuff.get(4), DeclTopType[].class);
            return module;
        }
        // TODO: Rest of ModuleTopType
        throw new JsonParseException("Weird object type: " + objType.toString());
    }
}

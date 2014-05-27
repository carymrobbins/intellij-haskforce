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
        } else if ((stuff = objType.getAsJsonArray("XmlPage")) != null) { // TODO: Test.
            XmlPage xmlPage = new XmlPage();
            xmlPage.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            xmlPage.moduleName = jsonDeserializationContext.deserialize(stuff.get(1), ModuleName.class);
            xmlPage.modulePragmas = jsonDeserializationContext.deserialize(stuff.get(2), ModulePragmaTopType[].class);
            xmlPage.xName = jsonDeserializationContext.deserialize(stuff.get(3), XNameTopType.class);
            xmlPage.xAttrs = jsonDeserializationContext.deserialize(stuff.get(4), XAttr[].class);
            xmlPage.expMaybe = jsonDeserializationContext.deserialize(stuff.get(5), ExpTopType.class);
            xmlPage.exps = jsonDeserializationContext.deserialize(stuff.get(6), ExpTopType[].class);
            return xmlPage;
        } else if ((stuff = objType.getAsJsonArray("XmlHybrid")) != null) { // TODO: Test.
            XmlHybrid xmlHybrid = new XmlHybrid();
            xmlHybrid.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            xmlHybrid.moduleHeadMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ModuleHead.class);
            xmlHybrid.modulePragmas = jsonDeserializationContext.deserialize(stuff.get(2), ModulePragmaTopType[].class);
            xmlHybrid.importDecls = jsonDeserializationContext.deserialize(stuff.get(3), ImportDecl[].class);
            xmlHybrid.decls = jsonDeserializationContext.deserialize(stuff.get(4), DeclTopType[].class);
            xmlHybrid.xName = jsonDeserializationContext.deserialize(stuff.get(5), XNameTopType.class);
            xmlHybrid.xAttrs = jsonDeserializationContext.deserialize(stuff.get(6), XAttr[].class);
            xmlHybrid.expMaybe = jsonDeserializationContext.deserialize(stuff.get(7), ExpTopType.class);
            xmlHybrid.exps = jsonDeserializationContext.deserialize(stuff.get(8), ExpTopType[].class);
            return xmlHybrid;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

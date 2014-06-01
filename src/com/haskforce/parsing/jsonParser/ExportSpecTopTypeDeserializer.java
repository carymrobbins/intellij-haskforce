package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes ExportSpecs.
 */
public class ExportSpecTopTypeDeserializer implements JsonDeserializer<ExportSpecTopType> {
    @Override
    public ExportSpecTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        Gson g = new Gson(); // TODO: Remove with 1.7.
        if ((stuff = objType.getAsJsonArray("EVar")) != null) {
            EVar eVar = new EVar();
            eVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            eVar.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return eVar;
        } else if ((stuff = objType.getAsJsonArray("EAbs")) != null) {
            EAbs eAbs = new EAbs();
            eAbs.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            eAbs.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return eAbs;
        } else if ((stuff = objType.getAsJsonArray("EThingAll")) != null) {
            EThingAll eThingAll = new EThingAll();
            eThingAll.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            eThingAll.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return eThingAll;
        } else if ((stuff = objType.getAsJsonArray("EThingWith")) != null) {
            EThingWith eThingWith = new EThingWith();
            eThingWith.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            eThingWith.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            eThingWith.cNames = jsonDeserializationContext.deserialize(stuff.get(2), CNameTopType[].class);
            return eThingWith;
        } else if ((stuff = objType.getAsJsonArray("EModuleContents")) != null) {
            EModuleContents eModuleContents = new EModuleContents();
            eModuleContents.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            eModuleContents.moduleName = jsonDeserializationContext.deserialize(stuff.get(1), ModuleName.class);
            return eModuleContents;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

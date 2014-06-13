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
 * Deserializes class declaration elements.
 */
public class ClassDeclTopTypeDeserializer implements JsonDeserializer<ClassDeclTopType> {
    @Override
    public ClassDeclTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ClsDecl")) != null) {
            ClsDecl clsDecl = new ClsDecl();
            clsDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            clsDecl.decl = jsonDeserializationContext.deserialize(stuff.get(1), DeclTopType.class);
            return clsDecl;
        } else if ((stuff = objType.getAsJsonArray("ClsDataFam")) != null) {
            ClsDataFam clsDataFam = new ClsDataFam();
            clsDataFam.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            clsDataFam.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(1), ContextTopType.class);
            clsDataFam.declHead = jsonDeserializationContext.deserialize(stuff.get(2), DeclHeadTopType.class);
            clsDataFam.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(3), KindTopType.class);
            return clsDataFam;
        } else if ((stuff = objType.getAsJsonArray("ClsTyFam")) != null) {
            ClsTyFam clsTyFam = new ClsTyFam();
            clsTyFam.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            clsTyFam.declHead = jsonDeserializationContext.deserialize(stuff.get(1), DeclHeadTopType.class);
            clsTyFam.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(2), KindTopType.class);
            return clsTyFam;
        } else if ((stuff = objType.getAsJsonArray("ClsTyDef")) != null) { // TODO: Test.
            ClsTyDef clsTyDef = new ClsTyDef();
            clsTyDef.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            clsTyDef.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            clsTyDef.t2 = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return clsTyDef;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

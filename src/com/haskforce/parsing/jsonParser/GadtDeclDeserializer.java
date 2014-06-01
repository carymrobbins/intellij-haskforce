package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.GadtDecl;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TypeTopType;

import java.lang.reflect.Type;

/**
 * Deserializes GadtDecls.
 */
public class GadtDeclDeserializer implements JsonDeserializer<GadtDecl> {
    @Override
    public GadtDecl deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("GadtDecl")) != null) {
            GadtDecl gadtDecl = new GadtDecl();
            gadtDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            gadtDecl.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            gadtDecl.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return gadtDecl;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

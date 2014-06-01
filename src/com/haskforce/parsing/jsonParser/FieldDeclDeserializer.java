package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BangTypeTopType;
import com.haskforce.parsing.srcExtsDatatypes.FieldDecl;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes field declarations.
 */
public class FieldDeclDeserializer implements JsonDeserializer<FieldDecl> {
    @Override
    public FieldDecl deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("FieldDecl")) != null) {
            FieldDecl fieldDecl = new FieldDecl();
            fieldDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            fieldDecl.names = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType[].class);
            fieldDecl.bang = jsonDeserializationContext.deserialize(stuff.get(2), BangTypeTopType.class);
            return fieldDecl;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BangTypeTopType;
import com.haskforce.parsing.srcExtsDatatypes.ConDecl;
import com.haskforce.parsing.srcExtsDatatypes.ConDeclTopType;
import com.haskforce.parsing.srcExtsDatatypes.FieldDecl;
import com.haskforce.parsing.srcExtsDatatypes.InfixConDecl;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.RecDecl;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes constructor declarations.
 */
public class ConDeclTopTypeDeserializer implements JsonDeserializer<ConDeclTopType> {
    @Override
    public ConDeclTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ConDecl")) != null) {
            ConDecl conDecl = new ConDecl();
            conDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            conDecl.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            conDecl.bangTypes = jsonDeserializationContext.deserialize(stuff.get(2), BangTypeTopType[].class);
            return conDecl;
        } else if ((stuff = objType.getAsJsonArray("InfixConDecl")) != null) {
            InfixConDecl infixConDecl = new InfixConDecl();
            infixConDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            infixConDecl.b1 = jsonDeserializationContext.deserialize(stuff.get(1), BangTypeTopType.class);
            infixConDecl.name = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType.class);
            infixConDecl.b2 = jsonDeserializationContext.deserialize(stuff.get(3), BangTypeTopType.class);
            return infixConDecl;
        } else if ((stuff = objType.getAsJsonArray("RecDecl")) != null) {
            RecDecl recDecl = new RecDecl();
            recDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            recDecl.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            recDecl.fields = jsonDeserializationContext.deserialize(stuff.get(2), FieldDecl[].class);
            return recDecl;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

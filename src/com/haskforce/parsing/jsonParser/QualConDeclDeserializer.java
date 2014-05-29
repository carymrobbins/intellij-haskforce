package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ConDeclTopType;
import com.haskforce.parsing.srcExtsDatatypes.ContextTopType;
import com.haskforce.parsing.srcExtsDatatypes.QualConDecl;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TyVarBindTopType;

import java.lang.reflect.Type;

/**
 * Deserializes qualified constructor declarations.
 */
public class QualConDeclDeserializer implements JsonDeserializer<QualConDecl> {
    @Override
    public QualConDecl deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("QualConDecl")) != null) {
            QualConDecl qualConDecl = new QualConDecl();
            qualConDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qualConDecl.tyVarBinds = jsonDeserializationContext.deserialize(stuff.get(1), TyVarBindTopType[].class);
            qualConDecl.contextMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ContextTopType.class);
            qualConDecl.conDecl = jsonDeserializationContext.deserialize(stuff.get(3), ConDeclTopType.class);
            return qualConDecl;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

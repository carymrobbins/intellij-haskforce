package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.FieldPun;
import com.haskforce.parsing.srcExtsDatatypes.FieldUpdate;
import com.haskforce.parsing.srcExtsDatatypes.FieldUpdateTopType;
import com.haskforce.parsing.srcExtsDatatypes.FieldWildcard;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes FieldUpdates.
 */
public class FieldUpdateTopTypeDeserializer implements JsonDeserializer<FieldUpdateTopType> {
    @Override
    public FieldUpdateTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("FieldUpdate")) != null) {
            FieldUpdate fieldUpdate = new FieldUpdate();
            fieldUpdate.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            fieldUpdate.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            fieldUpdate.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return fieldUpdate;
        } else if ((stuff = objType.getAsJsonArray("FieldPun")) != null) {
            FieldPun fieldPun = new FieldPun();
            fieldPun.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            fieldPun.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return fieldPun;
        } else if ((stuff = objType.getAsJsonArray("FieldWildcard")) != null) {
            FieldWildcard fieldWildcard = new FieldWildcard();
            fieldWildcard.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return fieldWildcard;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

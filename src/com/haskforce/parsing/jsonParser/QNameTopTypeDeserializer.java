package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ModuleName;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.Qual;
import com.haskforce.parsing.srcExtsDatatypes.Special;
import com.haskforce.parsing.srcExtsDatatypes.SpecialConTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.UnQual;

import java.lang.reflect.Type;

/**
 * Deserializes qualified names.
 */
public class QNameTopTypeDeserializer implements JsonDeserializer<QNameTopType> {
    @Override
    public QNameTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("UnQual")) != null) {
            UnQual un = new UnQual();
            un.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            un.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return un;
        } else if ((stuff = objType.getAsJsonArray("Symbol")) != null) {
            Qual qual = new Qual();
            qual.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qual.moduleName = jsonDeserializationContext.deserialize(stuff.get(1), ModuleName.class);
            qual.name = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType.class);
            return qual;
        } else if ((stuff = objType.getAsJsonArray("Special")) != null) {
            Special special = new Special();
            special.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            special.specialCon = jsonDeserializationContext.deserialize(stuff.get(1), SpecialConTopType.class);
            return special;
        } else if ((stuff = objType.getAsJsonArray("Qual")) != null) {
            Qual un = new Qual();
            un.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            un.moduleName = jsonDeserializationContext.deserialize(stuff.get(1), ModuleName.class);
            un.name = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType.class);
            return un;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

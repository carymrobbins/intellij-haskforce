package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.KindTopType;
import com.haskforce.parsing.srcExtsDatatypes.KindedVar;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TyVarBindTopType;
import com.haskforce.parsing.srcExtsDatatypes.UnkindedVar;

import java.lang.reflect.Type;

/**
 * Deserializes type variable bindings.
 */
public class TyVarBindTopTypeDeserializer implements JsonDeserializer<TyVarBindTopType> {
    @Override
    public TyVarBindTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("KindedVar")) != null) {
            KindedVar kindedVar = new KindedVar();
            kindedVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            kindedVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            kindedVar.kind = jsonDeserializationContext.deserialize(stuff.get(2), KindTopType.class);
            return kindedVar;
        } else if ((stuff = objType.getAsJsonArray("UnkindedVar")) != null) {
            UnkindedVar unkindedVar = new UnkindedVar();
            unkindedVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            unkindedVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return unkindedVar;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

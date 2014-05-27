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
 * Deserializes Bangs.
 */
public class BangTypeTopTypeDeserializer implements JsonDeserializer<BangTypeTopType> {
    @Override
    public BangTypeTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("BangedTy")) != null) {
            BangedTy bangedTy = new BangedTy();
            bangedTy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            bangedTy.type = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            return bangedTy;
        } else if ((stuff = objType.getAsJsonArray("UnBangedTy")) != null) {
            UnBangedTy unBangedTy = new UnBangedTy();
            unBangedTy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            unBangedTy.type = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            return unBangedTy;
        } else if ((stuff = objType.getAsJsonArray("UnpackedTy")) != null) {
            UnpackedTy unpackedTy = new UnpackedTy();
            unpackedTy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            unpackedTy.type = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            return unpackedTy;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

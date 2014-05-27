package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BangTypeTopType;
import com.haskforce.parsing.srcExtsDatatypes.BangedTy;
import com.haskforce.parsing.srcExtsDatatypes.PromotedTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TypeTopType;
import com.haskforce.parsing.srcExtsDatatypes.UnBangedTy;
import com.haskforce.parsing.srcExtsDatatypes.UnpackedTy;

import java.lang.reflect.Type;

/**
 * Deserializes promotions.
 */
public class PromotedTopTypeDeserializer implements JsonDeserializer<PromotedTopType> {
    @Override
    public PromotedTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
/*        if ((stuff = objType.getAsJsonArray("BangedTy")) != null) {
            BangedTy bangedTy = new BangedTy();
            bangedTy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            bangedTy.type = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            return bangedTy;
        }*/
        // TODO: Add rest of PromotedTopType.
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

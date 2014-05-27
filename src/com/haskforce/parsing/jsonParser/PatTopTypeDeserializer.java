package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.PApp;
import com.haskforce.parsing.srcExtsDatatypes.PVar;
import com.haskforce.parsing.srcExtsDatatypes.PWildCard;
import com.haskforce.parsing.srcExtsDatatypes.PatTopType;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes patterns.
 */
public class PatTopTypeDeserializer implements JsonDeserializer<PatTopType> {
    @Override
    public PatTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        JsonObject stuff2;
        if ((stuff = objType.getAsJsonArray("PVar")) != null) {
            PVar pVar = new PVar();
            pVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return pVar;
        } else if ((stuff = objType.getAsJsonArray("PApp")) != null) {
            PApp pApp = new PApp();
            pApp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pApp.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            pApp.pats = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType[].class);
            return pApp;
        } else if ((stuff2 = objType.getAsJsonObject("PWildCard")) != null) {
            PWildCard pWildCard = new PWildCard();
            pWildCard.srcInfoSpan = jsonDeserializationContext.deserialize(stuff2.get("srcInfoSpan"), SrcInfoSpan.class);
            return pWildCard;
        }
        // TODO: Rest of data Pat
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

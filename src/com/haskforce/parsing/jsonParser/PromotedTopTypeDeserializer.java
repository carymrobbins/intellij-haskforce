package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

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
        if ((stuff = objType.getAsJsonArray("PromotedInteger")) != null) { // TODO: Test.
            PromotedInteger promotedInteger = new PromotedInteger();
            promotedInteger.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            promotedInteger.value = jsonDeserializationContext.deserialize(stuff.get(1), Integer.class);
            promotedInteger.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return promotedInteger;
        } else if ((stuff = objType.getAsJsonArray("PromotedString")) != null) { // TODO: Test.
            PromotedString promotedString = new PromotedString();
            promotedString.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            promotedString.value = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            promotedString.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return promotedString;
        } else if ((stuff = objType.getAsJsonArray("PromotedCon")) != null) {
            PromotedCon promotedCon = new PromotedCon();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            promotedCon.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            promotedCon.value = g.fromJson(stuff.get(1), boolean.class);
            promotedCon.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            return promotedCon;
        } else if ((stuff = objType.getAsJsonArray("PromotedList")) != null) {
            PromotedList promotedList = new PromotedList();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            promotedList.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            promotedList.leadingQuote = g.fromJson(stuff.get(1), boolean.class);
            promotedList.promoteds = jsonDeserializationContext.deserialize(stuff.get(2), PromotedTopType[].class);
            return promotedList;
        } else if ((stuff = objType.getAsJsonArray("PromotedTuple")) != null) {
            PromotedTuple promotedTuple = new PromotedTuple();
            promotedTuple.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            promotedTuple.promoteds = jsonDeserializationContext.deserialize(stuff.get(1), PromotedTopType[].class);
            return promotedTuple;
        } else if ((stuff = objType.getAsJsonArray("PromotedUnit")) != null) { // TODO: Test.
            PromotedUnit promotedUnit = new PromotedUnit();
            promotedUnit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return promotedUnit;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

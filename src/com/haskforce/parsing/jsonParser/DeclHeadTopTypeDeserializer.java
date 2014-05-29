package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.DHInfix;
import com.haskforce.parsing.srcExtsDatatypes.DHParen;
import com.haskforce.parsing.srcExtsDatatypes.DHead;
import com.haskforce.parsing.srcExtsDatatypes.DeclHeadTopType;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TyVarBindTopType;

import java.lang.reflect.Type;

/**
 * Deserializes declaration heads.
 */
public class DeclHeadTopTypeDeserializer implements JsonDeserializer<DeclHeadTopType> {
    @Override
    public DeclHeadTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("DHead")) != null) {
            DHead dHead = new DHead();
            dHead.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dHead.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            dHead.tyVars = jsonDeserializationContext.deserialize(stuff.get(2), TyVarBindTopType[].class);
            return dHead;
        } else if ((stuff = objType.getAsJsonArray("DHInfix")) != null) {
            DHInfix dhInfix = new DHInfix();
            dhInfix.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dhInfix.tb1 = jsonDeserializationContext.deserialize(stuff.get(1), TyVarBindTopType.class);
            dhInfix.name = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType[].class);
            dhInfix.tb2 = jsonDeserializationContext.deserialize(stuff.get(3), TyVarBindTopType.class);
            return dhInfix;
        } else if ((stuff = objType.getAsJsonArray("DHParen")) != null) { // TODO: Test.
            DHParen dhParen = new DHParen();
            dhParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            dhParen.declHead = jsonDeserializationContext.deserialize(stuff.get(1), DeclHeadTopType.class);
            return dhParen;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

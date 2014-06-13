package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.IHInfix;
import com.haskforce.parsing.srcExtsDatatypes.IHParen;
import com.haskforce.parsing.srcExtsDatatypes.IHead;
import com.haskforce.parsing.srcExtsDatatypes.InstHeadTopType;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TypeTopType;

import java.lang.reflect.Type;

/**
 * Deserializes instance heads.
 */
public class InstHeadTopTypeDeserializer implements JsonDeserializer<InstHeadTopType> {
    @Override
    public InstHeadTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("IHead")) != null) {
            IHead iHead = new IHead();
            iHead.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            iHead.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            iHead.types = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType[].class);
            return iHead;
        } else if ((stuff = objType.getAsJsonArray("IHInfix")) != null) {
            IHInfix ihInfix = new IHInfix();
            ihInfix.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ihInfix.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            ihInfix.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            ihInfix.t2 = jsonDeserializationContext.deserialize(stuff.get(3), TypeTopType.class);
            return ihInfix;
        } else if ((stuff = objType.getAsJsonArray("IHParen")) != null) {
            IHParen ihParen = new IHParen();
            ihParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ihParen.instHead = jsonDeserializationContext.deserialize(stuff.get(1), InstHeadTopType.class);
            return ihParen;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

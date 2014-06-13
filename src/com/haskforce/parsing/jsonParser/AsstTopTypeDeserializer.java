package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.AsstTopType;
import com.haskforce.parsing.srcExtsDatatypes.ClassA;
import com.haskforce.parsing.srcExtsDatatypes.EqualP;
import com.haskforce.parsing.srcExtsDatatypes.IPNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.IParam;
import com.haskforce.parsing.srcExtsDatatypes.InfixA;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TypeTopType;

import java.lang.reflect.Type;

/**
 * Deserializes AsstTopType..
 */
public class AsstTopTypeDeserializer implements JsonDeserializer<AsstTopType> {
    @Override
    public AsstTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ClassA")) != null) {
            ClassA classA = new ClassA();
            classA.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            classA.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            classA.types = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType[].class);
            return classA;
        } else if ((stuff = objType.getAsJsonArray("InfixA")) != null) {
            InfixA infixA = new InfixA();
            infixA.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            infixA.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            infixA.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            infixA.t2 = jsonDeserializationContext.deserialize(stuff.get(3), TypeTopType.class);
            return infixA;
        } else if ((stuff = objType.getAsJsonArray("IParam")) != null) {
            IParam iParam = new IParam();
            iParam.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            iParam.ipName = jsonDeserializationContext.deserialize(stuff.get(1), IPNameTopType.class);
            iParam.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return iParam;
        } else if ((stuff = objType.getAsJsonArray("EqualP")) != null) {
            EqualP equalP = new EqualP();
            equalP.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            equalP.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            equalP.t2 = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return equalP;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

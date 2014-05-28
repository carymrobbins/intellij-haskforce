package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ConOp;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.OpTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.VarOp;

import java.lang.reflect.Type;

/**
 * Deserializes ops.
 */
public class OpTopTypeDeserializer implements JsonDeserializer<OpTopType> {
    @Override
    public OpTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("VarOp")) != null) { // TODO: Test.
            VarOp varOp = new VarOp();
            varOp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            varOp.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return varOp;
        } else if ((stuff = objType.getAsJsonArray("ConOp")) != null) { // TODO: Test.
            ConOp conOp = new ConOp();
            conOp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            conOp.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return conOp;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

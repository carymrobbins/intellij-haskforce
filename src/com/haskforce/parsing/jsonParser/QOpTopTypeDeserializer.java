package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.QConOp;
import com.haskforce.parsing.srcExtsDatatypes.QNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.QOpTopType;
import com.haskforce.parsing.srcExtsDatatypes.QVarOp;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes qualified ops.
 */
public class QOpTopTypeDeserializer implements JsonDeserializer<QOpTopType> {
    @Override
    public QOpTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("QVarOp")) != null) {
            QVarOp qVarOp = new QVarOp();
            qVarOp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qVarOp.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return qVarOp;
        } else if ((stuff = objType.getAsJsonArray("QConOp")) != null) {
            QConOp qConOp = new QConOp();
            qConOp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qConOp.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            return qConOp;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

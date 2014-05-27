package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.DataOrNewTopType;
import com.haskforce.parsing.srcExtsDatatypes.DataType;
import com.haskforce.parsing.srcExtsDatatypes.NewType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes data/new-types.
 */
public class DataOrNewTopTypeDeserializer implements JsonDeserializer<DataOrNewTopType> {
    @Override
    public DataOrNewTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonObject stuff;
        if ((stuff = objType.getAsJsonObject("DataType")) != null) {
            DataType dataType = new DataType();
            dataType.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return dataType;
        } else if ((stuff = objType.getAsJsonObject("NewType")) != null) {
            NewType newType = new NewType();
            newType.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return newType;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

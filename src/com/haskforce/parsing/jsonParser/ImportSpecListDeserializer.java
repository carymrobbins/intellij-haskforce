package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ImportSpecList;
import com.haskforce.parsing.srcExtsDatatypes.ImportSpecTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes import specification lists.
 */
public class ImportSpecListDeserializer implements JsonDeserializer<ImportSpecList> {
    @Override
    public ImportSpecList deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("ImportSpecList")) != null) {
            ImportSpecList importSpecList = new ImportSpecList();
            Gson g = new Gson();
            importSpecList.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            importSpecList.hiding = g.fromJson(stuff.get(1), Boolean.class);
            importSpecList.importSpecs = jsonDeserializationContext.deserialize(stuff.get(2), ImportSpecTopType[].class);
            return importSpecList;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

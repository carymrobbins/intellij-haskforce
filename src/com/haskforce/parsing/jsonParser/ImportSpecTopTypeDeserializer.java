package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.CNameTopType;
import com.haskforce.parsing.srcExtsDatatypes.IAbs;
import com.haskforce.parsing.srcExtsDatatypes.IThingAll;
import com.haskforce.parsing.srcExtsDatatypes.IThingWith;
import com.haskforce.parsing.srcExtsDatatypes.IVar;
import com.haskforce.parsing.srcExtsDatatypes.ImportSpecTopType;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes import specifications.
 */
public class ImportSpecTopTypeDeserializer implements JsonDeserializer<ImportSpecTopType> {
    @Override
    public ImportSpecTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("IVar")) != null) {
            IVar id = new IVar();
            id.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            id.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return id;
        } else if ((stuff = objType.getAsJsonArray("IAbs")) != null) {
            IAbs id = new IAbs();
            id.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            id.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return id;
        } else if ((stuff = objType.getAsJsonArray("IThingAll")) != null) {
            IThingAll iThingAll = new IThingAll();
            iThingAll.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            iThingAll.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return iThingAll;
        } else if ((stuff = objType.getAsJsonArray("IThingWith")) != null) {
            IThingWith iThingWith = new IThingWith();
            iThingWith.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            iThingWith.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            iThingWith.cnames = jsonDeserializationContext.deserialize(stuff.get(2), CNameTopType[].class);
            return iThingWith;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

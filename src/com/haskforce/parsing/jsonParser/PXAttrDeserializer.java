package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.PXAttr;
import com.haskforce.parsing.srcExtsDatatypes.PatTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.XNameTopType;

import java.lang.reflect.Type;

/**
 * Deserializes PXAttrs.
 */
public class PXAttrDeserializer implements JsonDeserializer<PXAttr> {
    @Override
    public PXAttr deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("PXAttr")) != null) {
            PXAttr pxAttr = new PXAttr();
            pxAttr.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxAttr.xName = jsonDeserializationContext.deserialize(stuff.get(1), XNameTopType.class);
            pxAttr.pat = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType.class);
            return pxAttr;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

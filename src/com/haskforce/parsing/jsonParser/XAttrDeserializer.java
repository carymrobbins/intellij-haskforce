package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.XAttr;
import com.haskforce.parsing.srcExtsDatatypes.XNameTopType;

import java.lang.reflect.Type;

/**
 * Deserializes XAttrs.
 */
public class XAttrDeserializer implements JsonDeserializer<XAttr> {
    @Override
    public XAttr deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("XAttr")) != null) {
            XAttr xAttr = new XAttr();
            xAttr.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            xAttr.xName = jsonDeserializationContext.deserialize(stuff.get(1), XNameTopType.class);
            xAttr.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return xAttr;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

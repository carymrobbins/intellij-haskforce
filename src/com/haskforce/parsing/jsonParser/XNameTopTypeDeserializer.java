package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.XDomName;
import com.haskforce.parsing.srcExtsDatatypes.XName;
import com.haskforce.parsing.srcExtsDatatypes.XNameTopType;

import java.lang.reflect.Type;

/**
 * Deserializes XNames.
 */
public class XNameTopTypeDeserializer implements JsonDeserializer<XNameTopType> {
    @Override
    public XNameTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("XName")) != null) { // TODO: Test.
            XName xName = new XName();
            xName.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            xName.s = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return xName;
        } else if ((stuff = objType.getAsJsonArray("XDomName")) != null) { // TODO: Test.
            XDomName xDomName = new XDomName();
            xDomName.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            xDomName.s1 = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            xDomName.s2 = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return xDomName;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

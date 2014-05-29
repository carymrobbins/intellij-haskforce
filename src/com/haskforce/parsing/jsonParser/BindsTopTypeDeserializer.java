package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BDecls;
import com.haskforce.parsing.srcExtsDatatypes.BindsTopType;
import com.haskforce.parsing.srcExtsDatatypes.DeclTopType;
import com.haskforce.parsing.srcExtsDatatypes.IPBind;
import com.haskforce.parsing.srcExtsDatatypes.IPBinds;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes bindings.
 */
public class BindsTopTypeDeserializer implements JsonDeserializer<BindsTopType> {
    @Override
    public BindsTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("BDecls")) != null) { // TODO: Test.
            BDecls bDecls = new BDecls();
            bDecls.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            bDecls.decls = jsonDeserializationContext.deserialize(stuff.get(1), DeclTopType[].class);
            return bDecls;
        } else if ((stuff = objType.getAsJsonArray("TypeAnn")) != null) { // TODO: Test.
            IPBinds ipBinds = new IPBinds();
            ipBinds.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ipBinds.ipBinds = jsonDeserializationContext.deserialize(stuff.get(1), IPBind[].class);
            return ipBinds;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Ann;
import com.haskforce.parsing.srcExtsDatatypes.AnnotationTopType;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.ModuleAnn;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TypeAnn;

import java.lang.reflect.Type;

/**
 * Deserializes annotations.
 */
public class AnnotationTopTypeDeserializer implements JsonDeserializer<AnnotationTopType> {
    @Override
    public AnnotationTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Ann")) != null) { // TODO: Test.
            Ann ann = new Ann();
            ann.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ann.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            ann.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return ann;
        } else if ((stuff = objType.getAsJsonArray("TypeAnn")) != null) { // TODO: Test.
            TypeAnn typeAnn = new TypeAnn();
            typeAnn.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typeAnn.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            typeAnn.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return typeAnn;
        } else if ((stuff = objType.getAsJsonArray("ModuleAnn")) != null) { // TODO: Test.
            ModuleAnn moduleAnn = new ModuleAnn();
            moduleAnn.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            moduleAnn.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return moduleAnn;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

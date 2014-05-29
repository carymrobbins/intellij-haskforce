package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.AnnModulePragma;
import com.haskforce.parsing.srcExtsDatatypes.AnnotationTopType;
import com.haskforce.parsing.srcExtsDatatypes.LanguagePragma;
import com.haskforce.parsing.srcExtsDatatypes.ModulePragmaTopType;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.OptionsPragma;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.ToolTopType;

import java.lang.reflect.Type;

/**
 * Deserializes module pragmas.
 */
public class ModulePragmaTopTypeDeserializer implements JsonDeserializer<ModulePragmaTopType> {
    @Override
    public ModulePragmaTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("LanguagePragma")) != null) {
            LanguagePragma languagePragma = new LanguagePragma();
            languagePragma.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            languagePragma.names = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType[].class);
            return languagePragma;
        } else if ((stuff = objType.getAsJsonArray("OptionsPragma")) != null) { // TODO: Test.
            OptionsPragma optionsPragma = new OptionsPragma();
            optionsPragma.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            optionsPragma.tool = jsonDeserializationContext.deserialize(stuff.get(1), ToolTopType.class);
            optionsPragma.s = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return optionsPragma;
        } else if ((stuff = objType.getAsJsonArray("AnnModulePragma")) != null) { // TODO: Test.
            AnnModulePragma annModulePragma = new AnnModulePragma();
            annModulePragma.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            annModulePragma.ann = jsonDeserializationContext.deserialize(stuff.get(1), AnnotationTopType.class);
            return annModulePragma;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

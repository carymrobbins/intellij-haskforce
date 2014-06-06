package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.DeprText;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.WarnText;
import com.haskforce.parsing.srcExtsDatatypes.WarningTextTopType;

import java.lang.reflect.Type;

/**
 * Deserializes warning texts.
 */
public class WarningTextTopTypeDeserializer implements JsonDeserializer<WarningTextTopType> {
    @Override
    public WarningTextTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("DeprText")) != null) {
            DeprText deprText = new DeprText();
            deprText.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            deprText.text = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return deprText;
        } else if ((stuff = objType.getAsJsonArray("WarnText")) != null) {
            WarnText warnText = new WarnText();
            warnText.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            warnText.text = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return warnText;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

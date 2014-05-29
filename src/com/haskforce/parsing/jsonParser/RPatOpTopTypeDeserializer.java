package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.RPOpt;
import com.haskforce.parsing.srcExtsDatatypes.RPOptG;
import com.haskforce.parsing.srcExtsDatatypes.RPPlus;
import com.haskforce.parsing.srcExtsDatatypes.RPPlusG;
import com.haskforce.parsing.srcExtsDatatypes.RPStar;
import com.haskforce.parsing.srcExtsDatatypes.RPStarG;
import com.haskforce.parsing.srcExtsDatatypes.RPatOpTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes Activations.
 */
public class RPatOpTopTypeDeserializer implements JsonDeserializer<RPatOpTopType> {
    @Override
    public RPatOpTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("RPStar")) != null) { // TODO: Test.
            RPStar rpStar = new RPStar();
            rpStar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpStar;
        } else if ((stuff = objType.getAsJsonArray("RPStarG")) != null) { // TODO: Test.
            RPStarG rpStarG = new RPStarG();
            rpStarG.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpStarG;
        } else if ((stuff = objType.getAsJsonArray("RPPlus")) != null) { // TODO: Test.
            RPPlus rpPlus = new RPPlus();
            rpPlus.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpPlus;
        } else if ((stuff = objType.getAsJsonArray("RPPlusG")) != null) { // TODO: Test.
            RPPlusG rpPlus = new RPPlusG();
            rpPlus.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpPlus;
        } else if ((stuff = objType.getAsJsonArray("RPOpt")) != null) { // TODO: Test.
            RPOpt rpOpt = new RPOpt();
            rpOpt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpOpt;
        } else if ((stuff = objType.getAsJsonArray("RPOptG")) != null) { // TODO: Test.
            RPOptG rpOptG = new RPOptG();
            rpOptG.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            return rpOptG;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes RuleVars.
 */
public class RuleVarTopTypeDeserializer implements JsonDeserializer<RuleVarTopType> {
    @Override
    public RuleVarTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("RuleVar")) != null) { // TODO: Test.
            RuleVar ruleVar = new RuleVar();
            ruleVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            ruleVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return ruleVar;
        } else if ((stuff = objType.getAsJsonArray("TypedRuleVar")) != null) { // TODO: Test.
            TypedRuleVar typedRuleVar = new TypedRuleVar();
            typedRuleVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            typedRuleVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            typedRuleVar.name = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return typedRuleVar;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

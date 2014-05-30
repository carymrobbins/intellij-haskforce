package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ActivationTopType;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.Rule;
import com.haskforce.parsing.srcExtsDatatypes.RuleVarTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes rules.
 */
public class RuleDeserializer implements JsonDeserializer<Rule> {
    @Override
    public Rule deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Rule")) != null) {
            Rule rule = new Rule();
            rule.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            rule.s = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            rule.activationMaybe = jsonDeserializationContext.deserialize(stuff.get(2), ActivationTopType.class);
            rule.ruleVars = jsonDeserializationContext.deserialize(stuff.get(3), RuleVarTopType[].class);
            rule.e1 = jsonDeserializationContext.deserialize(stuff.get(4), ExpTopType.class);
            rule.e2 = jsonDeserializationContext.deserialize(stuff.get(5), ExpTopType.class);
            return rule;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

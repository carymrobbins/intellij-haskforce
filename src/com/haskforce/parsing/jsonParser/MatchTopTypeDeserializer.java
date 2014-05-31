package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BindsTopType;
import com.haskforce.parsing.srcExtsDatatypes.InfixMatch;
import com.haskforce.parsing.srcExtsDatatypes.Match;
import com.haskforce.parsing.srcExtsDatatypes.MatchTopType;
import com.haskforce.parsing.srcExtsDatatypes.NameTopType;
import com.haskforce.parsing.srcExtsDatatypes.PatTopType;
import com.haskforce.parsing.srcExtsDatatypes.RhsTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes matches.
 */
public class MatchTopTypeDeserializer implements JsonDeserializer<MatchTopType> {
    @Override
    public MatchTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Match")) != null) {
            Match match = new Match();
            match.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            match.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            match.pats = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType[].class);
            match.rhs = jsonDeserializationContext.deserialize(stuff.get(3), RhsTopType.class);
            match.bindsMaybe = jsonDeserializationContext.deserialize(stuff.get(4), BindsTopType.class);
            return match;
        } else if ((stuff = objType.getAsJsonArray("InfixMatch")) != null) {
            InfixMatch infixMatch = new InfixMatch();
            infixMatch.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            infixMatch.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            infixMatch.name = jsonDeserializationContext.deserialize(stuff.get(2), NameTopType.class);
            infixMatch.pats = jsonDeserializationContext.deserialize(stuff.get(3), PatTopType[].class);
            infixMatch.rhs = jsonDeserializationContext.deserialize(stuff.get(4), RhsTopType.class);
            infixMatch.bindsMaybe = jsonDeserializationContext.deserialize(stuff.get(5), BindsTopType.class);
            return infixMatch;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

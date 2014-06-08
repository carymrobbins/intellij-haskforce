package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BindsTopType;
import com.haskforce.parsing.srcExtsDatatypes.ExpTopType;
import com.haskforce.parsing.srcExtsDatatypes.Generator;
import com.haskforce.parsing.srcExtsDatatypes.LetStmt;
import com.haskforce.parsing.srcExtsDatatypes.PatTopType;
import com.haskforce.parsing.srcExtsDatatypes.Qualifier;
import com.haskforce.parsing.srcExtsDatatypes.RecStmt;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.StmtTopType;

import java.lang.reflect.Type;

/**
 * Deserializes statements.
 */
public class StmtTopTypeDeserializer implements JsonDeserializer<StmtTopType> {
    @Override
    public StmtTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Generator")) != null) {
            Generator generator = new Generator();
            generator.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            generator.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            generator.exp = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return generator;
        } else if ((stuff = objType.getAsJsonArray("Qualifier")) != null) {
            Qualifier qualifier = new Qualifier();
            qualifier.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qualifier.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return qualifier;
        } else if ((stuff = objType.getAsJsonArray("LetStmt")) != null) {
            LetStmt letStmt = new LetStmt();
            letStmt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            letStmt.binds = jsonDeserializationContext.deserialize(stuff.get(1), BindsTopType.class);
            return letStmt;
        } else if ((stuff = objType.getAsJsonArray("RecStmt")) != null) {
            RecStmt recStmt = new RecStmt();
            recStmt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            recStmt.stmts = jsonDeserializationContext.deserialize(stuff.get(1), StmtTopType[].class);
            return recStmt;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

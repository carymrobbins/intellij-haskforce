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
 * Deserializes instance declarations.
 */
public class InstDeclTopTypeDeserializer implements JsonDeserializer<InstDeclTopType> {
    @Override
    public InstDeclTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("InsDecl")) != null) {
            InsDecl insDecl = new InsDecl();
            insDecl.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            insDecl.decl = jsonDeserializationContext.deserialize(stuff.get(1), DeclTopType.class);
            return insDecl;
        } else if ((stuff = objType.getAsJsonArray("InsType")) != null) {
            InsType insType = new InsType();
            insType.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            insType.t1 = jsonDeserializationContext.deserialize(stuff.get(1), TypeTopType.class);
            insType.t2 = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return insType;
        } else if ((stuff = objType.getAsJsonArray("InsData")) != null) {
            InsData insData = new InsData();
            insData.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            insData.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            insData.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            insData.qualConDecls = jsonDeserializationContext.deserialize(stuff.get(3), QualConDecl[].class);
            insData.derivingMaybe = jsonDeserializationContext.deserialize(stuff.get(4), Deriving.class);
            return insData;
        } else if ((stuff = objType.getAsJsonArray("InsGData")) != null) { // TODO: Test.
            InsGData insGData = new InsGData();
            insGData.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            insGData.dataOrNew = jsonDeserializationContext.deserialize(stuff.get(1), DataOrNewTopType.class);
            insGData.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            insGData.kindMaybe = jsonDeserializationContext.deserialize(stuff.get(3), KindTopType.class);
            insGData.gadtDecls = jsonDeserializationContext.deserialize(stuff.get(4), GadtDecl[].class);
            insGData.derivingMaybe = jsonDeserializationContext.deserialize(stuff.get(5), Deriving.class);
            return insGData;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

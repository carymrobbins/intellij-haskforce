package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.BoxedTopType;
import com.haskforce.parsing.srcExtsDatatypes.Cons;
import com.haskforce.parsing.srcExtsDatatypes.FunCon;
import com.haskforce.parsing.srcExtsDatatypes.ListCon;
import com.haskforce.parsing.srcExtsDatatypes.SpecialConTopType;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;
import com.haskforce.parsing.srcExtsDatatypes.TupleCon;
import com.haskforce.parsing.srcExtsDatatypes.UnboxedSingleCon;
import com.haskforce.parsing.srcExtsDatatypes.UnitCon;

import java.lang.reflect.Type;

/**
 * Deserializes special constructors.
 */
public class SpecialConTopTypeDeserializer implements JsonDeserializer<SpecialConTopType> {
    @Override
    public SpecialConTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if (objType.has("UnitCon")) {
            UnitCon unitCon = new UnitCon();
            unitCon.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("UnitCon"), SrcInfoSpan.class);
            return unitCon;
        } else if (objType.has("ListCon")) { // TODO: Test.
            ListCon listCon = new ListCon();
            listCon.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("ListCon"), SrcInfoSpan.class);
            return listCon;
        } else if (objType.has("FunCon")) { // TODO: Test.
            FunCon funCon = new FunCon();
            funCon.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("FunCon"), SrcInfoSpan.class);
            return funCon;
        } else if ((stuff = objType.getAsJsonArray("TupleCon")) != null) { // TODO: Test.
            TupleCon tupleCon = new TupleCon();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            tupleCon.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            tupleCon.boxed = jsonDeserializationContext.deserialize(stuff.get(1), BoxedTopType.class);
            tupleCon.i = g.fromJson(stuff.get(1), int.class);
            return tupleCon;
        } else if (objType.has("Cons")) { // TODO: Test.
            Cons cons = new Cons();
            cons.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("Cons"), SrcInfoSpan.class);
            return cons;
        } else if (objType.has("UnboxedSingleCon")) { // TODO: Test.
            UnboxedSingleCon unboxedSingleCon = new UnboxedSingleCon();
            unboxedSingleCon.srcInfoSpan = jsonDeserializationContext.deserialize(objType.get("UnboxedSingleCon"), SrcInfoSpan.class);
            return unboxedSingleCon;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

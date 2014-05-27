package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes literals.
 */
public class LiteralTopTypeDeserializer implements JsonDeserializer<LiteralTopType> {
    @Override
    public LiteralTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Char")) != null) { // TODO: Test.
            CharLit charLit = new CharLit();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            charLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            charLit.value = g.fromJson(stuff.get(1), char.class);
            charLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return charLit;
        } else if ((stuff = objType.getAsJsonArray("String")) != null) {
            StringLit stringLit = new StringLit();
            stringLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            stringLit.value = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            stringLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return stringLit;
        } else if ((stuff = objType.getAsJsonArray("Int")) != null) {
            IntLit intLit = new IntLit();
            intLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            intLit.value = jsonDeserializationContext.deserialize(stuff.get(1), Integer.class);
            intLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return intLit;
        } else if ((stuff = objType.getAsJsonArray("Frac")) != null) { // TODO: Test.
            FracLit fracLit = new FracLit();
            Gson g = new Gson();  // TODO: Remove with 1.7.
            fracLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            fracLit.value = g.fromJson(stuff.get(1), float.class);
            fracLit.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return fracLit;
        } else if ((stuff = objType.getAsJsonArray("PrimInt")) != null) { // TODO: Test.
            PrimInt primInt = new PrimInt();
            primInt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primInt.value = jsonDeserializationContext.deserialize(stuff.get(1), Integer.class);
            primInt.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primInt;
        } else if ((stuff = objType.getAsJsonArray("PrimWord")) != null) { // TODO: Test.
            PrimWord primWord = new PrimWord();
            primWord.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primWord.value = jsonDeserializationContext.deserialize(stuff.get(1), Integer.class);
            primWord.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primWord;
        } else if ((stuff = objType.getAsJsonArray("PrimFloat")) != null) { // TODO: Test.
            PrimFloat primFloat = new PrimFloat();
            Gson g = new Gson();  // TODO: Remove with 1.7.
            primFloat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primFloat.value = g.fromJson(stuff.get(1), float.class);
            primFloat.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primFloat;
        } else if ((stuff = objType.getAsJsonArray("PrimDouble")) != null) { // TODO: Test.
            PrimDouble primDouble = new PrimDouble();
            Gson g = new Gson();  // TODO: Remove with 1.7.
            primDouble.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primDouble.value = g.fromJson(stuff.get(1), double.class);
            primDouble.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primDouble;
        } else if ((stuff = objType.getAsJsonArray("PrimChar")) != null) { // TODO: Test.
            PrimChar primChar = new PrimChar();
            Gson g = new Gson();  // TODO: Remove with 1.7.
            primChar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primChar.value = g.fromJson(stuff.get(1), char.class);
            primChar.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primChar;
        }  else if ((stuff = objType.getAsJsonArray("PrimString")) != null) { // TODO: Test.
            PrimString primString = new PrimString();
            primString.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            primString.value = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            primString.representation = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return primString;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

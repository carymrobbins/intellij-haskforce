package com.haskforce.parsing.jsonParser;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.*;

import java.lang.reflect.Type;

/**
 * Deserializes calling conventions.
 */
public class CallConvTopTypeDeserializer implements JsonDeserializer<CallConvTopType> {
    @Override
    public CallConvTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonObject stuff;
        if ((stuff = objType.getAsJsonObject("StdCall")) != null) {
            StdCall stdCall = new StdCall();
            stdCall.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return stdCall;
        } else if ((stuff = objType.getAsJsonObject("CCall")) != null) {
            CCall cCall = new CCall();
            cCall.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return cCall;
        } else if ((stuff = objType.getAsJsonObject("CPlusPlus")) != null) {
            CPlusPlus cPlusPlus = new CPlusPlus();
            cPlusPlus.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return cPlusPlus;
        } else if ((stuff = objType.getAsJsonObject("DotNet")) != null) {
            DotNet dotNet = new DotNet();
            dotNet.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return dotNet;
        } else if ((stuff = objType.getAsJsonObject("Jvm")) != null) {
            Jvm jvm = new Jvm();
            jvm.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return jvm;
        } else if ((stuff = objType.getAsJsonObject("Js")) != null) {
            Js js = new Js();
            js.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return js;
        } else if ((stuff = objType.getAsJsonObject("CApi")) != null) {
            CApi cApi = new CApi();
            cApi.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get("srcInfoSpan"), SrcInfoSpan.class);
            return cApi;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

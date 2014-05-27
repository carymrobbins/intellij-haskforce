package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.Comment;
import com.haskforce.parsing.srcExtsDatatypes.SrcSpan;

import java.lang.reflect.Type;

/**
 * Deserializes comments.
 */
public class CommentDeserializer implements JsonDeserializer<Comment> {
    @Override
    public Comment deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("Comment")) != null) {
            Comment comment = new Comment();
            Gson g = new Gson(); // TODO: Remove with 1.7.
            comment.multiline = g.fromJson(stuff.get(0), Boolean.class);
            comment.loc = jsonDeserializationContext.deserialize(stuff.get(1), SrcSpan.class);
            comment.text = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return comment;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

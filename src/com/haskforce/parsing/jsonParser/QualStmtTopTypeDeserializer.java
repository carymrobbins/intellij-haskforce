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
 * Deserializes qualified statements.
 */
public class QualStmtTopTypeDeserializer implements JsonDeserializer<QualStmtTopType> {
    @Override
    public QualStmtTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        if ((stuff = objType.getAsJsonArray("QualStmt")) != null) { // TODO: Test.
            QualStmt qualStmt = new QualStmt();
            qualStmt.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            qualStmt.stmt = jsonDeserializationContext.deserialize(stuff.get(1), StmtTopType.class);
            return qualStmt;
        } else if ((stuff = objType.getAsJsonArray("ThenTrans")) != null) { // TODO: Test.
            ThenTrans thenTrans = new ThenTrans();
            thenTrans.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            thenTrans.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return thenTrans;
        } else if ((stuff = objType.getAsJsonArray("ThenBy")) != null) { // TODO: Test.
            ThenBy thenBy = new ThenBy();
            thenBy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            thenBy.e1 = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            thenBy.e2 = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return thenBy;
        } else if ((stuff = objType.getAsJsonArray("GroupBy")) != null) { // TODO: Test.
            GroupBy groupBy = new GroupBy();
            groupBy.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            groupBy.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return groupBy;
        } else if ((stuff = objType.getAsJsonArray("GroupUsing")) != null) { // TODO: Test.
            GroupUsing groupUsing = new GroupUsing();
            groupUsing.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            groupUsing.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            return groupUsing;
        } else if ((stuff = objType.getAsJsonArray("GroupByUsing")) != null) { // TODO: Test.
            GroupByUsing groupByUsing = new GroupByUsing();
            groupByUsing.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            groupByUsing.e1 = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            groupByUsing.e2 = jsonDeserializationContext.deserialize(stuff.get(2), ExpTopType.class);
            return groupByUsing;
        }
        throw new JsonParseException("Unexpected JSON object type: " + objType.toString());
    }
}

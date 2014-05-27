package com.haskforce.parsing.jsonParser;

import com.google.gson.Gson;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.haskforce.parsing.srcExtsDatatypes.ImportDecl;
import com.haskforce.parsing.srcExtsDatatypes.ImportSpecList;
import com.haskforce.parsing.srcExtsDatatypes.ModuleName;
import com.haskforce.parsing.srcExtsDatatypes.SrcInfoSpan;

import java.lang.reflect.Type;

/**
 * Deserializes import declarations.
 */
public class ImportDeclDeserializer implements JsonDeserializer<ImportDecl> {
    @Override
    public ImportDecl deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject stuff = jsonElement.getAsJsonObject();
        ImportDecl importDecl = new ImportDecl();
        Gson g = new Gson(); // TODO: Remove with 1.7.
        importDecl.importAnn = jsonDeserializationContext.deserialize(stuff.get("importAnn"), SrcInfoSpan.class);
        importDecl.importModule = jsonDeserializationContext.deserialize(stuff.get("importModule"), ModuleName.class);
        importDecl.importQualified = g.fromJson(stuff.get("importQualified"), Boolean.class);
        importDecl.importSrc = g.fromJson(stuff.get("importSrc"), Boolean.class);
        importDecl.importPkg = jsonDeserializationContext.deserialize(stuff.get("importPkg"), String.class);
        importDecl.importAs = jsonDeserializationContext.deserialize(stuff.get("importAs"), ModuleName.class);
        importDecl.importSpecs = jsonDeserializationContext.deserialize(stuff.get("importSpecs"), ImportSpecList.class);
        return importDecl;
    }
}

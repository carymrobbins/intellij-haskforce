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
 * Deserializes patterns.
 */
public class PatTopTypeDeserializer implements JsonDeserializer<PatTopType> {
    @Override
    public PatTopType deserialize(JsonElement jsonElement, Type type,
                                     JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
        JsonObject objType = jsonElement.getAsJsonObject();
        JsonArray stuff;
        JsonObject stuff2;
        if ((stuff = objType.getAsJsonArray("PVar")) != null) {
            PVar pVar = new PVar();
            pVar.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pVar.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            return pVar;
        } else if ((stuff = objType.getAsJsonArray("PLit")) != null) {
            PLit pLit = new PLit();
            pLit.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pLit.lit = jsonDeserializationContext.deserialize(stuff.get(1), LiteralTopType.class);
            return pLit;
        } else if ((stuff = objType.getAsJsonArray("PNeg")) != null) { // TODO: Test.
            PNeg pNeg = new PNeg();
            pNeg.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pNeg.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return pNeg;
        } else if ((stuff = objType.getAsJsonArray("PNPlusK")) != null) { // TODO: Test.
            PNPlusK pnPlusK = new PNPlusK();
            pnPlusK.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pnPlusK.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            pnPlusK.i = jsonDeserializationContext.deserialize(stuff.get(2), Integer.class);
            return pnPlusK;
        } else if ((stuff = objType.getAsJsonArray("PInfixApp")) != null) {
            PInfixApp pInfixApp = new PInfixApp();
            pInfixApp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pInfixApp.p1 = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            pInfixApp.qName = jsonDeserializationContext.deserialize(stuff.get(2), QNameTopType.class);
            pInfixApp.p2 = jsonDeserializationContext.deserialize(stuff.get(3), PatTopType.class);
            return pInfixApp;
        } else if ((stuff = objType.getAsJsonArray("PApp")) != null) {
            PApp pApp = new PApp();
            pApp.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pApp.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            pApp.pats = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType[].class);
            return pApp;
        } else if ((stuff = objType.getAsJsonArray("PTuple")) != null) {
            PTuple pTuple = new PTuple();
            pTuple.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pTuple.boxed = jsonDeserializationContext.deserialize(stuff.get(1), Boxed.class);
            pTuple.pats = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType[].class);
            return pTuple;
        } else if ((stuff = objType.getAsJsonArray("PList")) != null) {
            PList pList = new PList();
            pList.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pList.pats = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType[].class);
            return pList;
        } else if ((stuff = objType.getAsJsonArray("PParen")) != null) {
            PParen pParen = new PParen();
            pParen.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pParen.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return pParen;
        } else if ((stuff = objType.getAsJsonArray("PRec")) != null) { // TODO: Test.
            PRec pRec = new PRec();
            pRec.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pRec.qName = jsonDeserializationContext.deserialize(stuff.get(1), QNameTopType.class);
            pRec.patFields = jsonDeserializationContext.deserialize(stuff.get(2), PatFieldTopType[].class);
            return pRec;
        } else if ((stuff = objType.getAsJsonArray("PAsPat")) != null) { // TODO: Test.
            PAsPat pAsPat = new PAsPat();
            pAsPat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pAsPat.name = jsonDeserializationContext.deserialize(stuff.get(1), NameTopType.class);
            pAsPat.pat = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType.class);
            return pAsPat;
        } else if ((stuff2 = objType.getAsJsonObject("PWildCard")) != null) {
            PWildCard pWildCard = new PWildCard();
            pWildCard.srcInfoSpan = jsonDeserializationContext.deserialize(stuff2.get("srcInfoSpan"), SrcInfoSpan.class);
            return pWildCard;
        } else if ((stuff = objType.getAsJsonArray("PIrrPat")) != null) { // TODO: Test.
            PIrrPat pIrrPat = new PIrrPat();
            pIrrPat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pIrrPat.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return pIrrPat;
        } else if ((stuff = objType.getAsJsonArray("PatTypeSig")) != null) { // TODO: Test.
            PatTypeSig patTypeSig = new PatTypeSig();
            patTypeSig.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            patTypeSig.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            patTypeSig.type = jsonDeserializationContext.deserialize(stuff.get(2), TypeTopType.class);
            return patTypeSig;
        } else if ((stuff = objType.getAsJsonArray("PViewPat")) != null) { // TODO: Test.
            PViewPat pViewPat = new PViewPat();
            pViewPat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pViewPat.exp = jsonDeserializationContext.deserialize(stuff.get(1), ExpTopType.class);
            pViewPat.pat = jsonDeserializationContext.deserialize(stuff.get(2), PatTopType.class);
            return pViewPat;
        } else if ((stuff = objType.getAsJsonArray("PRPat")) != null) { // TODO: Test.
            PRPat prPat = new PRPat();
            prPat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            prPat.rpats = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType[].class);
            return prPat;
        } else if ((stuff = objType.getAsJsonArray("PXTag")) != null) { // TODO: Test.
            PXTag pxTag = new PXTag();
            pxTag.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxTag.xName = jsonDeserializationContext.deserialize(stuff.get(1), XNameTopType.class);
            pxTag.pxAttrs = jsonDeserializationContext.deserialize(stuff.get(2), PXAttr[].class);
            pxTag.patMaybe = jsonDeserializationContext.deserialize(stuff.get(3), PatTopType.class);
            pxTag.pats = jsonDeserializationContext.deserialize(stuff.get(4), PatTopType[].class);
            return pxTag;
        } else if ((stuff = objType.getAsJsonArray("PXETag")) != null) { // TODO: Test.
            PXETag pxeTag = new PXETag();
            pxeTag.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxeTag.xName = jsonDeserializationContext.deserialize(stuff.get(1), XNameTopType.class);
            pxeTag.pxAttrs = jsonDeserializationContext.deserialize(stuff.get(2), PXAttr[].class);
            pxeTag.patMaybe = jsonDeserializationContext.deserialize(stuff.get(3), PatTopType.class);
            return pxeTag;
        } else if ((stuff = objType.getAsJsonArray("PXPcdata")) != null) { // TODO: Test.
            PXPcdata pxPcdata = new PXPcdata();
            pxPcdata.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxPcdata.s = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            return pxPcdata;
        } else if ((stuff = objType.getAsJsonArray("PXPatTag")) != null) { // TODO: Test.
            PXPatTag pxPatTag = new PXPatTag();
            pxPatTag.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxPatTag.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return pxPatTag;
        } else if ((stuff = objType.getAsJsonArray("PXRPats")) != null) { // TODO: Test.
            PXRPats pxrPats = new PXRPats();
            pxrPats.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pxrPats.rPats = jsonDeserializationContext.deserialize(stuff.get(1), RPatTopType[].class);
            return pxrPats;
        } else if ((stuff = objType.getAsJsonArray("PQuasiQuote")) != null) { // TODO: Test.
            PQuasiQuote pQuasiQuote = new PQuasiQuote();
            pQuasiQuote.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pQuasiQuote.s1 = jsonDeserializationContext.deserialize(stuff.get(1), String.class);
            pQuasiQuote.s2 = jsonDeserializationContext.deserialize(stuff.get(2), String.class);
            return pQuasiQuote;
        } else if ((stuff = objType.getAsJsonArray("PBangPat")) != null) { // TODO: Test.
            PBangPat pBangPat = new PBangPat();
            pBangPat.srcInfoSpan = jsonDeserializationContext.deserialize(stuff.get(0), SrcInfoSpan.class);
            pBangPat.pat = jsonDeserializationContext.deserialize(stuff.get(1), PatTopType.class);
            return pBangPat;
        }
        throw new JsonParseException("Unexpected object type: " + objType.toString());
    }
}

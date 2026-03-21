import * as Data$dMaybe from "../Data.Maybe/index.js";
const $RequestBody = (tag, _1) => ({tag, _1});
const ArrayView = value0 => $RequestBody("ArrayView", value0);
const Blob = value0 => $RequestBody("Blob", value0);
const Document = value0 => $RequestBody("Document", value0);
const $$String = value0 => $RequestBody("String", value0);
const FormData = value0 => $RequestBody("FormData", value0);
const FormURLEncoded = value0 => $RequestBody("FormURLEncoded", value0);
const Json = value0 => $RequestBody("Json", value0);
const toMediaType = v => {
  if (v.tag === "FormURLEncoded") { return Data$dMaybe.$Maybe("Just", "application/x-www-form-urlencoded"); }
  if (v.tag === "Json") { return Data$dMaybe.$Maybe("Just", "application/json"); }
  return Data$dMaybe.Nothing;
};
const string = $$String;
const json = Json;
const formURLEncoded = FormURLEncoded;
const formData = FormData;
const document = Document;
const blob = Blob;
const arrayView = av => $RequestBody("ArrayView", f => f(av));
export {$RequestBody, ArrayView, Blob, Document, FormData, FormURLEncoded, Json, $$String as String, arrayView, blob, document, formData, formURLEncoded, json, string, toMediaType};

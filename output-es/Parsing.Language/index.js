// | This module is a port of the Haskell
// | [__Text.Parsec.Language__](https://hackage.haskell.org/package/parsec/docs/Text-Parsec-Language.html)
// | module.
import * as Parsing from "../Parsing/index.js";
import * as Parsing$dCombinators from "../Parsing.Combinators/index.js";
import * as Parsing$dString from "../Parsing.String/index.js";
import * as Parsing$dString$dBasic from "../Parsing.String.Basic/index.js";
import * as Parsing$dToken from "../Parsing.Token/index.js";
const emptyDef = /* #__PURE__ */ (() => {
  const op$p = Parsing$dString$dBasic.oneOf([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
  return {
    commentStart: "",
    commentEnd: "",
    commentLine: "",
    nestedComments: true,
    identStart: (() => {
      const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "_"))("'_'");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => Parsing$dString$dBasic.letter(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    identLetter: (() => {
      const $0 = Parsing$dString$dBasic.oneOf(["_", "'"]);
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => Parsing$dString$dBasic.alphaNum(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    opStart: op$p,
    opLetter: op$p,
    reservedOpNames: [],
    reservedNames: [],
    caseSensitive: true
  };
})();
const haskellStyle = /* #__PURE__ */ (() => {
  const op$p = Parsing$dString$dBasic.oneOf([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
  return {
    commentStart: "{-",
    commentEnd: "-}",
    commentLine: "--",
    nestedComments: true,
    identStart: Parsing$dString$dBasic.letter,
    identLetter: (() => {
      const $0 = Parsing$dString$dBasic.oneOf(["_", "'"]);
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => Parsing$dString$dBasic.alphaNum(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    opStart: op$p,
    opLetter: op$p,
    reservedNames: [],
    reservedOpNames: [],
    caseSensitive: true
  };
})();
const haskell98Def = /* #__PURE__ */ (() => (
  {
    commentStart: haskellStyle.commentStart,
    commentEnd: haskellStyle.commentEnd,
    commentLine: haskellStyle.commentLine,
    nestedComments: haskellStyle.nestedComments,
    identStart: haskellStyle.identStart,
    identLetter: haskellStyle.identLetter,
    opStart: haskellStyle.opStart,
    opLetter: haskellStyle.opLetter,
    reservedNames: [
      "let",
      "in",
      "case",
      "of",
      "if",
      "then",
      "else",
      "data",
      "type",
      "class",
      "default",
      "deriving",
      "do",
      "import",
      "infix",
      "infixl",
      "infixr",
      "instance",
      "module",
      "newtype",
      "where",
      "primitive"
    ],
    reservedOpNames: ["::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>"],
    caseSensitive: haskellStyle.caseSensitive
  }
))();
const haskellDef = /* #__PURE__ */ (() => (
  {
    commentStart: haskellStyle.commentStart,
    commentEnd: haskellStyle.commentEnd,
    commentLine: haskellStyle.commentLine,
    nestedComments: haskellStyle.nestedComments,
    identStart: haskellStyle.identStart,
    identLetter: (() => {
      const $0 = Parsing$dCombinators.withErrorMessage(Parsing$dString.satisfy(v => v === "#"))("'#'");
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => haskellStyle.identLetter(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    opStart: haskellStyle.opStart,
    opLetter: haskellStyle.opLetter,
    reservedNames: [...haskell98Def.reservedNames, "foreign", "import", "export", "primitive", "_ccall_", "_casm_", "forall"],
    reservedOpNames: haskell98Def.reservedOpNames,
    caseSensitive: haskellStyle.caseSensitive
  }
))();
const haskell = /* #__PURE__ */ Parsing$dToken.makeTokenParser(haskellDef);
const javaStyle = /* #__PURE__ */ (() => (
  {
    commentStart: "/*",
    commentEnd: "*/",
    commentLine: "//",
    nestedComments: true,
    identStart: (() => {
      const $0 = Parsing$dString$dBasic.oneOf(["_", "$"]);
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => Parsing$dString$dBasic.letter(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    identLetter: (() => {
      const $0 = Parsing$dString$dBasic.oneOf(["_", "$"]);
      return (v2, $1, $2, $3, $4) => {
        const $5 = v2._1;
        const $6 = v2._2;
        return $1(v3 => Parsing$dString$dBasic.alphaNum(
          Parsing.$ParseState($5, $6, false),
          $1,
          $2,
          (v4, $7) => {
            const $8 = v4._3;
            return $1(v5 => {
              if ($8) { return $3(v4, $7); }
              return $0(v2, $1, $2, $3, $4);
            });
          },
          $4
        ));
      };
    })(),
    opStart: emptyDef.opStart,
    opLetter: emptyDef.opLetter,
    reservedNames: [],
    reservedOpNames: [],
    caseSensitive: false
  }
))();
export {emptyDef, haskell, haskell98Def, haskellDef, haskellStyle, javaStyle};

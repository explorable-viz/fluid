import * as $runtime from "../runtime.js";
import * as Bind from "../Bind/index.js";
import * as Control$dBind from "../Control.Bind/index.js";
import * as Control$dMonad$dExcept$dTrans from "../Control.Monad.Except.Trans/index.js";
import * as Data$dBitraversable from "../Data.Bitraversable/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dEq from "../Data.Eq/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dGeneric$dRep from "../Data.Generic.Rep/index.js";
import * as Data$dIdentity from "../Data.Identity/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dNonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dNonEmpty from "../Data.NonEmpty/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dShow$dGeneric from "../Data.Show.Generic/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as DataType from "../DataType/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Expr from "../Expr/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Util$dPair from "../Util.Pair/index.js";
const $DictEntry = (tag, _1, _2) => ({tag, _1, _2});
const $Expr = (tag, _1, _2, _3, _4) => ({tag, _1, _2, _3, _4});
const $ListRest = (tag, _1, _2, _3) => ({tag, _1, _2, _3});
const $ListRestPattern = (tag, _1, _2) => ({tag, _1, _2});
const $Module = _1 => ({tag: "Module", _1});
const $ParagraphElem = (tag, _1) => ({tag, _1});
const $Pattern = (tag, _1, _2) => ({tag, _1, _2});
const $Qualifier = (tag, _1, _2) => ({tag, _1, _2});
const $VarDef = (_1, _2) => ({tag: "VarDef", _1, _2});
const genericShowArgsArgument = {genericShowArgs: v => [Data$dShow.showStringImpl(v)]};
const genericShowSum = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument)({reflectSymbol: () => "PVar"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const PConstrIsSymbol = {reflectSymbol: () => "PConstr"};
const showTuple = dictShow1 => ({show: v => "(Tuple " + Data$dShow.showStringImpl(v._1) + " " + dictShow1.show(v._2) + ")"});
const PRecordIsSymbol = {reflectSymbol: () => "PRecord"};
const genericShowSum1 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "PListEmpty"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const PListNonEmptyIsSymbol = {reflectSymbol: () => "PListNonEmpty"};
const genericShowSum2 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument)({reflectSymbol: () => "PListVar"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const genericShowSum3 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsNoArguments)({reflectSymbol: () => "PListEnd"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const PListNextIsSymbol = {reflectSymbol: () => "PListNext"};
const VarDefIsSymbol = {reflectSymbol: () => "VarDef"};
const ListCompGuardIsSymbol = {reflectSymbol: () => "ListCompGuard"};
const ListCompGenIsSymbol = {reflectSymbol: () => "ListCompGen"};
const ListCompDeclIsSymbol = {reflectSymbol: () => "ListCompDecl"};
const genericShowSum4 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument)({reflectSymbol: () => "Token"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const UnquoteIsSymbol = {reflectSymbol: () => "Unquote"};
const EndIsSymbol = {reflectSymbol: () => "End"};
const NextIsSymbol = {reflectSymbol: () => "Next"};
const genericShowSum5 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument)({reflectSymbol: () => "Var"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const genericShowSum6 = /* #__PURE__ */ (() => {
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument)({reflectSymbol: () => "Op"});
  return dictGenericShow1 => (
    {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") { return dictGenericShow1["genericShow'"](v._1); }
        $runtime.fail();
      }
    }
  );
})();
const genericShowArgsArgument1 = {genericShowArgs: v => [Data$dShow.showIntImpl(v)]};
const IntIsSymbol = {reflectSymbol: () => "Int"};
const genericShowArgsArgument2 = {genericShowArgs: v => [Data$dShow.showNumberImpl(v)]};
const FloatIsSymbol = {reflectSymbol: () => "Float"};
const StrIsSymbol = {reflectSymbol: () => "Str"};
const ConstrIsSymbol = {reflectSymbol: () => "Constr"};
const DictionaryIsSymbol = {reflectSymbol: () => "Dictionary"};
const genericShowArgsProduct1 = /* #__PURE__ */ Data$dShow$dGeneric.genericShowArgsProduct(/* #__PURE__ */ (() => {
  const $0 = showTuple(Data$dShow.showString);
  return {genericShowArgs: v => [$0.show(v)]};
})());
const MatrixIsSymbol = {reflectSymbol: () => "Matrix"};
const LambdaIsSymbol = {reflectSymbol: () => "Lambda"};
const ProjectIsSymbol = {reflectSymbol: () => "Project"};
const DProjectIsSymbol = {reflectSymbol: () => "DProject"};
const AppIsSymbol = {reflectSymbol: () => "App"};
const BinaryAppIsSymbol = {reflectSymbol: () => "BinaryApp"};
const MatchAsIsSymbol = {reflectSymbol: () => "MatchAs"};
const IfElseIsSymbol = {reflectSymbol: () => "IfElse"};
const ParagraphIsSymbol = {reflectSymbol: () => "Paragraph"};
const ListEmptyIsSymbol = {reflectSymbol: () => "ListEmpty"};
const ListNonEmptyIsSymbol = {reflectSymbol: () => "ListNonEmpty"};
const ListEnumIsSymbol = {reflectSymbol: () => "ListEnum"};
const ListCompIsSymbol = {reflectSymbol: () => "ListComp"};
const LetIsSymbol = {reflectSymbol: () => "Let"};
const LetRecIsSymbol = {reflectSymbol: () => "LetRec"};
const DocExprIsSymbol = {reflectSymbol: () => "DocExpr"};
const ExprKeyIsSymbol = {reflectSymbol: () => "ExprKey"};
const VarKeyIsSymbol = {reflectSymbol: () => "VarKey"};
const ClausesIsSymbol = {reflectSymbol: () => "Clauses"};
const ClauseIsSymbol = {reflectSymbol: () => "Clause"};
const difference = /* #__PURE__ */ Data$dList.difference(Data$dEq.eqString);
const toUnfoldable = x => {
  const go = (m$p, z$p) => {
    if (m$p.tag === "Leaf") { return z$p; }
    if (m$p.tag === "Node") { return go(m$p._5, Data$dList$dTypes.$List("Cons", m$p._3, go(m$p._6, z$p))); }
    $runtime.fail();
  };
  const go$1 = go$1$a0$copy => go$1$a1$copy => {
    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
    while (go$1$c) {
      const source = go$1$a0, memo = go$1$a1;
      if (source.tag === "Nil") {
        const go$2 = go$2$a0$copy => go$2$a1$copy => {
          let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
          while (go$2$c) {
            const b = go$2$a0, v = go$2$a1;
            if (v.tag === "Nil") {
              go$2$c = false;
              go$2$r = b;
              continue;
            }
            if (v.tag === "Cons") {
              go$2$a0 = Data$dList$dTypes.$List("Cons", v._1, b);
              go$2$a1 = v._2;
              continue;
            }
            $runtime.fail();
          }
          return go$2$r;
        };
        go$1$c = false;
        go$1$r = go$2(Data$dList$dTypes.Nil)(memo);
        continue;
      }
      if (source.tag === "Cons") {
        go$1$a0 = source._2;
        go$1$a1 = Data$dList$dTypes.$List("Cons", source._1, memo);
        continue;
      }
      $runtime.fail();
    }
    return go$1$r;
  };
  return go$1(go(x, Data$dList$dTypes.Nil))(Data$dList$dTypes.Nil);
};
const monadThrowExceptT = /* #__PURE__ */ Control$dMonad$dExcept$dTrans.monadThrowExceptT(Data$dIdentity.monadIdentity);
const fromFoldable = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dFoldable.foldableArray);
const fromFoldable1 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableNonEmptyList);
const fromFoldable2 = /* #__PURE__ */ Foreign$dObject.fromFoldable(Data$dList$dTypes.foldableList);
const monadErrorExceptT = /* #__PURE__ */ Control$dMonad$dExcept$dTrans.monadErrorExceptT(Data$dIdentity.monadIdentity);
const PListVar = value0 => $ListRestPattern("PListVar", value0);
const PListEnd = /* #__PURE__ */ $ListRestPattern("PListEnd");
const PListNext = value0 => value1 => $ListRestPattern("PListNext", value0, value1);
const PVar = value0 => $Pattern("PVar", value0);
const PConstr = value0 => value1 => $Pattern("PConstr", value0, value1);
const PRecord = value0 => $Pattern("PRecord", value0);
const PListEmpty = /* #__PURE__ */ $Pattern("PListEmpty");
const PListNonEmpty = value0 => value1 => $Pattern("PListNonEmpty", value0, value1);
const Clause = x => x;
const Var = value0 => $Expr("Var", value0);
const Op = value0 => $Expr("Op", value0);
const Int = value0 => value1 => $Expr("Int", value0, value1);
const Float = value0 => value1 => $Expr("Float", value0, value1);
const Str = value0 => value1 => $Expr("Str", value0, value1);
const Constr = value0 => value1 => value2 => $Expr("Constr", value0, value1, value2);
const Dictionary = value0 => value1 => $Expr("Dictionary", value0, value1);
const Matrix = value0 => value1 => value2 => value3 => $Expr("Matrix", value0, value1, value2, value3);
const Lambda = value0 => $Expr("Lambda", value0);
const Project = value0 => value1 => $Expr("Project", value0, value1);
const DProject = value0 => value1 => $Expr("DProject", value0, value1);
const App = value0 => value1 => $Expr("App", value0, value1);
const BinaryApp = value0 => value1 => value2 => $Expr("BinaryApp", value0, value1, value2);
const MatchAs = value0 => value1 => $Expr("MatchAs", value0, value1);
const IfElse = value0 => value1 => $Expr("IfElse", value0, value1);
const Paragraph = value0 => $Expr("Paragraph", value0);
const ListEmpty = value0 => $Expr("ListEmpty", value0);
const ListNonEmpty = value0 => value1 => value2 => $Expr("ListNonEmpty", value0, value1, value2);
const ListEnum = value0 => value1 => $Expr("ListEnum", value0, value1);
const ListComp = value0 => value1 => value2 => $Expr("ListComp", value0, value1, value2);
const Let = value0 => value1 => $Expr("Let", value0, value1);
const LetRec = value0 => value1 => $Expr("LetRec", value0, value1);
const DocExpr = value0 => value1 => $Expr("DocExpr", value0, value1);
const ExprKey = value0 => $DictEntry("ExprKey", value0);
const VarKey = value0 => value1 => $DictEntry("VarKey", value0, value1);
const Clauses = x => x;
const Token = value0 => $ParagraphElem("Token", value0);
const Unquote = value0 => $ParagraphElem("Unquote", value0);
const End = value0 => $ListRest("End", value0);
const Next = value0 => value1 => value2 => $ListRest("Next", value0, value1, value2);
const ListCompGuard = value0 => $Qualifier("ListCompGuard", value0);
const ListCompGen = value0 => value1 => $Qualifier("ListCompGen", value0, value1);
const ListCompDecl = value0 => $Qualifier("ListCompDecl", value0);
const VarDef = value0 => value1 => $VarDef(value0, value1);
const RecDef = x => x;
const Module = value0 => $Module(value0);
const newtypeRecDef_ = {Coercible0: () => {}};
const newtypeClauses_ = {Coercible0: () => {}};
const newtypeClause_ = {Coercible0: () => {}};
const joinSemilatticeExpr = dictJoinSemilattice => ({join: v => Effect$dException.throwException(Effect$dException.error("unimplemented"))()});
const genericVarDef_ = {to: x => $VarDef(x._1, x._2), from: x => Data$dGeneric$dRep.$Product(x._1, x._2)};
const genericQualifier_ = {
  to: x => {
    if (x.tag === "Inl") { return $Qualifier("ListCompGuard", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $Qualifier("ListCompGen", x._1._1._1, x._1._1._2); }
      if (x._1.tag === "Inr") { return $Qualifier("ListCompDecl", x._1._1); }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "ListCompGuard") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "ListCompGen") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))); }
    if (x.tag === "ListCompDecl") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", x._1)); }
    $runtime.fail();
  }
};
const genericPattern_ = {
  to: x => {
    if (x.tag === "Inl") { return $Pattern("PVar", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $Pattern("PConstr", x._1._1._1, x._1._1._2); }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return $Pattern("PRecord", x._1._1._1); }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") { return PListEmpty; }
          if (x._1._1._1.tag === "Inr") { return $Pattern("PListNonEmpty", x._1._1._1._1._1, x._1._1._1._1._2); }
        }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "PVar") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "PConstr") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))); }
    if (x.tag === "PRecord") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1))); }
    if (x.tag === "PListEmpty") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))));
    }
    if (x.tag === "PListNonEmpty") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, x._2)))));
    }
    $runtime.fail();
  }
};
const genericParagraphElem_ = {
  to: x => {
    if (x.tag === "Inl") { return $ParagraphElem("Token", x._1); }
    if (x.tag === "Inr") { return $ParagraphElem("Unquote", x._1); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Token") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "Unquote") { return Data$dGeneric$dRep.$Sum("Inr", x._1); }
    $runtime.fail();
  }
};
const genericListRest_ = {
  to: x => {
    if (x.tag === "Inl") { return $ListRest("End", x._1); }
    if (x.tag === "Inr") { return $ListRest("Next", x._1._1, x._1._2._1, x._1._2._2); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "End") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "Next") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))); }
    $runtime.fail();
  }
};
const genericListRestPattern_ = {
  to: x => {
    if (x.tag === "Inl") { return $ListRestPattern("PListVar", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return PListEnd; }
      if (x._1.tag === "Inr") { return $ListRestPattern("PListNext", x._1._1._1, x._1._1._2); }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "PListVar") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "PListEnd") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (x.tag === "PListNext") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, x._2))); }
    $runtime.fail();
  }
};
const showPattern1 = {
  show: c => genericShowSum((() => {
    const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument)((() => {
      const $0 = Data$dList$dTypes.showList(showPattern1);
      return {genericShowArgs: v => [$0.show(v)]};
    })()))(PConstrIsSymbol);
    const $1 = Data$dShow$dGeneric.genericShowConstructor((() => {
      const $1 = Data$dList$dTypes.showList(showTuple(showPattern1));
      return {genericShowArgs: v => [$1.show(v)]};
    })())(PRecordIsSymbol);
    const $2 = genericShowSum1(Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [showPattern1.show(v)]})({
      genericShowArgs: v => [showListRestPattern.show(v)]
    }))(PListNonEmptyIsSymbol));
    return {
      "genericShow'": v => {
        if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
        if (v.tag === "Inr") {
          if (v._1.tag === "Inl") { return $1["genericShow'"](v._1._1); }
          if (v._1.tag === "Inr") { return $2["genericShow'"](v._1._1); }
        }
        $runtime.fail();
      }
    };
  })())["genericShow'"]((() => {
    if (c.tag === "PVar") { return Data$dGeneric$dRep.$Sum("Inl", c._1); }
    if (c.tag === "PConstr") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(c._1, c._2))); }
    if (c.tag === "PRecord") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", c._1))); }
    if (c.tag === "PListEmpty") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments))));
    }
    if (c.tag === "PListNonEmpty") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(c._1, c._2)))));
    }
    $runtime.fail();
  })())
};
const showListRestPattern = {
  show: c => genericShowSum2(genericShowSum3(Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [showPattern1.show(v)]})({
    genericShowArgs: v => [showListRestPattern.show(v)]
  }))(PListNextIsSymbol)))["genericShow'"]((() => {
    if (c.tag === "PListVar") { return Data$dGeneric$dRep.$Sum("Inl", c._1); }
    if (c.tag === "PListEnd") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.NoArguments)); }
    if (c.tag === "PListNext") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(c._1, c._2))); }
    $runtime.fail();
  })())
};
const showTuple1 = dictShow1 => ({show: v => "(Tuple " + showPattern1.show(v._1) + " " + dictShow1.show(v._2) + ")"});
const showTuple2 = /* #__PURE__ */ (() => {
  const $0 = Data$dList$dTypes.showNonEmptyList(showPattern1);
  return dictShow1 => ({show: v => "(Tuple " + $0.show(v._1) + " " + dictShow1.show(v._2) + ")"});
})();
const genericExpr_ = {
  to: x => {
    if (x.tag === "Inl") { return $Expr("Var", x._1); }
    if (x.tag === "Inr") {
      if (x._1.tag === "Inl") { return $Expr("Op", x._1._1); }
      if (x._1.tag === "Inr") {
        if (x._1._1.tag === "Inl") { return $Expr("Int", x._1._1._1._1, x._1._1._1._2); }
        if (x._1._1.tag === "Inr") {
          if (x._1._1._1.tag === "Inl") { return $Expr("Float", x._1._1._1._1._1, x._1._1._1._1._2); }
          if (x._1._1._1.tag === "Inr") {
            if (x._1._1._1._1.tag === "Inl") { return $Expr("Str", x._1._1._1._1._1._1, x._1._1._1._1._1._2); }
            if (x._1._1._1._1.tag === "Inr") {
              if (x._1._1._1._1._1.tag === "Inl") { return $Expr("Constr", x._1._1._1._1._1._1._1, x._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._2._2); }
              if (x._1._1._1._1._1.tag === "Inr") {
                if (x._1._1._1._1._1._1.tag === "Inl") { return $Expr("Dictionary", x._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._2); }
                if (x._1._1._1._1._1._1.tag === "Inr") {
                  if (x._1._1._1._1._1._1._1.tag === "Inl") {
                    return $Expr("Matrix", x._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._2._1, x._1._1._1._1._1._1._1._1._2._2._1, x._1._1._1._1._1._1._1._1._2._2._2);
                  }
                  if (x._1._1._1._1._1._1._1.tag === "Inr") {
                    if (x._1._1._1._1._1._1._1._1.tag === "Inl") { return $Expr("Lambda", x._1._1._1._1._1._1._1._1._1); }
                    if (x._1._1._1._1._1._1._1._1.tag === "Inr") {
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $Expr("Project", x._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._2); }
                      if (x._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                          return $Expr("DProject", x._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._2);
                        }
                        if (x._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                            return $Expr("App", x._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._2);
                          }
                          if (x._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                              return $Expr(
                                "BinaryApp",
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                              );
                            }
                            if (x._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                return $Expr("MatchAs", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2);
                              }
                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                  return $Expr("IfElse", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1, x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2);
                                }
                                if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                    return $Expr("Paragraph", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                  }
                                  if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                    if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                      return $Expr("ListEmpty", x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                    }
                                    if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                      if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                        return $Expr(
                                          "ListNonEmpty",
                                          x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                          x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                          x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                                        );
                                      }
                                      if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                        if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                          return $Expr(
                                            "ListEnum",
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                            x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                          );
                                        }
                                        if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                          if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                            return $Expr(
                                              "ListComp",
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._1,
                                              x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2._2
                                            );
                                          }
                                          if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                            if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                              return $Expr(
                                                "Let",
                                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                                x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                              );
                                            }
                                            if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                                return $Expr(
                                                  "LetRec",
                                                  x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                                  x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                                );
                                              }
                                              if (x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                                return $Expr(
                                                  "DocExpr",
                                                  x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1,
                                                  x._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._2
                                                );
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "Var") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "Op") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)); }
    if (x.tag === "Int") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))); }
    if (x.tag === "Float") {
      return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))));
    }
    if (x.tag === "Str") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))))
      );
    }
    if (x.tag === "Constr") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))))
            )
          )
        )
      );
    }
    if (x.tag === "Dictionary") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2)))))
          )
        )
      );
    }
    if (x.tag === "Matrix") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, Data$dGeneric$dRep.$Product(x._3, x._4))))
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "Lambda") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1)))))
            )
          )
        )
      );
    }
    if (x.tag === "Project") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "DProject") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "App") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "BinaryApp") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "MatchAs") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "IfElse") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "Paragraph") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1))))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListEmpty") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", x._1))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListNonEmpty") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListEnum") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "ListComp") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum(
                                        "Inr",
                                        Data$dGeneric$dRep.$Sum(
                                          "Inr",
                                          Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, Data$dGeneric$dRep.$Product(x._2, x._3))))
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "Let") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum(
                                        "Inr",
                                        Data$dGeneric$dRep.$Sum(
                                          "Inr",
                                          Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "LetRec") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum(
                                        "Inr",
                                        Data$dGeneric$dRep.$Sum(
                                          "Inr",
                                          Data$dGeneric$dRep.$Sum(
                                            "Inr",
                                            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inl", Data$dGeneric$dRep.$Product(x._1, x._2))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    if (x.tag === "DocExpr") {
      return Data$dGeneric$dRep.$Sum(
        "Inr",
        Data$dGeneric$dRep.$Sum(
          "Inr",
          Data$dGeneric$dRep.$Sum(
            "Inr",
            Data$dGeneric$dRep.$Sum(
              "Inr",
              Data$dGeneric$dRep.$Sum(
                "Inr",
                Data$dGeneric$dRep.$Sum(
                  "Inr",
                  Data$dGeneric$dRep.$Sum(
                    "Inr",
                    Data$dGeneric$dRep.$Sum(
                      "Inr",
                      Data$dGeneric$dRep.$Sum(
                        "Inr",
                        Data$dGeneric$dRep.$Sum(
                          "Inr",
                          Data$dGeneric$dRep.$Sum(
                            "Inr",
                            Data$dGeneric$dRep.$Sum(
                              "Inr",
                              Data$dGeneric$dRep.$Sum(
                                "Inr",
                                Data$dGeneric$dRep.$Sum(
                                  "Inr",
                                  Data$dGeneric$dRep.$Sum(
                                    "Inr",
                                    Data$dGeneric$dRep.$Sum(
                                      "Inr",
                                      Data$dGeneric$dRep.$Sum(
                                        "Inr",
                                        Data$dGeneric$dRep.$Sum(
                                          "Inr",
                                          Data$dGeneric$dRep.$Sum(
                                            "Inr",
                                            Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, x._2))))
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      );
    }
    $runtime.fail();
  }
};
const genericDictEntry_ = {
  to: x => {
    if (x.tag === "Inl") { return $DictEntry("ExprKey", x._1); }
    if (x.tag === "Inr") { return $DictEntry("VarKey", x._1._1, x._1._2); }
    $runtime.fail();
  },
  from: x => {
    if (x.tag === "ExprKey") { return Data$dGeneric$dRep.$Sum("Inl", x._1); }
    if (x.tag === "VarKey") { return Data$dGeneric$dRep.$Sum("Inr", Data$dGeneric$dRep.$Product(x._1, x._2)); }
    $runtime.fail();
  }
};
const genericClauses_ = {to: x => x, from: x => x};
const genericClause_ = {to: x => x, from: x => x};
const showVarDef = dictShow => (
  {
    show: c => Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [showPattern1.show(v)]})((() => {
      const $0 = showExpr(dictShow);
      return {genericShowArgs: v => [$0.show(v)]};
    })()))(VarDefIsSymbol)["genericShow'"](Data$dGeneric$dRep.$Product(c._1, c._2))
  }
);
const showQualifier = dictShow => (
  {
    show: c => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $0 = showExpr(dictShow);
        return {genericShowArgs: v => [$0.show(v)]};
      })())(ListCompGuardIsSymbol);
      const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [showPattern1.show(v)]})((() => {
        const $1 = showExpr(dictShow);
        return {genericShowArgs: v => [$1.show(v)]};
      })()))(ListCompGenIsSymbol);
      const $2 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $2 = showVarDef(dictShow);
        return {genericShowArgs: v => [$2.show(v)]};
      })())(ListCompDeclIsSymbol);
      if (c.tag === "ListCompGuard") { return $0["genericShow'"](c._1); }
      if (c.tag === "ListCompGen") { return $1["genericShow'"](Data$dGeneric$dRep.$Product(c._1, c._2)); }
      if (c.tag === "ListCompDecl") { return $2["genericShow'"](c._1); }
      $runtime.fail();
    }
  }
);
const showParagraphElem = dictShow => (
  {
    show: c => genericShowSum4(Data$dShow$dGeneric.genericShowConstructor((() => {
      const $0 = showExpr(dictShow);
      return {genericShowArgs: v => [$0.show(v)]};
    })())(UnquoteIsSymbol))["genericShow'"]((() => {
      if (c.tag === "Token") { return Data$dGeneric$dRep.$Sum("Inl", c._1); }
      if (c.tag === "Unquote") { return Data$dGeneric$dRep.$Sum("Inr", c._1); }
      $runtime.fail();
    })())
  }
);
const showListRest = dictShow => {
  const genericShowArgsArgument3 = {genericShowArgs: v => [dictShow.show(v)]};
  const $0 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument3)(EndIsSymbol);
  return {
    show: c => {
      const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $1 = showExpr(dictShow);
        return {genericShowArgs: v => [$1.show(v)]};
      })())((() => {
        const $1 = showListRest(dictShow);
        return {genericShowArgs: v => [$1.show(v)]};
      })())))(NextIsSymbol);
      if (c.tag === "End") { return $0["genericShow'"](c._1); }
      if (c.tag === "Next") { return $1["genericShow'"](Data$dGeneric$dRep.$Product(c._1, Data$dGeneric$dRep.$Product(c._2, c._3))); }
      $runtime.fail();
    }
  };
};
const showExpr = dictShow => {
  const genericShowArgsArgument3 = {genericShowArgs: v => [dictShow.show(v)]};
  const $0 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument1))(IntIsSymbol);
  const $1 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument2))(FloatIsSymbol);
  const $2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(genericShowArgsArgument))(StrIsSymbol);
  const $3 = Data$dShow$dGeneric.genericShowConstructor(genericShowArgsArgument3)(ListEmptyIsSymbol);
  return {
    show: c => genericShowSum5(genericShowSum6((() => {
      const $4 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument)((() => {
        const $4 = Data$dList$dTypes.showList(showExpr(dictShow));
        return {genericShowArgs: v => [$4.show(v)]};
      })())))(ConstrIsSymbol);
      const $5 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)((() => {
        const $5 = Data$dList$dTypes.showList((() => {
          const $5 = showDictEntry(dictShow);
          const $6 = showExpr(dictShow);
          return {show: v => "(Tuple " + $5.show(v._1) + " " + $6.show(v._2) + ")"};
        })());
        return {genericShowArgs: v => [$5.show(v)]};
      })()))(DictionaryIsSymbol);
      const $6 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $6 = showExpr(dictShow);
        return {genericShowArgs: v => [$6.show(v)]};
      })())(genericShowArgsProduct1((() => {
        const $6 = showExpr(dictShow);
        return {genericShowArgs: v => [$6.show(v)]};
      })()))))(MatrixIsSymbol);
      const $7 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $7 = showClauses(dictShow);
        return {genericShowArgs: v => [$7.show(v)]};
      })())(LambdaIsSymbol);
      const $8 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $8 = showExpr(dictShow);
        return {genericShowArgs: v => [$8.show(v)]};
      })())(genericShowArgsArgument))(ProjectIsSymbol);
      const $9 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $9 = showExpr(dictShow);
        return {genericShowArgs: v => [$9.show(v)]};
      })())((() => {
        const $9 = showExpr(dictShow);
        return {genericShowArgs: v => [$9.show(v)]};
      })()))(DProjectIsSymbol);
      const $10 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $10 = showExpr(dictShow);
        return {genericShowArgs: v => [$10.show(v)]};
      })())((() => {
        const $10 = showExpr(dictShow);
        return {genericShowArgs: v => [$10.show(v)]};
      })()))(AppIsSymbol);
      const $11 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $11 = showExpr(dictShow);
        return {genericShowArgs: v => [$11.show(v)]};
      })())(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument)((() => {
        const $11 = showExpr(dictShow);
        return {genericShowArgs: v => [$11.show(v)]};
      })())))(BinaryAppIsSymbol);
      const $12 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $12 = showExpr(dictShow);
        return {genericShowArgs: v => [$12.show(v)]};
      })())((() => {
        const $12 = Data$dList$dTypes.showNonEmptyList(showTuple1(showExpr(dictShow)));
        return {genericShowArgs: v => [$12.show(v)]};
      })()))(MatchAsIsSymbol);
      const $13 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $13 = Data$dList$dTypes.showNonEmptyList((() => {
          const $13 = showExpr(dictShow);
          const $14 = showExpr(dictShow);
          return {show: v => "(Tuple " + $13.show(v._1) + " " + $14.show(v._2) + ")"};
        })());
        return {genericShowArgs: v => [$13.show(v)]};
      })())((() => {
        const $13 = showExpr(dictShow);
        return {genericShowArgs: v => [$13.show(v)]};
      })()))(IfElseIsSymbol);
      const $14 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $14 = Data$dList$dTypes.showList(showParagraphElem(dictShow));
        return {genericShowArgs: v => [$14.show(v)]};
      })())(ParagraphIsSymbol);
      const $15 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $15 = showExpr(dictShow);
        return {genericShowArgs: v => [$15.show(v)]};
      })())((() => {
        const $15 = showListRest(dictShow);
        return {genericShowArgs: v => [$15.show(v)]};
      })())))(ListNonEmptyIsSymbol);
      const $16 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $16 = showExpr(dictShow);
        return {genericShowArgs: v => [$16.show(v)]};
      })())((() => {
        const $16 = showExpr(dictShow);
        return {genericShowArgs: v => [$16.show(v)]};
      })()))(ListEnumIsSymbol);
      const $17 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct(genericShowArgsArgument3)(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $17 = showExpr(dictShow);
        return {genericShowArgs: v => [$17.show(v)]};
      })())((() => {
        const $17 = Data$dList$dTypes.showList(showQualifier(dictShow));
        return {genericShowArgs: v => [$17.show(v)]};
      })())))(ListCompIsSymbol);
      const $18 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $18 = Data$dList$dTypes.showNonEmptyList(showVarDef(dictShow));
        return {genericShowArgs: v => [$18.show(v)]};
      })())((() => {
        const $18 = showExpr(dictShow);
        return {genericShowArgs: v => [$18.show(v)]};
      })()))(LetIsSymbol);
      const $19 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $19 = Data$dList$dTypes.showNonEmptyList(showTuple(showClause(dictShow)));
        return {genericShowArgs: v => [$19.show(v)]};
      })())((() => {
        const $19 = showExpr(dictShow);
        return {genericShowArgs: v => [$19.show(v)]};
      })()))(LetRecIsSymbol);
      const $20 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct((() => {
        const $20 = showExpr(dictShow);
        return {genericShowArgs: v => [$20.show(v)]};
      })())((() => {
        const $20 = showExpr(dictShow);
        return {genericShowArgs: v => [$20.show(v)]};
      })()))(DocExprIsSymbol);
      return {
        "genericShow'": v => {
          if (v.tag === "Inl") { return $0["genericShow'"](v._1); }
          if (v.tag === "Inr") {
            if (v._1.tag === "Inl") { return $1["genericShow'"](v._1._1); }
            if (v._1.tag === "Inr") {
              if (v._1._1.tag === "Inl") { return $2["genericShow'"](v._1._1._1); }
              if (v._1._1.tag === "Inr") {
                if (v._1._1._1.tag === "Inl") { return $4["genericShow'"](v._1._1._1._1); }
                if (v._1._1._1.tag === "Inr") {
                  if (v._1._1._1._1.tag === "Inl") { return $5["genericShow'"](v._1._1._1._1._1); }
                  if (v._1._1._1._1.tag === "Inr") {
                    if (v._1._1._1._1._1.tag === "Inl") { return $6["genericShow'"](v._1._1._1._1._1._1); }
                    if (v._1._1._1._1._1.tag === "Inr") {
                      if (v._1._1._1._1._1._1.tag === "Inl") { return $7["genericShow'"](v._1._1._1._1._1._1._1); }
                      if (v._1._1._1._1._1._1.tag === "Inr") {
                        if (v._1._1._1._1._1._1._1.tag === "Inl") { return $8["genericShow'"](v._1._1._1._1._1._1._1._1); }
                        if (v._1._1._1._1._1._1._1.tag === "Inr") {
                          if (v._1._1._1._1._1._1._1._1.tag === "Inl") { return $9["genericShow'"](v._1._1._1._1._1._1._1._1._1); }
                          if (v._1._1._1._1._1._1._1._1.tag === "Inr") {
                            if (v._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $10["genericShow'"](v._1._1._1._1._1._1._1._1._1._1); }
                            if (v._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                              if (v._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $11["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1); }
                              if (v._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                if (v._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $12["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1); }
                                if (v._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                  if (v._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $13["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1); }
                                  if (v._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                    if (v._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $14["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1); }
                                    if (v._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                      if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") { return $3["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1); }
                                      if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                        if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                          return $15["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                        }
                                        if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                          if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                            return $16["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                          }
                                          if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                            if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                              return $17["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                            }
                                            if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                              if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                                return $18["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                              }
                                              if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                                if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inl") {
                                                  return $19["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                                }
                                                if (v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1.tag === "Inr") {
                                                  return $20["genericShow'"](v._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1._1);
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          $runtime.fail();
        }
      };
    })()))["genericShow'"](genericExpr_.from(c))
  };
};
const showDictEntry = dictShow => {
  const genericShowConstructor2 = Data$dShow$dGeneric.genericShowConstructor(Data$dShow$dGeneric.genericShowArgsProduct({genericShowArgs: v => [dictShow.show(v)]})(genericShowArgsArgument))(VarKeyIsSymbol);
  return {
    show: c => {
      const $0 = Data$dShow$dGeneric.genericShowConstructor((() => {
        const $0 = showExpr(dictShow);
        return {genericShowArgs: v => [$0.show(v)]};
      })())(ExprKeyIsSymbol);
      if (c.tag === "ExprKey") { return $0["genericShow'"](c._1); }
      if (c.tag === "VarKey") { return genericShowConstructor2["genericShow'"](Data$dGeneric$dRep.$Product(c._1, c._2)); }
      $runtime.fail();
    }
  };
};
const showClauses = dictShow => (
  {
    show: c => Data$dShow$dGeneric.genericShowConstructor((() => {
      const $0 = Data$dList$dTypes.showNonEmptyList(showClause(dictShow));
      return {genericShowArgs: v => [$0.show(v)]};
    })())(ClausesIsSymbol)["genericShow'"](c)
  }
);
const showClause = dictShow => (
  {
    show: c => Data$dShow$dGeneric.genericShowConstructor((() => {
      const $0 = showTuple2(showExpr(dictShow));
      return {genericShowArgs: v => [$0.show(v)]};
    })())(ClauseIsSymbol)["genericShow'"](c)
  }
);
const functorVarDef = {map: f => m => $VarDef(m._1, functorExpr.map(f)(m._2))};
const functorQualifier = {
  map: f => m => {
    if (m.tag === "ListCompGuard") { return $Qualifier("ListCompGuard", functorExpr.map(f)(m._1)); }
    if (m.tag === "ListCompGen") { return $Qualifier("ListCompGen", m._1, functorExpr.map(f)(m._2)); }
    if (m.tag === "ListCompDecl") { return $Qualifier("ListCompDecl", $VarDef(m._1._1, functorExpr.map(f)(m._1._2))); }
    $runtime.fail();
  }
};
const functorParagraphElem = {
  map: f => m => {
    if (m.tag === "Token") { return $ParagraphElem("Token", m._1); }
    if (m.tag === "Unquote") { return $ParagraphElem("Unquote", functorExpr.map(f)(m._1)); }
    $runtime.fail();
  }
};
const functorListRest = {
  map: f => m => {
    if (m.tag === "End") { return $ListRest("End", f(m._1)); }
    if (m.tag === "Next") { return $ListRest("Next", f(m._1), functorExpr.map(f)(m._2), functorListRest.map(f)(m._3)); }
    $runtime.fail();
  }
};
const functorExpr = {
  map: f => m => {
    if (m.tag === "Var") { return $Expr("Var", m._1); }
    if (m.tag === "Op") { return $Expr("Op", m._1); }
    if (m.tag === "Int") { return $Expr("Int", f(m._1), m._2); }
    if (m.tag === "Float") { return $Expr("Float", f(m._1), m._2); }
    if (m.tag === "Str") { return $Expr("Str", f(m._1), m._2); }
    if (m.tag === "Constr") { return $Expr("Constr", f(m._1), m._2, Data$dList$dTypes.listMap(functorExpr.map(f))(m._3)); }
    if (m.tag === "Dictionary") {
      return $Expr(
        "Dictionary",
        f(m._1),
        Data$dList$dTypes.listMap((() => {
          const $0 = functorDictEntry.map(f);
          const $1 = functorExpr.map(f);
          return v => Data$dTuple.$Tuple($0(v._1), $1(v._2));
        })())(m._2)
      );
    }
    if (m.tag === "Matrix") { return $Expr("Matrix", f(m._1), functorExpr.map(f)(m._2), m._3, functorExpr.map(f)(m._4)); }
    if (m.tag === "Lambda") { return $Expr("Lambda", functorClauses.map(f)(m._1)); }
    if (m.tag === "Project") { return $Expr("Project", functorExpr.map(f)(m._1), m._2); }
    if (m.tag === "DProject") { return $Expr("DProject", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "App") { return $Expr("App", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "BinaryApp") { return $Expr("BinaryApp", functorExpr.map(f)(m._1), m._2, functorExpr.map(f)(m._3)); }
    if (m.tag === "MatchAs") {
      return $Expr(
        "MatchAs",
        functorExpr.map(f)(m._1),
        (() => {
          const $0 = functorExpr.map(f);
          return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple(m._2._1._1, $0(m._2._1._2)), Data$dList$dTypes.listMap(m$1 => Data$dTuple.$Tuple(m$1._1, $0(m$1._2)))(m._2._2));
        })()
      );
    }
    if (m.tag === "IfElse") {
      return $Expr(
        "IfElse",
        (() => {
          const $0 = functorExpr.map(f);
          const $1 = functorExpr.map(f);
          return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple($0(m._1._1._1), $1(m._1._1._2)), Data$dList$dTypes.listMap(v => Data$dTuple.$Tuple($0(v._1), $1(v._2)))(m._1._2));
        })(),
        functorExpr.map(f)(m._2)
      );
    }
    if (m.tag === "Paragraph") { return $Expr("Paragraph", Data$dList$dTypes.listMap(functorParagraphElem.map(f))(m._1)); }
    if (m.tag === "ListEmpty") { return $Expr("ListEmpty", f(m._1)); }
    if (m.tag === "ListNonEmpty") { return $Expr("ListNonEmpty", f(m._1), functorExpr.map(f)(m._2), functorListRest.map(f)(m._3)); }
    if (m.tag === "ListEnum") { return $Expr("ListEnum", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    if (m.tag === "ListComp") { return $Expr("ListComp", f(m._1), functorExpr.map(f)(m._2), Data$dList$dTypes.listMap(functorQualifier.map(f))(m._3)); }
    if (m.tag === "Let") {
      return $Expr(
        "Let",
        Data$dNonEmpty.$NonEmpty($VarDef(m._1._1._1, functorExpr.map(f)(m._1._1._2)), Data$dList$dTypes.listMap(functorVarDef.map(f))(m._1._2)),
        functorExpr.map(f)(m._2)
      );
    }
    if (m.tag === "LetRec") {
      return $Expr(
        "LetRec",
        (() => {
          const $0 = functorClause.map(f);
          return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple(m._1._1._1, $0(m._1._1._2)), Data$dList$dTypes.listMap(m$1 => Data$dTuple.$Tuple(m$1._1, $0(m$1._2)))(m._1._2));
        })(),
        functorExpr.map(f)(m._2)
      );
    }
    if (m.tag === "DocExpr") { return $Expr("DocExpr", functorExpr.map(f)(m._1), functorExpr.map(f)(m._2)); }
    $runtime.fail();
  }
};
const functorDictEntry = {
  map: f => m => {
    if (m.tag === "ExprKey") { return $DictEntry("ExprKey", functorExpr.map(f)(m._1)); }
    if (m.tag === "VarKey") { return $DictEntry("VarKey", f(m._1), m._2); }
    $runtime.fail();
  }
};
const functorClauses = {
  map: f => m => {
    const $0 = functorClause.map(f);
    return Data$dNonEmpty.$NonEmpty($0(m._1), Data$dList$dTypes.listMap($0)(m._2));
  }
};
const functorClause = {map: f => m => Data$dTuple.$Tuple(m._1, functorExpr.map(f)(m._2))};
const functorModule = {
  map: f => v => $Module(Data$dList$dTypes.listMap(v2 => {
    if (v2.tag === "Left") {
      return Data$dEither.$Either(
        "Left",
        Data$dNonEmpty.$NonEmpty($VarDef(v2._1._1._1, functorExpr.map(f)(v2._1._1._2)), Data$dList$dTypes.listMap(functorVarDef.map(f))(v2._1._2))
      );
    }
    if (v2.tag === "Right") {
      return Data$dEither.$Either(
        "Right",
        (() => {
          const $0 = v3 => Data$dTuple.$Tuple(v3._1, Data$dTuple.$Tuple(v3._2._1, functorExpr.map(f)(v3._2._2)));
          return Data$dNonEmpty.$NonEmpty($0(v2._1._1), Data$dList$dTypes.listMap($1 => $0($1))(v2._1._2));
        })()
      );
    }
    $runtime.fail();
  })(v._1))
};
const eqPattern = {
  eq: x => y => {
    if (x.tag === "PVar") { return y.tag === "PVar" && x._1 === y._1; }
    if (x.tag === "PConstr") {
      return y.tag === "PConstr" && x._1 === y._1 && (() => {
        const go = v => v1 => v2 => {
          if (!v2) { return false; }
          if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && eqPattern.eq(v1._1)(v._1));
        };
        return go(x._2)(y._2)(true);
      })();
    }
    if (x.tag === "PRecord") {
      return y.tag === "PRecord" && (() => {
        const go = v => v1 => v2 => {
          if (!v2) { return false; }
          if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && v1._1._1 === v._1._1 && eqPattern.eq(v1._1._2)(v._1._2));
        };
        return go(x._1)(y._1)(true);
      })();
    }
    if (x.tag === "PListEmpty") { return y.tag === "PListEmpty"; }
    return x.tag === "PListNonEmpty" && y.tag === "PListNonEmpty" && eqPattern.eq(x._1)(y._1) && eqListRestPattern.eq(x._2)(y._2);
  }
};
const eqListRestPattern = {
  eq: x => y => {
    if (x.tag === "PListVar") { return y.tag === "PListVar" && x._1 === y._1; }
    if (x.tag === "PListEnd") { return y.tag === "PListEnd"; }
    return x.tag === "PListNext" && y.tag === "PListNext" && eqPattern.eq(x._1)(y._1) && eqListRestPattern.eq(x._2)(y._2);
  }
};
const eqTuple1 = dictEq1 => ({eq: x => y => eqPattern.eq(x._1)(y._1) && dictEq1.eq(x._2)(y._2)});
const eqTuple2 = dictEq1 => (
  {
    eq: x => y => eqPattern.eq(x._1._1)(y._1._1) && (() => {
      const go = v => v1 => v2 => {
        if (!v2) { return false; }
        if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && eqPattern.eq(v1._1)(v._1));
      };
      return go(x._1._2)(y._1._2)(true);
    })() && dictEq1.eq(x._2)(y._2)
  }
);
const eqList = {
  eq: xs => ys => {
    const go = v => v1 => v2 => {
      if (!v2) { return false; }
      if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v1._1.tag === "Left"
        ? v2 && v._1.tag === "Left" && eqPattern.eq(v1._1._1)(v._1._1)
        : v2 && v1._1.tag === "Right" && v._1.tag === "Right" && eqListRestPattern.eq(v1._1._1)(v._1._1));
    };
    return go(xs)(ys)(true);
  }
};
const eq8 = xs => ys => {
  const go = v => v1 => v2 => {
    if (!v2) { return false; }
    if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
    return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && eqPattern.eq(v1._1)(v._1));
  };
  return go(xs)(ys)(true);
};
const eqVarDef = dictEq => ({eq: x => y => eqPattern.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2)});
const eqQualifier = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "ListCompGuard") { return y.tag === "ListCompGuard" && eqExpr(dictEq).eq(x._1)(y._1); }
      if (x.tag === "ListCompGen") { return y.tag === "ListCompGen" && eqPattern.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      return x.tag === "ListCompDecl" && y.tag === "ListCompDecl" && eqVarDef(dictEq).eq(x._1)(y._1);
    }
  }
);
const eqParagraphElem = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Token") { return y.tag === "Token" && x._1 === y._1; }
      return x.tag === "Unquote" && y.tag === "Unquote" && eqExpr(dictEq).eq(x._1)(y._1);
    }
  }
);
const eqListRest = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "End") { return y.tag === "End" && dictEq.eq(x._1)(y._1); }
      return x.tag === "Next" && y.tag === "Next" && dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && eqListRest(dictEq).eq(x._3)(y._3);
    }
  }
);
const eqExpr = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "Var") { return y.tag === "Var" && x._1 === y._1; }
      if (x.tag === "Op") { return y.tag === "Op" && x._1 === y._1; }
      if (x.tag === "Int") { return y.tag === "Int" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Float") { return y.tag === "Float" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Str") { return y.tag === "Str" && dictEq.eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "Constr") {
        return y.tag === "Constr" && (() => {
          const $0 = eqExpr(dictEq);
          return dictEq.eq(x._1)(y._1) && x._2 === y._2 && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._3)(y._3)(true);
          })();
        })();
      }
      if (x.tag === "Dictionary") {
        return y.tag === "Dictionary" && (() => {
          const $0 = eqDictEntry(dictEq);
          return dictEq.eq(x._1)(y._1) && (() => {
            const $1 = eqExpr(dictEq);
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1._1)(v._1._1) && $1.eq(v1._1._2)(v._1._2));
            };
            return go(x._2)(y._2)(true);
          })();
        })();
      }
      if (x.tag === "Matrix") {
        return y.tag === "Matrix" && dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && x._3._1 === y._3._1 && x._3._2 === y._3._2 && eqExpr(dictEq).eq(x._4)(y._4);
      }
      if (x.tag === "Lambda") { return y.tag === "Lambda" && eqClauses(dictEq).eq(x._1)(y._1); }
      if (x.tag === "Project") { return y.tag === "Project" && eqExpr(dictEq).eq(x._1)(y._1) && x._2 === y._2; }
      if (x.tag === "DProject") { return y.tag === "DProject" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "App") { return y.tag === "App" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "BinaryApp") { return y.tag === "BinaryApp" && eqExpr(dictEq).eq(x._1)(y._1) && x._2 === y._2 && eqExpr(dictEq).eq(x._3)(y._3); }
      if (x.tag === "MatchAs") {
        return y.tag === "MatchAs" && (() => {
          const $0 = eqTuple1(eqExpr(dictEq));
          return eqExpr(dictEq).eq(x._1)(y._1) && $0.eq(x._2._1)(y._2._1) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._2._2)(y._2._2)(true);
          })();
        })();
      }
      if (x.tag === "IfElse") {
        return y.tag === "IfElse" && (() => {
          const $0 = eqExpr(dictEq);
          const $1 = eqExpr(dictEq);
          const $2 = (x$1, y$1) => $0.eq(x$1._1)(y$1._1) && $1.eq(x$1._2)(y$1._2);
          return $2(x._1._1, y._1._1) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $2(v1._1, v._1));
            };
            return go(x._1._2)(y._1._2)(true);
          })() && eqExpr(dictEq).eq(x._2)(y._2);
        })();
      }
      if (x.tag === "Paragraph") {
        return y.tag === "Paragraph" && (() => {
          const $0 = eqParagraphElem(dictEq);
          const go = v => v1 => v2 => {
            if (!v2) { return false; }
            if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
            return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
          };
          return go(x._1)(y._1)(true);
        })();
      }
      if (x.tag === "ListEmpty") { return y.tag === "ListEmpty" && dictEq.eq(x._1)(y._1); }
      if (x.tag === "ListNonEmpty") { return y.tag === "ListNonEmpty" && dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && eqListRest(dictEq).eq(x._3)(y._3); }
      if (x.tag === "ListEnum") { return y.tag === "ListEnum" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2); }
      if (x.tag === "ListComp") {
        return y.tag === "ListComp" && (() => {
          const $0 = eqQualifier(dictEq);
          return dictEq.eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._3)(y._3)(true);
          })();
        })();
      }
      if (x.tag === "Let") {
        return y.tag === "Let" && (() => {
          const $0 = eqVarDef(dictEq);
          return $0.eq(x._1._1)(y._1._1) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
            };
            return go(x._1._2)(y._1._2)(true);
          })() && eqExpr(dictEq).eq(x._2)(y._2);
        })();
      }
      if (x.tag === "LetRec") {
        return y.tag === "LetRec" && (() => {
          const $0 = eqClause(dictEq);
          return x._1._1._1 === y._1._1._1 && $0.eq(x._1._1._2)(y._1._1._2) && (() => {
            const go = v => v1 => v2 => {
              if (!v2) { return false; }
              if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
              return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && v1._1._1 === v._1._1 && $0.eq(v1._1._2)(v._1._2));
            };
            return go(x._1._2)(y._1._2)(true);
          })() && eqExpr(dictEq).eq(x._2)(y._2);
        })();
      }
      return x.tag === "DocExpr" && y.tag === "DocExpr" && eqExpr(dictEq).eq(x._1)(y._1) && eqExpr(dictEq).eq(x._2)(y._2);
    }
  }
);
const eqDictEntry = dictEq => (
  {
    eq: x => y => {
      if (x.tag === "ExprKey") { return y.tag === "ExprKey" && eqExpr(dictEq).eq(x._1)(y._1); }
      return x.tag === "VarKey" && y.tag === "VarKey" && dictEq.eq(x._1)(y._1) && x._2 === y._2;
    }
  }
);
const eqClauses = dictEq => (
  {
    eq: x => y => {
      const $0 = eqClause(dictEq);
      return $0.eq(x._1)(y._1) && (() => {
        const go = v => v1 => v2 => {
          if (!v2) { return false; }
          if (v.tag === "Nil") { return v1.tag === "Nil" && v2; }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
        };
        return go(x._2)(y._2)(true);
      })();
    }
  }
);
const eqClause = dictEq => ({eq: x => y => eqTuple2(eqExpr(dictEq)).eq(x)(y)});
const varKeyBwd = v => v1 => {
  if (v.tag === "Str" && v1.tag === "VarKey") { return $DictEntry("VarKey", v._1, v1._2); }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const toClausesStateFwd = v => Data$dList$dTypes.listMap(v1 => Data$dTuple.$Tuple(
  Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v1._1._1), Data$dList$dTypes.Nil),
  Data$dTuple.$Tuple(v1._1._2, v1._2)
))(Data$dList$dTypes.$List("Cons", v._1, v._2));
const toClausesStateBwd = v => {
  if (v.tag === "Nil") { return Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))(); }
  if (v.tag === "Cons") {
    return Data$dNonEmpty.$NonEmpty(
      v._1._1.tag === "Cons" && v._1._1._1.tag === "Left" && v._1._1._2.tag === "Nil"
        ? Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v._1._1._1._1, v._1._2._1), v._1._2._2)
        : Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))(),
      Data$dList$dTypes.listMap(v1 => {
        if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Nil") { return Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v1._1._1._1, v1._2._1), v1._2._2); }
        return Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))();
      })(v._2)
    );
  }
  $runtime.fail();
};
const subpatts = v => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PConstr") { return Data$dList$dTypes.listMap(Data$dEither.Left)(v._1._2); }
    if (v._1.tag === "PRecord") { return Data$dList$dTypes.listMap(Data$dEither.Left)(Data$dList$dTypes.listMap(Data$dTuple.snd)(v._1._1)); }
    if (v._1.tag === "PListEmpty") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PListNonEmpty") {
      return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v._1._1), Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", v._1._2), Data$dList$dTypes.Nil));
    }
    $runtime.fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PListEnd") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PListNext") {
      return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v._1._1), Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", v._1._2), Data$dList$dTypes.Nil));
    }
  }
  $runtime.fail();
};
const showPattern = v => {
  if (v.tag === "Left") { return showPattern1.show(v._1); }
  if (v.tag === "Right") { return showListRestPattern.show(v._1); }
  $runtime.fail();
};
const popVarFwd = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return v => v1 => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Left" && v1._1._1._1._1.tag === "PVar") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._1._1._1;
        const $2 = v1._1._1._2;
        const $3 = v1._1._2._1;
        return Monad0.Bind1().Apply0().Functor0().map(v2 => Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple($2, Data$dTuple.$Tuple($3, $0)), v2))(popVarFwd(dictMonadError)(Util.assertWhen(false)("mustEq")(v$1 => v === $1)(v))(v1._2));
      }
      return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") { return Monad0.Applicative0().pure(Data$dList$dTypes.Nil); }
    return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
  };
};
const popVarFwd1 = /* #__PURE__ */ popVarFwd(monadErrorExceptT);
const popVarBwd = v => v1 => {
  if (v1.tag === "Cons") {
    return Data$dList$dTypes.$List(
      "Cons",
      Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", $Pattern("PVar", v)), v1._1._1), Data$dTuple.$Tuple(v1._1._2._1, v1._1._2._2)),
      popVarBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  $runtime.fail();
};
const popRecordFwd = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return v => v1 => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Left" && v1._1._1._1._1.tag === "PRecord") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._1._1._1;
        const $2 = v1._1._1._2;
        const $3 = v1._1._2._1;
        return Util.assertWith("")((() => {
          const go = v$1 => v1$1 => v2 => {
            if (!v2) { return false; }
            if (v$1.tag === "Nil") { return v1$1.tag === "Nil" && v2; }
            return v$1.tag === "Cons" && v1$1.tag === "Cons" && go(v$1._2)(v1$1._2)(v2 && v1$1._1 === v$1._1);
          };
          return go(Data$dList$dTypes.listMap(Data$dTuple.fst)($1))(v)(true);
        })())(Monad0.Bind1().Apply0().Functor0().map(v2 => Data$dList$dTypes.$List(
          "Cons",
          Data$dTuple.$Tuple(
            Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)($2)(Data$dList$dTypes.listMap(x => Data$dEither.$Either("Left", x._2))($1)),
            Data$dTuple.$Tuple($3, $0)
          ),
          v2
        ))(popRecordFwd(dictMonadError)(v)(v1._2)));
      }
      return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") { return Monad0.Applicative0().pure(Data$dList$dTypes.Nil); }
    return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
  };
};
const popRecordFwd1 = /* #__PURE__ */ popRecordFwd(monadErrorExceptT);
const popRecordBwd = v => v1 => {
  if (v1.tag === "Cons") {
    return Data$dList$dTypes.$List(
      "Cons",
      Data$dTuple.$Tuple(
        Data$dList$dTypes.$List(
          "Cons",
          Data$dEither.$Either(
            "Left",
            $Pattern(
              "PRecord",
              (() => {
                const go = go$a0$copy => go$a1$copy => go$a2$copy => {
                  let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
                  while (go$c) {
                    const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
                    if (v$1.tag === "Nil") {
                      go$c = false;
                      go$r = v2;
                      continue;
                    }
                    if (v1$1.tag === "Nil") {
                      go$c = false;
                      go$r = v2;
                      continue;
                    }
                    if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                      go$a0 = v$1._2;
                      go$a1 = v1$1._2;
                      go$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v$1._1, v1$1._1), v2);
                      continue;
                    }
                    $runtime.fail();
                  }
                  return go$r;
                };
                const go$1 = go$1$a0$copy => go$1$a1$copy => {
                  let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                  while (go$1$c) {
                    const v$1 = go$1$a0, v1$1 = go$1$a1;
                    if (v1$1.tag === "Nil") {
                      go$1$c = false;
                      go$1$r = v$1;
                      continue;
                    }
                    if (v1$1.tag === "Cons") {
                      go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                      go$1$a1 = v1$1._2;
                      continue;
                    }
                    $runtime.fail();
                  }
                  return go$1$r;
                };
                return go$1(Data$dList$dTypes.Nil)(go(v)(Data$dList$dTypes.listMap(v2 => {
                  if (v2.tag === "Left") { return v2._1; }
                  $runtime.fail();
                })(Data$dList.take((() => {
                  const go$2 = go$2$a0$copy => go$2$a1$copy => {
                    let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
                    while (go$2$c) {
                      const b = go$2$a0, v$1 = go$2$a1;
                      if (v$1.tag === "Nil") {
                        go$2$c = false;
                        go$2$r = b;
                        continue;
                      }
                      if (v$1.tag === "Cons") {
                        go$2$a0 = 1 + b | 0;
                        go$2$a1 = v$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$2$r;
                  };
                  return go$2(0)(v);
                })())(v1._1._1)))(Data$dList$dTypes.Nil));
              })()
            )
          ),
          Data$dList.drop((() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const b = go$a0, v$1 = go$a1;
                if (v$1.tag === "Nil") {
                  go$c = false;
                  go$r = b;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$a0 = 1 + b | 0;
                  go$a1 = v$1._2;
                  continue;
                }
                $runtime.fail();
              }
              return go$r;
            };
            return go(0)(v);
          })())(v1._1._1)
        ),
        Data$dTuple.$Tuple(v1._1._2._1, v1._1._2._2)
      ),
      popRecordBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  $runtime.fail();
};
const popListVarFwd = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return v => v1 => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Cons" && v1._1._1._1.tag === "Right" && v1._1._1._1._1.tag === "PListVar") {
        const $0 = v1._1._2._2;
        const $1 = v1._1._1._1._1._1;
        const $2 = v1._1._1._2;
        const $3 = v1._1._2._1;
        return Monad0.Bind1().Apply0().Functor0().map(v2 => Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple($2, Data$dTuple.$Tuple($3, $0)), v2))(popListVarFwd(dictMonadError)(Util.assertWhen(false)("mustEq")(v$1 => v === $1)(v))(v1._2));
      }
      return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
    }
    if (v1.tag === "Nil") { return Monad0.Applicative0().pure(Data$dList$dTypes.Nil); }
    return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
  };
};
const popListVarFwd1 = /* #__PURE__ */ popListVarFwd(monadErrorExceptT);
const popListVarBwd = v => v1 => {
  if (v1.tag === "Cons") {
    return Data$dList$dTypes.$List(
      "Cons",
      Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", $Pattern("PVar", v)), v1._1._1), Data$dTuple.$Tuple(v1._1._2._1, v1._1._2._2)),
      popListVarBwd(v)(v1._2)
    );
  }
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  $runtime.fail();
};
const popArgFwd = dictMonadError => {
  const MonadThrow0 = dictMonadError.MonadThrow0();
  const Monad0 = MonadThrow0.Monad0();
  return v => {
    if (v.tag === "Cons") {
      if (v._1._1.tag === "Nil" && v._1._2._1.tag === "Cons") {
        const $0 = v._1._2._1._1;
        const $1 = v._1._2._2;
        const $2 = v._1._2._1._2;
        return Monad0.Bind1().Apply0().Functor0().map(v1 => Data$dList$dTypes.$List(
          "Cons",
          Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", $0), Data$dList$dTypes.Nil), Data$dTuple.$Tuple($2, $1)),
          v1
        ))(popArgFwd(dictMonadError)(v._2));
      }
      return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
    }
    if (v.tag === "Nil") { return Monad0.Applicative0().pure(Data$dList$dTypes.Nil); }
    return MonadThrow0.throwError(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()));
  };
};
const popArgFwd1 = /* #__PURE__ */ popArgFwd(monadErrorExceptT);
const popArgBwd = v => {
  if (v.tag === "Cons") {
    if (v._1._1.tag === "Cons" && v._1._1._1.tag === "Left" && v._1._1._2.tag === "Nil") {
      return Data$dList$dTypes.$List(
        "Cons",
        Data$dTuple.$Tuple(Data$dList$dTypes.Nil, Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", v._1._1._1._1, v._1._2._1), v._1._2._2)),
        popArgBwd(v._2)
      );
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Nil") { return Data$dList$dTypes.Nil; }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const pVarAnon = /* #__PURE__ */ $Pattern("PVar", "_");
const pListVarAnon = /* #__PURE__ */ $ListRestPattern("PListVar", "_");
const unless = v => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PRecord") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PConstr") {
      return Data$dList$dTypes.listMap(c$p => Data$dEither.$Either(
        "Left",
        $Pattern(
          "PConstr",
          c$p,
          (() => {
            const go = go$a0$copy => go$a1$copy => {
              let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
              while (go$c) {
                const source = go$a0, memo = go$a1;
                if (source <= 0) {
                  const go$1 = go$1$a0$copy => go$1$a1$copy => {
                    let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                    while (go$1$c) {
                      const b = go$1$a0, v$1 = go$1$a1;
                      if (v$1.tag === "Nil") {
                        go$1$c = false;
                        go$1$r = b;
                        continue;
                      }
                      if (v$1.tag === "Cons") {
                        go$1$a0 = Data$dList$dTypes.$List("Cons", v$1._1, b);
                        go$1$a1 = v$1._2;
                        continue;
                      }
                      $runtime.fail();
                    }
                    return go$1$r;
                  };
                  go$c = false;
                  go$r = go$1(Data$dList$dTypes.Nil)(memo);
                  continue;
                }
                go$a0 = source - 1 | 0;
                go$a1 = Data$dList$dTypes.$List("Cons", $Pattern("PVar", "_"), memo);
              }
              return go$r;
            };
            return go(Util.defined(DataType.arity(monadThrowExceptT)(c$p)))(Data$dList$dTypes.Nil);
          })()
        )
      ))(difference(toUnfoldable(DataType.fromFoldable1(Util$dMap.mapObjectString.keys(Util.defined(DataType.dataTypeForCtr.dataTypeFor(monadThrowExceptT)(v._1._1))._2))))(Data$dList$dTypes.$List(
        "Cons",
        v._1._1,
        Data$dList$dTypes.Nil
      )));
    }
    if (v._1.tag === "PListEmpty") {
      return Data$dList$dTypes.$List(
        "Cons",
        Data$dEither.$Either(
          "Left",
          $Pattern(
            "PConstr",
            ":",
            (() => {
              const go = go$a0$copy => go$a1$copy => {
                let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
                while (go$c) {
                  const source = go$a0, memo = go$a1;
                  if (source <= 0) {
                    const go$1 = go$1$a0$copy => go$1$a1$copy => {
                      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
                      while (go$1$c) {
                        const b = go$1$a0, v$1 = go$1$a1;
                        if (v$1.tag === "Nil") {
                          go$1$c = false;
                          go$1$r = b;
                          continue;
                        }
                        if (v$1.tag === "Cons") {
                          go$1$a0 = Data$dList$dTypes.$List("Cons", v$1._1, b);
                          go$1$a1 = v$1._2;
                          continue;
                        }
                        $runtime.fail();
                      }
                      return go$1$r;
                    };
                    go$c = false;
                    go$r = go$1(Data$dList$dTypes.Nil)(memo);
                    continue;
                  }
                  go$a0 = source - 1 | 0;
                  go$a1 = Data$dList$dTypes.$List("Cons", $Pattern("PVar", "_"), memo);
                }
                return go$r;
              };
              return go(2)(Data$dList$dTypes.Nil);
            })()
          )
        ),
        Data$dList$dTypes.Nil
      );
    }
    if (v._1.tag === "PListNonEmpty") { return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", PListEmpty), Data$dList$dTypes.Nil); }
    $runtime.fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") { return Data$dList$dTypes.Nil; }
    if (v._1.tag === "PListNext") { return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", PListEnd), Data$dList$dTypes.Nil); }
    if (v._1.tag === "PListEnd") {
      return Data$dList$dTypes.$List(
        "Cons",
        Data$dEither.$Either("Right", $ListRestPattern("PListNext", $Pattern("PVar", "_"), $ListRestPattern("PListVar", "_"))),
        Data$dList$dTypes.Nil
      );
    }
  }
  $runtime.fail();
};
const forConstrFwd = v => v1 => v2 => {
  if (v2.tag === "Nil") { return Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v, Data$dList$dTypes.$List("Cons", v1, Data$dList$dTypes.Nil)), Data$dList$dTypes.Nil); }
  if (v2.tag === "Cons") {
    if (v === v2._1._1) { return Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v2._1._1, Data$dList$dTypes.$List("Cons", v1, v2._1._2)), v2._2); }
    return Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v2._1._1, v2._1._2), forConstrFwd(v)(v1)(v2._2));
  }
  $runtime.fail();
};
const forConstrBwd = v => v1 => {
  if (v1.tag === "Nil") { return Data$dMaybe.Nothing; }
  if (v1.tag === "Cons") {
    if (v === v1._1._1) {
      if (v1._1._2.tag === "Nil") { return Data$dMaybe.Nothing; }
      if (v1._1._2.tag === "Cons") {
        return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(v1._1._2._1, Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v1._1._1, v1._1._2._2), v1._2)));
      }
      $runtime.fail();
    }
    const $0 = forConstrBwd(v)(v1._2);
    if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple($0._1._1, Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v1._1._1, v1._1._2), $0._1._2))); }
    return Data$dMaybe.Nothing;
  }
  $runtime.fail();
};
const enil = α => Expr.$Expr("Constr", α, "Nil", Data$dList$dTypes.Nil);
const elimBool = κ => κ$p => Expr.$Elim("ElimConstr", fromFoldable([Data$dTuple.$Tuple("True", κ), Data$dTuple.$Tuple("False", κ$p)]));
const econs = α => e => e$p => Expr.$Expr("Constr", α, ":", Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil)));
const ctrFor = v => {
  if (v.tag === "Left") {
    if (v._1.tag === "PVar") { return Data$dMaybe.Nothing; }
    if (v._1.tag === "PConstr") { return Data$dMaybe.$Maybe("Just", v._1._1); }
    if (v._1.tag === "PRecord") { return Data$dMaybe.Nothing; }
    if (v._1.tag === "PListEmpty") { return Data$dMaybe.$Maybe("Just", "Nil"); }
    if (v._1.tag === "PListNonEmpty") { return Data$dMaybe.$Maybe("Just", ":"); }
    $runtime.fail();
  }
  if (v.tag === "Right") {
    if (v._1.tag === "PListVar") { return Data$dMaybe.Nothing; }
    if (v._1.tag === "PListEnd") { return Data$dMaybe.$Maybe("Just", "Nil"); }
    if (v._1.tag === "PListNext") { return Data$dMaybe.$Maybe("Just", ":"); }
  }
  $runtime.fail();
};
const popConstrBwd = v => v1 => {
  if (v1.tag === "Cons") {
    if (v1._1._1.tag === "Nil") { return Effect$dException.throwException(Effect$dException.error("absurd"))(); }
    if (v1._1._1.tag === "Cons") {
      const v2 = forConstrBwd(Util.definitely("absurd")((() => {
        if (v1._1._1._1.tag === "Left") {
          if (v1._1._1._1._1.tag === "PVar") { return Data$dMaybe.Nothing; }
          if (v1._1._1._1._1.tag === "PConstr") { return Data$dMaybe.$Maybe("Just", v1._1._1._1._1._1); }
          if (v1._1._1._1._1.tag === "PRecord") { return Data$dMaybe.Nothing; }
          if (v1._1._1._1._1.tag === "PListEmpty") { return Data$dMaybe.$Maybe("Just", "Nil"); }
          if (v1._1._1._1._1.tag === "PListNonEmpty") { return Data$dMaybe.$Maybe("Just", ":"); }
          $runtime.fail();
        }
        if (v1._1._1._1.tag === "Right") {
          if (v1._1._1._1._1.tag === "PListVar") { return Data$dMaybe.Nothing; }
          if (v1._1._1._1._1.tag === "PListEnd") { return Data$dMaybe.$Maybe("Just", "Nil"); }
          if (v1._1._1._1._1.tag === "PListNext") { return Data$dMaybe.$Maybe("Just", ":"); }
        }
        $runtime.fail();
      })()))(v);
      if (v2.tag === "Nothing") { return popConstrBwd(v)(v1._2); }
      if (v2.tag === "Just") {
        if (eqList.eq(v2._1._1._1)(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v1._1._1._2)(subpatts(v1._1._1._1))) && eq8(v2._1._1._2._1)(v1._1._2._1)) {
          return Data$dList$dTypes.$List(
            "Cons",
            Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", v1._1._1._1, v1._1._1._2), Data$dTuple.$Tuple(v1._1._2._1, v2._1._1._2._2)),
            popConstrBwd(v2._1._2)(v1._2)
          );
        }
        return popConstrBwd(v)(v1._2);
      }
    }
    $runtime.fail();
  }
  if (v1.tag === "Nil") { return Data$dList$dTypes.Nil; }
  $runtime.fail();
};
const popConstrFwd = dictMonadError => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  return v => v1 => {
    if (v1.tag === "Cons") {
      if (v1._1._1.tag === "Nil") { return Effect$dException.throwException(Effect$dException.error("absurd"))(); }
      if (v1._1._1.tag === "Cons") {
        const π = subpatts(v1._1._1._1);
        const c = Util.definitely("Failed to distinguish constructor: " + showPattern(v1._1._1._1))((() => {
          if (v1._1._1._1.tag === "Left") {
            if (v1._1._1._1._1.tag === "PVar") { return Data$dMaybe.Nothing; }
            if (v1._1._1._1._1.tag === "PConstr") { return Data$dMaybe.$Maybe("Just", v1._1._1._1._1._1); }
            if (v1._1._1._1._1.tag === "PRecord") { return Data$dMaybe.Nothing; }
            if (v1._1._1._1._1.tag === "PListEmpty") { return Data$dMaybe.$Maybe("Just", "Nil"); }
            if (v1._1._1._1._1.tag === "PListNonEmpty") { return Data$dMaybe.$Maybe("Just", ":"); }
            $runtime.fail();
          }
          if (v1._1._1._1.tag === "Right") {
            if (v1._1._1._1._1.tag === "PListVar") { return Data$dMaybe.Nothing; }
            if (v1._1._1._1._1.tag === "PListEnd") { return Data$dMaybe.$Maybe("Just", "Nil"); }
            if (v1._1._1._1._1.tag === "PListNext") { return Data$dMaybe.$Maybe("Just", ":"); }
          }
          $runtime.fail();
        })());
        return Util.assertWith("")((() => {
          const go = go$a0$copy => go$a1$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
            while (go$c) {
              const b = go$a0, v$1 = go$a1;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = b;
                continue;
              }
              if (v$1.tag === "Cons") {
                go$a0 = 1 + b | 0;
                go$a1 = v$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          return go(0)(π) === Util.defined(DataType.arity(monadThrowExceptT)(c)) && Util.defined(DataType.dataTypeForCtr.dataTypeFor(monadThrowExceptT)(c))._1 === v._1;
        })())(Monad0.Bind1().Apply0().Functor0().map(forConstrFwd(c)(Data$dTuple.$Tuple(
          Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v1._1._1._2)(π),
          Data$dTuple.$Tuple(v1._1._2._1, v1._1._2._2)
        )))(popConstrFwd(dictMonadError)(v)(v1._2)));
      }
      $runtime.fail();
    }
    if (v1.tag === "Nil") { return Monad0.Applicative0().pure(Data$dList$dTypes.Nil); }
    $runtime.fail();
  };
};
const popConstrFwd1 = /* #__PURE__ */ popConstrFwd(monadErrorExceptT);
const anon = v => {
  if (v.tag === "Left") { return Data$dEither.$Either("Left", $Pattern("PVar", "_")); }
  if (v.tag === "Right") { return Data$dEither.$Either("Right", $ListRestPattern("PListVar", "_")); }
  $runtime.fail();
};
const orElseBwd = dictBoundedJoinSemilattice => {
  const bot = dictBoundedJoinSemilattice.bot;
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  return v => ks => {
    if (v._1.tag === "Nil") {
      if (ks._1._1.tag === "Nil" && ks._2.tag === "Nil") { return Data$dTuple.$Tuple(bot, ks._1._2); }
      $runtime.fail();
    }
    if (v._1.tag === "Cons") {
      const $1 = v._1._2;
      const popIfPresent = v1 => v2 => {
        if (v1.tag === "Nil") { return Data$dTuple.$Tuple(bot, v2); }
        const v3 = Data$dList$dNonEmpty.unsnoc(v2);
        const v4 = Data$dList$dNonEmpty.unsnoc(Util.nonEmptyListNonEmptyList.nonEmpty(v1));
        if (!eqList.eq(Data$dList$dTypes.$List("Cons", v4.last, Data$dList$dTypes.listMap(anon)($1)))(v3.last._1)) { return popIfPresent(v4.init)(v2); }
        const $2 = popIfPresent(v4.init)(Util.nonEmptyListNonEmptyList.nonEmpty(v3.init));
        return Data$dTuple.$Tuple(
          $0.join($2._1)((() => {
            if (v3.last._2.tag === "ListEmpty") { return v3.last._2._1; }
            $runtime.fail();
          })()),
          $2._2
        );
      };
      const $2 = popIfPresent(unless(v._1._1))(ks);
      const $3 = orElseBwd(dictBoundedJoinSemilattice)(Data$dTuple.$Tuple(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)($1)(subpatts(v._1._1)), v._2))(Data$dNonEmpty.$NonEmpty(
        (() => {
          if ($2._2._1._1.tag === "Cons") {
            return Data$dTuple.$Tuple(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)($2._2._1._1._2)(subpatts($2._2._1._1._1)), $2._2._1._2);
          }
          $runtime.fail();
        })(),
        Data$dList$dTypes.listMap(v2 => {
          if (v2._1.tag === "Cons") { return Data$dTuple.$Tuple(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(v2._1._2)(subpatts(v2._1._1)), v2._2); }
          $runtime.fail();
        })($2._2._2)
      ));
      return Data$dTuple.$Tuple($0.join($3._1)($2._1), $3._2);
    }
    $runtime.fail();
  };
};
const orElseFwd = α => v => {
  if (v._1.tag === "Nil") { return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple(Data$dList$dTypes.Nil, v._2), Data$dList$dTypes.Nil); }
  if (v._1.tag === "Cons") {
    const $0 = v._1._2;
    const π$p = subpatts(v._1._1);
    const $1 = orElseFwd(α)(Data$dTuple.$Tuple(Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)($0)(π$p), v._2));
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v$1 = go$a1;
        if (v$1.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v$1.tag === "Cons") {
          go$a0 = 1 + b | 0;
          go$a1 = v$1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    const $2 = go(0)(π$p);
    const $3 = v$1 => Data$dTuple.$Tuple(Data$dList.take($2)(v$1._1), Data$dTuple.$Tuple(Data$dList.drop($2)(v$1._1), v$1._2));
    const $4 = (() => {
      if (v._1._1.tag === "Left") {
        if (v._1._1._1.tag === "PVar") {
          const $4 = v._1._1._1._1;
          return v1 => Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", $Pattern("PVar", $4)), v1._2._1), v1._2._2);
        }
        if (v._1._1._1.tag === "PRecord") {
          const $4 = v._1._1._1._1;
          return v1 => Data$dTuple.$Tuple(
            Data$dList$dTypes.$List(
              "Cons",
              Data$dEither.$Either(
                "Left",
                $Pattern(
                  "PRecord",
                  (() => {
                    const go$1 = go$1$a0$copy => go$1$a1$copy => go$1$a2$copy => {
                      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$a2 = go$1$a2$copy, go$1$c = true, go$1$r;
                      while (go$1$c) {
                        const v$1 = go$1$a0, v1$1 = go$1$a1, v2 = go$1$a2;
                        if (v$1.tag === "Nil") {
                          go$1$c = false;
                          go$1$r = v2;
                          continue;
                        }
                        if (v1$1.tag === "Nil") {
                          go$1$c = false;
                          go$1$r = v2;
                          continue;
                        }
                        if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                          go$1$a0 = v$1._2;
                          go$1$a1 = v1$1._2;
                          go$1$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v$1._1, v1$1._1), v2);
                          continue;
                        }
                        $runtime.fail();
                      }
                      return go$1$r;
                    };
                    const go$2 = go$2$a0$copy => go$2$a1$copy => {
                      let go$2$a0 = go$2$a0$copy, go$2$a1 = go$2$a1$copy, go$2$c = true, go$2$r;
                      while (go$2$c) {
                        const v$1 = go$2$a0, v1$1 = go$2$a1;
                        if (v1$1.tag === "Nil") {
                          go$2$c = false;
                          go$2$r = v$1;
                          continue;
                        }
                        if (v1$1.tag === "Cons") {
                          go$2$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                          go$2$a1 = v1$1._2;
                          continue;
                        }
                        $runtime.fail();
                      }
                      return go$2$r;
                    };
                    return go$2(Data$dList$dTypes.Nil)(go$1(Data$dList$dTypes.listMap(Data$dTuple.fst)($4))(Data$dList$dTypes.listMap(v2 => {
                      if (v2.tag === "Left") { return v2._1; }
                      $runtime.fail();
                    })(v1._1))(Data$dList$dTypes.Nil));
                  })()
                )
              ),
              v1._2._1
            ),
            v1._2._2
          );
        }
        if (v._1._1._1.tag === "PConstr") {
          const $4 = v._1._1._1._1;
          return v1 => Data$dTuple.$Tuple(
            Data$dList$dTypes.$List(
              "Cons",
              Data$dEither.$Either(
                "Left",
                $Pattern(
                  "PConstr",
                  $4,
                  Data$dList$dTypes.listMap(v2 => {
                    if (v2.tag === "Left") { return v2._1; }
                    $runtime.fail();
                  })(v1._1)
                )
              ),
              v1._2._1
            ),
            v1._2._2
          );
        }
        if (v._1._1._1.tag === "PListEmpty") { return v1 => Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", PListEmpty), v1._2._1), v1._2._2); }
        if (v._1._1._1.tag === "PListNonEmpty") {
          return v1 => {
            if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Cons" && v1._1._2._1.tag === "Right" && v1._1._2._2.tag === "Nil") {
              return Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", $Pattern("PListNonEmpty", v1._1._1._1, v1._1._2._1._1)), v1._2._1), v1._2._2);
            }
            $runtime.fail();
          };
        }
        $runtime.fail();
      }
      if (v._1._1.tag === "Right") {
        if (v._1._1._1.tag === "PListVar") {
          const $4 = v._1._1._1._1;
          return v1 => Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", $ListRestPattern("PListVar", $4)), v1._2._1), v1._2._2);
        }
        if (v._1._1._1.tag === "PListNext") {
          return v1 => {
            if (v1._1.tag === "Cons" && v1._1._1.tag === "Left" && v1._1._2.tag === "Cons" && v1._1._2._1.tag === "Right" && v1._1._2._2.tag === "Nil") {
              return Data$dTuple.$Tuple(
                Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", $ListRestPattern("PListNext", v1._1._1._1, v1._1._2._1._1)), v1._2._1),
                v1._2._2
              );
            }
            $runtime.fail();
          };
        }
        if (v._1._1._1.tag === "PListEnd") { return v1 => Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", PListEnd), v1._2._1), v1._2._2); }
      }
      $runtime.fail();
    })();
    return Data$dNonEmpty.$NonEmpty(
      $4($3($1._1)),
      Data$dList$dTypes.foldableList.foldr(Data$dList$dTypes.Cons)(Data$dList$dTypes.listMap(p$p => Data$dTuple.$Tuple(
        Data$dList$dTypes.$List("Cons", p$p, Data$dList$dTypes.listMap(anon)($0)),
        $Expr("ListEmpty", α)
      ))(unless(v._1._1)))(Data$dList$dTypes.listMap($4)(Data$dList$dTypes.listMap($5 => $3($5))($1._2)))
    );
  }
  $runtime.fail();
};
const desugarableListRestExpr = {
  desug: dictMonadError => dictBoundedLattice => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Apply0 = Monad0.Bind1().Apply0();
    return v => {
      if (v.tag === "End") { return Monad0.Applicative0().pure(Expr.$Expr("Constr", v._1, "Nil", Data$dList$dTypes.Nil)); }
      if (v.tag === "Next") {
        const $0 = v._1;
        return Apply0.apply(Apply0.Functor0().map(e => e$p => Expr.$Expr(
          "Constr",
          $0,
          ":",
          Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil))
        ))(desugarableExprExpr.desug(dictMonadError)(dictBoundedLattice)(v._2)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)(v._3));
      }
      $runtime.fail();
    };
  },
  desugBwd: dictBoundedJoinSemilattice => v => v1 => {
    if (v.tag === "Constr") {
      if (v1.tag === "End") { return $ListRest("End", v._1); }
      if (v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v1.tag === "Next") {
        return $ListRest(
          "Next",
          v._1,
          desugarableExprExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2),
          desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._3)
        );
      }
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  },
  Functor0: () => functorListRest,
  Functor1: () => Expr.functorExpr
};
const desugarableExprExpr = {
  desug: dictMonadError => dictBoundedLattice => exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()),
  desugBwd: dictBoundedJoinSemilattice => exprBwd(dictBoundedJoinSemilattice),
  Functor0: () => functorExpr,
  Functor1: () => Expr.functorExpr
};
const desugarableDictEntryExpr = {
  desug: dictMonadError => dictBoundedLattice => v => {
    if (v.tag === "ExprKey") { return exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1); }
    if (v.tag === "VarKey") { return dictMonadError.MonadThrow0().Monad0().Applicative0().pure(Expr.$Expr("Str", v._1, v._2)); }
    $runtime.fail();
  },
  desugBwd: dictBoundedJoinSemilattice => v => v1 => {
    if (v1.tag === "ExprKey") { return $DictEntry("ExprKey", exprBwd(dictBoundedJoinSemilattice)(v)(v1._1)); }
    return varKeyBwd(v)(v1);
  },
  Functor0: () => functorDictEntry,
  Functor1: () => Expr.functorExpr
};
const desugarableClausesElim = {
  desug: dictMonadError => dictBoundedLattice => {
    const $0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0();
    return μ => $0.map(Expr.asElim)(clausesStateFwd(dictBoundedLattice)(dictMonadError)(toClausesStateFwd(μ)));
  },
  desugBwd: dictBoundedJoinSemilattice => σ => μ => toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", σ))(toClausesStateFwd(μ))),
  Functor0: () => functorClauses,
  Functor1: () => Expr.functorElim
};
const varDefsFwd = dictMonadError => {
  const Apply0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0();
  const $0 = Apply0.Functor0();
  return dictBoundedLattice => v => {
    if (v._1._2.tag === "Nil") {
      return Apply0.apply($0.map(Expr.Let)(varDefFwd(dictMonadError)(dictBoundedLattice)(v._1._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
    }
    if (v._1._2.tag === "Cons") {
      return Apply0.apply($0.map(Expr.Let)(varDefFwd(dictMonadError)(dictBoundedLattice)(v._1._1)))(varDefsFwd(dictMonadError)(dictBoundedLattice)(Data$dTuple.$Tuple(
        Data$dNonEmpty.$NonEmpty(v._1._2._1, v._1._2._2),
        v._2
      )));
    }
    $runtime.fail();
  };
};
const varDefsBwd = dictBoundedJoinSemilattice => v => v1 => {
  if (v.tag === "Let") {
    if (v1._1._2.tag === "Nil") {
      return Data$dTuple.$Tuple(
        Data$dNonEmpty.$NonEmpty($VarDef(v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1._1._2)), Data$dList$dTypes.Nil),
        exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)
      );
    }
    if (v1._1._2.tag === "Cons") {
      const v2 = varDefsBwd(dictBoundedJoinSemilattice)(v._2)(Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v1._1._2._1, v1._1._2._2), v1._2));
      return Data$dTuple.$Tuple(
        Data$dNonEmpty.$NonEmpty($VarDef(v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1._1._2)), Data$dList$dTypes.$List("Cons", v2._1._1, v2._1._2)),
        v2._2
      );
    }
  }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const varDefFwd = dictMonadError => {
  const Apply0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0();
  return dictBoundedLattice => {
    const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
    return v => Apply0.apply(Apply0.Functor0().map(Expr.VarDef)(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(Data$dNonEmpty.$NonEmpty(
      Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v._1, Data$dList$dTypes.Nil), $Expr("Dictionary", top, Data$dList$dTypes.Nil)),
      Data$dList$dTypes.Nil
    ))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
  };
};
const recDefsFwd = dictMonadError => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const traverse2 = Data$dList$dTypes.traversableNonEmptyList.traverse(Monad0.Applicative0());
  return dictBoundedLattice => {
    const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
    return xcs => Monad0.Bind1().Apply0().Functor0().map((() => {
      const $0 = Expr.RecDefs(top);
      return x => $0(fromFoldable1(x));
    })())(traverse2(recDefFwd(dictMonadError)(dictBoundedLattice))((() => {
      const $0 = Data$dList$dNonEmpty.wrappedOperation("groupBy")(Data$dList.groupBy(x => y => x._1 === y._1))(xcs);
      return Data$dNonEmpty.$NonEmpty($0._1, Data$dList$dTypes.listMap(RecDef)($0._2));
    })()));
  };
};
const recDefsBwd = dictBoundedJoinSemilattice => v => xcs => {
  const $0 = v._2;
  const go = v1 => Data$dNonEmpty.$NonEmpty(
    recDefBwd(dictBoundedJoinSemilattice)(Data$dTuple.$Tuple(v1._1._1._1, Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(v1._1._1._1)($0)))(v1._1),
    (() => {
      if (v1._2.tag === "Nil") { return Data$dList$dTypes.Nil; }
      if (v1._2.tag === "Cons") {
        const $1 = go(Data$dNonEmpty.$NonEmpty(v1._2._1, v1._2._2));
        return Data$dList$dTypes.$List("Cons", $1._1, $1._2);
      }
      $runtime.fail();
    })()
  );
  return Data$dList$dTypes.bindNonEmptyList.bind(go(Data$dList$dNonEmpty.wrappedOperation("groupBy")(Data$dList.groupBy(x => y => x._1 === y._1))(xcs)))(Control$dBind.identity);
};
const recDefFwd = dictMonadError => dictBoundedLattice => xcs => dictMonadError.MonadThrow0().Monad0().Bind1().Apply0().Functor0().map(v => Data$dTuple.$Tuple(xcs._1._1, v))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(Data$dNonEmpty.$NonEmpty(
  xcs._1._2,
  Data$dList$dTypes.listMap(Data$dTuple.snd)(xcs._2)
)));
const recDefBwd = dictBoundedJoinSemilattice => v => v1 => {
  const $0 = v._1;
  const $1 = toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", v._2))(toClausesStateFwd(Data$dNonEmpty.$NonEmpty(
    v1._1._2,
    Data$dList$dTypes.listMap(Data$dTuple.snd)(v1._2)
  ))));
  return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple($0, $1._1), Data$dList$dTypes.listMap(v2 => Data$dTuple.$Tuple($0, v2))($1._2));
};
const paragraphFwd = dictBoundedLattice => {
  const bot = dictBoundedLattice.BoundedJoinSemilattice0().bot;
  return dictMonadError => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    return elems => Monad0.Bind1().bind(paragraphElemsFwd(dictBoundedLattice)(dictMonadError)(elems))(es => Monad0.Applicative0().pure(Expr.$Expr(
      "Constr",
      bot,
      "Paragraph",
      Data$dList$dTypes.$List("Cons", es, Data$dList$dTypes.Nil)
    )));
  };
};
const paragraphElemsFwd = dictBoundedLattice => {
  const bot = dictBoundedLattice.BoundedJoinSemilattice0().bot;
  return dictMonadError => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const $0 = Monad0.Applicative0();
    const $1 = Monad0.Bind1();
    return v => {
      if (v.tag === "Nil") { return $0.pure(Expr.$Expr("Constr", bot, "Nil", Data$dList$dTypes.Nil)); }
      if (v.tag === "Cons") {
        if (v._1.tag === "Token") {
          const $2 = v._1._1;
          return $1.bind(paragraphElemsFwd(dictBoundedLattice)(dictMonadError)(v._2))(e$p => $0.pure(Expr.$Expr(
            "Constr",
            bot,
            ":",
            Data$dList$dTypes.$List("Cons", Expr.$Expr("Str", bot, $2), Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil))
          )));
        }
        if (v._1.tag === "Unquote") {
          const $2 = v._2;
          return $1.bind(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1._1))(e => $1.bind(paragraphElemsFwd(dictBoundedLattice)(dictMonadError)($2))(e$p => $0.pure(Expr.$Expr(
            "Constr",
            bot,
            ":",
            Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil))
          ))));
        }
      }
      $runtime.fail();
    };
  };
};
const paragraphElemsBwd = dictBoundedJoinSemilattice => v => v1 => {
  if (v.tag === "Constr") {
    if (v1.tag === "Nil") {
      if (v._3.tag === "Nil" && v._2 === "Nil") { return Data$dList$dTypes.Nil; }
      return Effect$dException.throwException(Effect$dException.error("absurd"))();
    }
    if (v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v1.tag === "Cons" && v._2 === ":") {
      if (v._3._1.tag === "Str" && v1._1.tag === "Token") {
        return Data$dList$dTypes.$List("Cons", $ParagraphElem("Token", v._3._1._2), paragraphElemsBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._2));
      }
      if (v1._1.tag === "Unquote") {
        return Data$dList$dTypes.$List(
          "Cons",
          $ParagraphElem("Unquote", exprBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._1._1)),
          paragraphElemsBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._2)
        );
      }
    }
  }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const listCompFwd = dictMonadError => {
  const Bind1 = dictMonadError.MonadThrow0().Monad0().Bind1();
  const Functor0 = Bind1.Apply0().Functor0();
  return dictBoundedLattice => v => {
    if (v._2._1.tag === "Nil") {
      const $0 = v._1;
      return Functor0.map(f => f(Expr.$Expr("Constr", $0, "Nil", Data$dList$dTypes.Nil)))(Functor0.map(e => e$p => Expr.$Expr(
        "Constr",
        $0,
        ":",
        Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil))
      ))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2._2)));
    }
    if (v._2._1.tag === "Cons") {
      if (v._2._1._1.tag === "ListCompGuard") {
        const $0 = v._2._1._1._1;
        const $1 = v._1;
        return Bind1.bind(listCompFwd(dictMonadError)(dictBoundedLattice)(Data$dTuple.$Tuple($1, Data$dTuple.$Tuple(v._2._1._2, v._2._2))))(e => Functor0.map(Expr.App(Expr.$Expr(
          "Lambda",
          $1,
          Expr.$Elim(
            "ElimConstr",
            fromFoldable([
              Data$dTuple.$Tuple("True", Expr.$Cont("ContExpr", e)),
              Data$dTuple.$Tuple("False", Expr.$Cont("ContExpr", Expr.$Expr("Constr", $1, "Nil", Data$dList$dTypes.Nil)))
            ])
          )
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
      if (v._2._1._1.tag === "ListCompDecl") {
        const $0 = v._2._1._1._1._2;
        const $1 = v._1;
        return Bind1.bind(clausesStateFwd(dictBoundedLattice)(dictMonadError)(Data$dList$dTypes.$List(
          "Cons",
          Data$dTuple.$Tuple(
            Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v._2._1._1._1._1), Data$dList$dTypes.Nil),
            Data$dTuple.$Tuple(Data$dList$dTypes.Nil, $Expr("ListComp", $1, v._2._2, v._2._1._2))
          ),
          Data$dList$dTypes.Nil
        )))(σ => Functor0.map(Expr.App(Expr.$Expr("Lambda", $1, σ.tag === "ContElim" ? σ._1 : Effect$dException.throwException(Effect$dException.error("Eliminator expected"))())))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
      if (v._2._1._1.tag === "ListCompGen") {
        const $0 = v._2._1._1._2;
        const $1 = v._1;
        return Bind1.bind(clausesStateFwd(dictBoundedLattice)(dictMonadError)((() => {
          const $2 = orElseFwd($1)(Data$dTuple.$Tuple(
            Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v._2._1._1._1), Data$dList$dTypes.Nil),
            $Expr("ListComp", $1, v._2._2, v._2._1._2)
          ));
          return Data$dList$dTypes.$List(
            "Cons",
            Data$dTuple.$Tuple($2._1._1, Data$dTuple.$Tuple(Data$dList$dTypes.Nil, $2._1._2)),
            Data$dList$dTypes.listMap(m => Data$dTuple.$Tuple(m._1, Data$dTuple.$Tuple(Data$dList$dTypes.Nil, m._2)))($2._2)
          );
        })()))(σ => Functor0.map(Expr.App(Expr.$Expr(
          "App",
          Expr.$Expr("Var", "concat_map"),
          Expr.$Expr("Lambda", $1, σ.tag === "ContElim" ? σ._1 : Effect$dException.throwException(Effect$dException.error("Eliminator expected"))())
        )))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())($0)));
      }
    }
    $runtime.fail();
  };
};
const listCompBwd = dictBoundedJoinSemilattice => {
  const $0 = dictBoundedJoinSemilattice.JoinSemilattice0();
  const orElseBwd1 = orElseBwd(dictBoundedJoinSemilattice);
  return v => v1 => {
    const $1 = (e, p, qs, s0, s0$p, α$p, σ) => {
      const $1 = clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", σ))(Data$dList$dTypes.$List(
        "Cons",
        Data$dTuple.$Tuple(
          Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", p), Data$dList$dTypes.Nil),
          Data$dTuple.$Tuple(Data$dList$dTypes.Nil, $Expr("ListComp", undefined, s0$p, qs))
        ),
        Data$dList$dTypes.Nil
      ));
      if (
        $1.tag === "Cons" && $1._1._1.tag === "Cons" && $1._1._1._1.tag === "Left" && $1._1._1._2.tag === "Nil" && $1._1._2._1.tag === "Nil" && $1._1._2._2.tag === "ListComp" && $1._2.tag === "Nil"
      ) {
        return Data$dTuple.$Tuple(
          $0.join($1._1._2._2._1)(α$p),
          Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", $Qualifier("ListCompDecl", $VarDef(p, exprBwd(dictBoundedJoinSemilattice)(e)(s0))), $1._1._2._2._3), $1._1._2._2._2)
        );
      }
      $runtime.fail();
    };
    if (v.tag === "Constr") {
      if (
        v._3.tag === "Cons" && v._3._2.tag === "Cons" && v._3._2._1.tag === "Constr" && v._3._2._1._3.tag === "Nil" && v._3._2._2.tag === "Nil" && v1._1.tag === "Nil" && v._2 === ":" && v._3._2._1._2 === "Nil"
      ) {
        return Data$dTuple.$Tuple($0.join(v._3._2._1._1)(v._1), Data$dTuple.$Tuple(Data$dList$dTypes.Nil, exprBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2)));
      }
      return Effect$dException.throwException(Effect$dException.error("absurd"))();
    }
    if (v.tag === "App" && v1._1.tag === "Cons") {
      if (v._1.tag === "Lambda") {
        if (v._1._2.tag === "ElimConstr" && v1._1._1.tag === "ListCompGuard") {
          const $2 = listCompBwd(dictBoundedJoinSemilattice)((() => {
            const $2 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("True")(v._1._2._1);
            if ($2.tag === "ContExpr") { return $2._1; }
            return Effect$dException.throwException(Effect$dException.error("Expression expected"))();
          })())(Data$dTuple.$Tuple(v1._1._2, v1._2));
          const $3 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("False")(v._1._2._1);
          const $4 = $3.tag === "ContExpr" ? $3._1 : Effect$dException.throwException(Effect$dException.error("Expression expected"))();
          if ($4.tag === "Constr" && $4._3.tag === "Nil" && $4._2 === "Nil") {
            return Data$dTuple.$Tuple(
              $0.join($0.join($2._1)(v._1._1))($4._1),
              Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", $Qualifier("ListCompGuard", exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._1)), $2._2._1), $2._2._2)
            );
          }
          $runtime.fail();
        }
        if (v1._1._1.tag === "ListCompDecl") { return $1(v._2, v1._1._1._1._1, v1._1._2, v1._1._1._1._2, v1._2, v._1._1, v._1._2); }
        return Effect$dException.throwException(Effect$dException.error("absurd"))();
      }
      if (v._1.tag === "App" && v._1._1.tag === "Var" && v._1._1._1 === "concat_map" && v._1._2.tag === "Lambda" && v1._1._1.tag === "ListCompGen") {
        const $2 = orElseBwd1(Data$dTuple.$Tuple(
          Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v1._1._1._1), Data$dList$dTypes.Nil),
          $Expr("ListComp", undefined, v1._2, v1._1._2)
        ))((() => {
          const $2 = Util.nonEmptyListNonEmptyList.nonEmpty(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", v._1._2._2))((() => {
            const $2 = orElseFwd()(Data$dTuple.$Tuple(
              Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Left", v1._1._1._1), Data$dList$dTypes.Nil),
              $Expr("ListComp", undefined, v1._2, v1._1._2)
            ));
            return Data$dList$dTypes.$List(
              "Cons",
              Data$dTuple.$Tuple($2._1._1, Data$dTuple.$Tuple(Data$dList$dTypes.Nil, $2._1._2)),
              Data$dList$dTypes.listMap(m => Data$dTuple.$Tuple(m._1, Data$dTuple.$Tuple(Data$dList$dTypes.Nil, m._2)))($2._2)
            );
          })()));
          return Data$dNonEmpty.$NonEmpty(
            (() => {
              if ($2._1._2._1.tag === "Nil") { return Data$dTuple.$Tuple($2._1._1, $2._1._2._2); }
              $runtime.fail();
            })(),
            Data$dList$dTypes.listMap(v2 => {
              if (v2._2._1.tag === "Nil") { return Data$dTuple.$Tuple(v2._1, v2._2._2); }
              $runtime.fail();
            })($2._2)
          );
        })());
        if ($2._2.tag === "ListComp") {
          return Data$dTuple.$Tuple(
            $0.join($0.join($2._2._1)(v._1._2._1))($2._1),
            Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", $Qualifier("ListCompGen", v1._1._1._1, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._2)), $2._2._3), $2._2._2)
          );
        }
        $runtime.fail();
      }
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  };
};
const ifElseFwd = dictBoundedLattice => {
  const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
  return dictMonadError => {
    const Apply0 = dictMonadError.MonadThrow0().Monad0().Bind1().Apply0();
    const $0 = Apply0.Functor0();
    return v => {
      const $1 = (v1, e3) => Apply0.apply($0.map(Expr.App)($0.map(Expr.Lambda(top))(Apply0.apply($0.map(elimBool)($0.map(Expr.ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v1._2))))($0.map(Expr.ContExpr)(e3)))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v1._1));
      return $1(
        v._1._1,
        Data$dList$dTypes.foldableList.foldr($2 => $3 => $1($2, $3))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2))(v._1._2)
      );
    };
  };
};
const ifElseBwd = dictBoundedJoinSemilattice => {
  const botOf = functorExpr.map((() => {
    const $0 = dictBoundedJoinSemilattice.bot;
    return v => $0;
  })());
  return v => v1 => {
    if (v.tag === "App" && v._1.tag === "Lambda" && v._1._2.tag === "ElimConstr") {
      const $0 = v._1._2._1;
      const bwdWhen = c => {
        if (Object.hasOwn($0, c)) {
          return exprBwd(dictBoundedJoinSemilattice)((() => {
            const $1 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(c)($0);
            if ($1.tag === "ContExpr") { return $1._1; }
            return Effect$dException.throwException(Effect$dException.error("Expression expected"))();
          })());
        }
        return botOf;
      };
      if (v1._1._2.tag === "Nil") {
        return Data$dTuple.$Tuple(
          Util.nonEmptyListNonEmptyList.nonEmpty(Data$dList$dTypes.$List(
            "Cons",
            Data$dTuple.$Tuple(exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._1), bwdWhen("True")(v1._1._1._2)),
            Data$dList$dTypes.Nil
          )),
          bwdWhen("False")(v1._2)
        );
      }
      const v3 = bwdWhen("False")($Expr("IfElse", Util.nonEmptyListNonEmptyList.nonEmpty(v1._1._2), v1._2));
      if (v3.tag === "IfElse") {
        return Data$dTuple.$Tuple(
          Util.nonEmptyListNonEmptyList.nonEmpty(Data$dList$dTypes.$List(
            "Cons",
            Data$dTuple.$Tuple(exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._1), bwdWhen("True")(v1._1._1._2)),
            Data$dList$dTypes.$List("Cons", v3._1._1, v3._1._2)
          )),
          v3._2
        );
      }
      return Data$dTuple.$Tuple(
        Util.nonEmptyListNonEmptyList.nonEmpty(Data$dList$dTypes.$List(
          "Cons",
          Data$dTuple.$Tuple(exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1._1._1), bwdWhen("True")(v1._1._1._2)),
          Effect$dException.throwException(Effect$dException.error("absurd"))()._1
        )),
        Effect$dException.throwException(Effect$dException.error("absurd"))()._2
      );
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  };
};
const exprFwd = dictBoundedLattice => {
  const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
  const JoinSemilattice0 = dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0();
  return dictMonadError => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Applicative0 = Monad0.Applicative0();
    const Bind1 = Monad0.Bind1();
    const Apply0 = Bind1.Apply0();
    const Functor0 = Apply0.Functor0();
    const traverse2 = Data$dList$dTypes.traversableList.traverse(Applicative0);
    return dictJoinSemilattice => v => {
      if (v.tag === "Var") { return Applicative0.pure(Expr.$Expr("Var", v._1)); }
      if (v.tag === "Op") { return Applicative0.pure(Expr.$Expr("Op", v._1)); }
      if (v.tag === "Int") { return Applicative0.pure(Expr.$Expr("Int", v._1, v._2)); }
      if (v.tag === "Float") { return Applicative0.pure(Expr.$Expr("Float", v._1, v._2)); }
      if (v.tag === "Str") { return Applicative0.pure(Expr.$Expr("Str", v._1, v._2)); }
      if (v.tag === "Constr") {
        return Functor0.map(Expr.Constr(v._1)(v._2))(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))(v._3));
      }
      if (v.tag === "Dictionary") {
        const $0 = v._1;
        const v1 = Data$dList.unzip(v._2);
        const $1 = v1._2;
        return Bind1.bind(traverse2(desugarableDictEntryExpr.desug(dictMonadError)(dictBoundedLattice))(v1._1))(ks$p => Bind1.bind(traverse2(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0()))($1))(es => Functor0.map(Expr.Dictionary($0))(Applicative0.pure((() => {
          const go = go$a0$copy => go$a1$copy => go$a2$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v1$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                go$a0 = v$1._2;
                go$a1 = v1$1._2;
                go$a2 = Data$dList$dTypes.$List("Cons", Util$dPair.$Pair(v$1._1, v1$1._1), v2);
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v$1 = go$1$a0, v1$1 = go$1$a1;
              if (v1$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v$1;
                continue;
              }
              if (v1$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$1$a1 = v1$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(Data$dList$dTypes.Nil)(go(ks$p)(es)(Data$dList$dTypes.Nil));
        })()))));
      }
      if (v.tag === "Matrix") {
        return Apply0.apply(Functor0.map(f => f(Data$dTuple.$Tuple(v._3._1, v._3._2)))(Functor0.map(Expr.Matrix(v._1))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._4));
      }
      if (v.tag === "Lambda") { return Functor0.map(Expr.Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(v._1)); }
      if (v.tag === "Project") {
        return Functor0.map(f => f(Expr.$Expr("Str", top, v._2)))(Functor0.map(Expr.DProject)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)));
      }
      if (v.tag === "DProject") {
        return Apply0.apply(Functor0.map(Expr.DProject)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "App") {
        return Apply0.apply(Functor0.map(Expr.App)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "BinaryApp") {
        return Apply0.apply(Functor0.map(Expr.App)(Functor0.map(Expr.App(Expr.$Expr("Op", v._2)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._3));
      }
      if (v.tag === "MatchAs") {
        return Apply0.apply(Functor0.map(Expr.App)(Functor0.map(Expr.Lambda(top))(desugarableClausesElim.desug(dictMonadError)(dictBoundedLattice)(Data$dNonEmpty.$NonEmpty(
          Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v._2._1._1, Data$dList$dTypes.Nil), v._2._1._2),
          Data$dList$dTypes.listMap(x => Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(x._1, Data$dList$dTypes.Nil), x._2))(v._2._2)
        )))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1));
      }
      if (v.tag === "IfElse") { return ifElseFwd(dictBoundedLattice)(dictMonadError)(Data$dTuple.$Tuple(v._1, v._2)); }
      if (v.tag === "Paragraph") { return paragraphFwd(dictBoundedLattice)(dictMonadError)(v._1); }
      if (v.tag === "ListEmpty") { return Applicative0.pure(Expr.$Expr("Constr", v._1, "Nil", Data$dList$dTypes.Nil)); }
      if (v.tag === "ListNonEmpty") {
        const $0 = v._1;
        return Apply0.apply(Functor0.map(e => e$p => Expr.$Expr("Constr", $0, ":", Data$dList$dTypes.$List("Cons", e, Data$dList$dTypes.$List("Cons", e$p, Data$dList$dTypes.Nil))))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2)))(desugarableListRestExpr.desug(dictMonadError)(dictBoundedLattice)(v._3));
      }
      if (v.tag === "ListEnum") {
        return Apply0.apply(Functor0.map(Expr.App)(Functor0.map(Expr.App(Expr.$Expr("Var", "range")))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._1))))(Functor0.map(f => f(Expr.$Expr(
          "Int",
          top,
          1
        )))(Functor0.map(Expr.App)(Functor0.map(Expr.App(Expr.$Expr("Op", "+")))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2)))));
      }
      if (v.tag === "ListComp") {
        if (v._3.tag === "Cons" && v._3._1.tag === "ListCompGen") {
          return listCompFwd(dictMonadError)(dictBoundedLattice)(Data$dTuple.$Tuple(
            v._1,
            Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", $Qualifier("ListCompGen", v._3._1._1, v._3._1._2), v._3._2), v._2)
          ));
        }
        return listCompFwd(dictMonadError)(dictBoundedLattice)(Data$dTuple.$Tuple(v._1, Data$dTuple.$Tuple(v._3, v._2)));
      }
      if (v.tag === "Let") { return varDefsFwd(dictMonadError)(dictBoundedLattice)(Data$dTuple.$Tuple(v._1, v._2)); }
      if (v.tag === "LetRec") {
        return Apply0.apply(Functor0.map(Expr.LetRec)(recDefsFwd(dictMonadError)(dictBoundedLattice)(v._1)))(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(v._2));
      }
      if (v.tag === "DocExpr") {
        const $0 = v._2;
        return Bind1.bind(exprFwd(dictBoundedLattice)(dictMonadError)(JoinSemilattice0)(v._1))(e => Bind1.bind(exprFwd(dictBoundedLattice)(dictMonadError)(JoinSemilattice0)($0))(e$p => Applicative0.pure(Expr.$Expr(
          "DocExpr",
          e,
          e$p
        ))));
      }
      $runtime.fail();
    };
  };
};
const exprBwd = dictBoundedJoinSemilattice => v => v1 => {
  const $0 = (e, q, qs, s) => {
    const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)(Data$dTuple.$Tuple(Data$dList$dTypes.$List("Cons", q, qs), s));
    return $Expr("ListComp", v2._1, v2._2._2, v2._2._1);
  };
  const $1 = (e, qs, s) => {
    const v2 = listCompBwd(dictBoundedJoinSemilattice)(e)(Data$dTuple.$Tuple(qs, s));
    return $Expr("ListComp", v2._1, v2._2._2, v2._2._1);
  };
  if (v.tag === "Var") {
    if (v1.tag === "Var") { return $Expr("Var", v1._1); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Op") {
    if (v1.tag === "Op") { return $Expr("Op", v1._1); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Int") {
    if (v1.tag === "Int") { return $Expr("Int", v._1, v1._2); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Float") {
    if (v1.tag === "Float") { return $Expr("Float", v._1, v1._2); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Str") {
    if (v1.tag === "Str") { return $Expr("Str", v._1, v1._2); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Constr") {
    if (v1.tag === "Constr") {
      return $Expr(
        "Constr",
        v._1,
        v1._2,
        Data$dList$dTypes.listMap((() => {
          const $2 = exprBwd(dictBoundedJoinSemilattice);
          return v$1 => $2(v$1._1)(v$1._2);
        })())((() => {
          const go = go$a0$copy => go$a1$copy => go$a2$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v1$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                go$a0 = v$1._2;
                go$a1 = v1$1._2;
                go$a2 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v$1._1, v1$1._1), v2);
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v$1 = go$1$a0, v1$1 = go$1$a1;
              if (v1$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v$1;
                continue;
              }
              if (v1$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$1$a1 = v1$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(Data$dList$dTypes.Nil)(go(v._3)(v1._3)(Data$dList$dTypes.Nil));
        })())
      );
    }
    if (v._3.tag === "Cons") {
      if (v._3._2.tag === "Nil") {
        if (v1.tag === "Paragraph") {
          if (v._2 === "Paragraph") { return $Expr("Paragraph", paragraphElemsBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._1)); }
          return Effect$dException.throwException(Effect$dException.error("absurd"))();
        }
        if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
        return Effect$dException.throwException(Effect$dException.error("absurd"))();
      }
      if (v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v1.tag === "ListNonEmpty") {
        return $Expr("ListNonEmpty", v._1, exprBwd(dictBoundedJoinSemilattice)(v._3._1)(v1._2), desugarableListRestExpr.desugBwd(dictBoundedJoinSemilattice)(v._3._2._1)(v1._3));
      }
      if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
      return Effect$dException.throwException(Effect$dException.error("absurd"))();
    }
    if (v._3.tag === "Nil" && v1.tag === "ListEmpty") { return $Expr("ListEmpty", v._1); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Dictionary") {
    if (v1.tag === "Dictionary") {
      return $Expr(
        "Dictionary",
        v._1,
        (() => {
          const go = go$a0$copy => go$a1$copy => go$a2$copy => {
            let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$a2 = go$a2$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0, v1$1 = go$a1, v2 = go$a2;
              if (v$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v1$1.tag === "Nil") {
                go$c = false;
                go$r = v2;
                continue;
              }
              if (v$1.tag === "Cons" && v1$1.tag === "Cons") {
                go$a0 = v$1._2;
                go$a1 = v1$1._2;
                go$a2 = Data$dList$dTypes.$List(
                  "Cons",
                  Data$dTuple.$Tuple(
                    desugarableDictEntryExpr.desugBwd(dictBoundedJoinSemilattice)(v$1._1._1)(v1$1._1._1),
                    exprBwd(dictBoundedJoinSemilattice)(v$1._1._2)(v1$1._1._2)
                  ),
                  v2
                );
                continue;
              }
              $runtime.fail();
            }
            return go$r;
          };
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v$1 = go$1$a0, v1$1 = go$1$a1;
              if (v1$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v$1;
                continue;
              }
              if (v1$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$1$a1 = v1$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          return go$1(Data$dList$dTypes.Nil)(go(v._2)(v1._2)(Data$dList$dTypes.Nil));
        })()
      );
    }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Matrix") {
    if (v1.tag === "Matrix") {
      return $Expr("Matrix", v._1, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2), Data$dTuple.$Tuple(v1._3._1, v1._3._2), exprBwd(dictBoundedJoinSemilattice)(v._4)(v1._4));
    }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "Lambda") {
    if (v1.tag === "Lambda") { return $Expr("Lambda", toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", v._2))(toClausesStateFwd(v1._1)))); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "DProject") {
    if (v1.tag === "Project") { return $Expr("Project", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), v1._2); }
    if (v1.tag === "DProject") { return $Expr("DProject", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)); }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "App") {
    if (v1.tag === "App") { return $Expr("App", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)); }
    if (v._1.tag === "App") {
      if (v._1._1.tag === "Op") {
        if (v1.tag === "BinaryApp") { return $Expr("BinaryApp", exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1), v1._2, exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._3)); }
        if (v1.tag === "ListComp") {
          if (v1._3.tag === "Cons" && v1._3._1.tag === "ListCompGen") { return $0(v, v1._3._1, v1._3._2, v1._2); }
          return $1(v, v1._3, v1._2);
        }
        return Effect$dException.throwException(Effect$dException.error("absurd"))();
      }
      if (v._1._1.tag === "Var" && v._1._1._1 === "range" && v1.tag === "ListEnum") {
        if (v._2.tag === "App" && v._2._1.tag === "App") {
          return $Expr("ListEnum", exprBwd(dictBoundedJoinSemilattice)(v._1._2)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2._1._2)(v1._2));
        }
        return Effect$dException.throwException(Effect$dException.error("absurd"))();
      }
      if (v1.tag === "ListComp") {
        if (v1._3.tag === "Cons" && v1._3._1.tag === "ListCompGen") { return $0(v, v1._3._1, v1._3._2, v1._2); }
        return $1(v, v1._3, v1._2);
      }
      return Effect$dException.throwException(Effect$dException.error("absurd"))();
    }
    if (v._1.tag === "Lambda") {
      if (v1.tag === "MatchAs") {
        return $Expr(
          "MatchAs",
          exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._1),
          (() => {
            const $2 = toClausesStateBwd(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", v._1._2))(toClausesStateFwd(Data$dNonEmpty.$NonEmpty(
              Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(v1._2._1._1, Data$dList$dTypes.Nil), v1._2._1._2),
              Data$dList$dTypes.listMap(x => Data$dTuple.$Tuple(Data$dNonEmpty.$NonEmpty(x._1, Data$dList$dTypes.Nil), x._2))(v1._2._2)
            ))));
            return Data$dNonEmpty.$NonEmpty(Data$dTuple.$Tuple($2._1._1._1, $2._1._2), Data$dList$dTypes.listMap(x => Data$dTuple.$Tuple(x._1._1, x._2))($2._2));
          })()
        );
      }
      if (v._1._2.tag === "ElimConstr" && v1.tag === "IfElse") {
        const v2 = ifElseBwd(dictBoundedJoinSemilattice)(v)(Data$dTuple.$Tuple(v1._1, v1._2));
        return $Expr("IfElse", v2._1, v2._2);
      }
    }
    if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v1.tag === "ListComp") { return $1(v, v1._3, v1._2); }
  if (v.tag === "Let") {
    if (v1.tag === "Let") {
      const v2 = varDefsBwd(dictBoundedJoinSemilattice)(Expr.$Expr("Let", v._1, v._2))(Data$dTuple.$Tuple(v1._1, v1._2));
      return $Expr("Let", v2._1, v2._2);
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "LetRec") {
    if (v1.tag === "LetRec") { return $Expr("LetRec", recDefsBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)); }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (v.tag === "DocExpr" && v1.tag === "DocExpr") { return $Expr("DocExpr", exprBwd(dictBoundedJoinSemilattice)(v._1)(v1._1), exprBwd(dictBoundedJoinSemilattice)(v._2)(v1._2)); }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const clausesStateFwd = dictBoundedLattice => {
  const top = dictBoundedLattice.BoundedMeetSemilattice1().top;
  return dictMonadError => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    const Bind1 = Monad0.Bind1();
    const $0 = Bind1.Apply0().Functor0();
    const popArgFwd2 = popArgFwd(dictMonadError);
    const popVarFwd2 = popVarFwd(dictMonadError);
    const popRecordFwd2 = popRecordFwd(dictMonadError);
    const popListVarFwd2 = popListVarFwd(dictMonadError);
    const popConstrFwd2 = popConstrFwd(dictMonadError);
    const Applicative0 = Monad0.Applicative0();
    const sequence1 = Data$dList$dTypes.traversableList.traverse(Applicative0)(Data$dList$dTypes.identity);
    const rtraverse1 = Data$dBitraversable.bitraversableTuple.bitraverse(Applicative0)(Applicative0.pure);
    return ks => {
      const $1 = p => Bind1.bind(popConstrFwd2(Util.defined(DataType.dataTypeForCtr.dataTypeFor(monadThrowExceptT)(Util.definitely("clausesStateFwd ctrFor failed for: " + showPattern(p))((() => {
        if (p.tag === "Left") {
          if (p._1.tag === "PVar") { return Data$dMaybe.Nothing; }
          if (p._1.tag === "PConstr") { return Data$dMaybe.$Maybe("Just", p._1._1); }
          if (p._1.tag === "PRecord") { return Data$dMaybe.Nothing; }
          if (p._1.tag === "PListEmpty") { return Data$dMaybe.$Maybe("Just", "Nil"); }
          if (p._1.tag === "PListNonEmpty") { return Data$dMaybe.$Maybe("Just", ":"); }
          $runtime.fail();
        }
        if (p.tag === "Right") {
          if (p._1.tag === "PListVar") { return Data$dMaybe.Nothing; }
          if (p._1.tag === "PListEnd") { return Data$dMaybe.$Maybe("Just", "Nil"); }
          if (p._1.tag === "PListNext") { return Data$dMaybe.$Maybe("Just", ":"); }
        }
        $runtime.fail();
      })()))))(ks))(kss => $0.map(x => Expr.$Cont("ContElim", Expr.$Elim("ElimConstr", fromFoldable2(x))))(sequence1(Data$dList$dTypes.listMap(rtraverse1(clausesStateFwd(dictBoundedLattice)(dictMonadError)))(kss))));
      if (ks.tag === "Nil") { return Effect$dException.throwException(Effect$dException.error("absurd"))(); }
      if (ks.tag === "Cons") {
        if (ks._1._1.tag === "Nil") {
          if (ks._1._2._1.tag === "Nil" && ks._2.tag === "Nil") {
            return $0.map(Expr.ContExpr)(exprFwd(dictBoundedLattice)(dictMonadError)(dictBoundedLattice.BoundedJoinSemilattice0().JoinSemilattice0())(ks._1._2._2));
          }
          return $0.map((() => {
            const $2 = Expr.Lambda(top);
            return x => Expr.$Cont("ContExpr", $2(x.tag === "ContElim" ? x._1 : Effect$dException.throwException(Effect$dException.error("Eliminator expected"))()));
          })())(Bind1.bind(popArgFwd2(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
        }
        if (ks._1._1.tag === "Cons") {
          if (ks._1._1._1.tag === "Left") {
            if (ks._1._1._1._1.tag === "PVar") {
              const $2 = ks._1._1._1._1._1;
              return $0.map((() => {
                const $3 = Expr.ElimVar($2);
                return x => Expr.$Cont("ContElim", $3(x));
              })())(Bind1.bind(popVarFwd2($2)(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
            }
            if (ks._1._1._1._1.tag === "PRecord") {
              const $2 = ks._1._1._1._1._1;
              return $0.map((() => {
                const $3 = Expr.ElimDict(Bind.keys($2));
                return x => Expr.$Cont("ContElim", $3(x));
              })())(Bind1.bind(popRecordFwd2(Data$dList$dTypes.listMap(Data$dTuple.fst)($2))(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
            }
            return $1(ks._1._1._1);
          }
          if (ks._1._1._1.tag === "Right" && ks._1._1._1._1.tag === "PListVar") {
            const $2 = ks._1._1._1._1._1;
            return $0.map((() => {
              const $3 = Expr.ElimVar($2);
              return x => Expr.$Cont("ContElim", $3(x));
            })())(Bind1.bind(popListVarFwd2($2)(ks))(clausesStateFwd(dictBoundedLattice)(dictMonadError)));
          }
          return $1(ks._1._1._1);
        }
      }
      $runtime.fail();
    };
  };
};
const clausesStateBwd = dictBoundedJoinSemilattice => κ0 => ks => {
  const $0 = σ => popArgBwd(clausesStateBwd(dictBoundedJoinSemilattice)(Expr.$Cont("ContElim", σ))(Util.defined(popArgFwd1(ks))));
  const $1 = (m, p) => popConstrBwd((() => {
    const go = go$a0$copy => go$a1$copy => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          const go$1 = go$1$a0$copy => go$1$a1$copy => {
            let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
            while (go$1$c) {
              const v$1 = go$1$a0, v1$1 = go$1$a1;
              if (v1$1.tag === "Nil") {
                go$1$c = false;
                go$1$r = v$1;
                continue;
              }
              if (v1$1.tag === "Cons") {
                go$1$a0 = Data$dList$dTypes.$List("Cons", v1$1._1, v$1);
                go$1$a1 = v1$1._2;
                continue;
              }
              $runtime.fail();
            }
            return go$1$r;
          };
          go$c = false;
          go$r = go$1(Data$dList$dTypes.Nil)(v);
          continue;
        }
        if (v1.tag === "Cons") {
          const $1 = clausesStateBwd(dictBoundedJoinSemilattice);
          const $2 = Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, v1._1._1, m);
          if ($2.tag === "Just") {
            go$a0 = Data$dList$dTypes.$List("Cons", Data$dTuple.$Tuple(v1._1._1, $1($2._1)(v1._1._2)), v);
            go$a1 = v1._2;
            continue;
          }
          go$a0 = v;
          go$a1 = v1._2;
          continue;
        }
        $runtime.fail();
      }
      return go$r;
    };
    return go(Data$dList$dTypes.Nil)(Util.defined(popConstrFwd1(Util.defined(DataType.dataTypeForCtr.dataTypeFor(monadThrowExceptT)(Util.definitely("absurd")((() => {
      if (p.tag === "Left") {
        if (p._1.tag === "PVar") { return Data$dMaybe.Nothing; }
        if (p._1.tag === "PConstr") { return Data$dMaybe.$Maybe("Just", p._1._1); }
        if (p._1.tag === "PRecord") { return Data$dMaybe.Nothing; }
        if (p._1.tag === "PListEmpty") { return Data$dMaybe.$Maybe("Just", "Nil"); }
        if (p._1.tag === "PListNonEmpty") { return Data$dMaybe.$Maybe("Just", ":"); }
        $runtime.fail();
      }
      if (p.tag === "Right") {
        if (p._1.tag === "PListVar") { return Data$dMaybe.Nothing; }
        if (p._1.tag === "PListEnd") { return Data$dMaybe.$Maybe("Just", "Nil"); }
        if (p._1.tag === "PListNext") { return Data$dMaybe.$Maybe("Just", ":"); }
      }
      $runtime.fail();
    })()))))(ks)));
  })())(ks);
  if (ks.tag === "Nil") { return Effect$dException.throwException(Effect$dException.error("absurd"))(); }
  if (κ0.tag === "ContExpr") {
    if (ks.tag === "Cons" && ks._1._1.tag === "Nil") {
      if (ks._1._2._1.tag === "Nil" && ks._2.tag === "Nil") {
        return Data$dList$dTypes.$List(
          "Cons",
          Data$dTuple.$Tuple(Data$dList$dTypes.Nil, Data$dTuple.$Tuple(Data$dList$dTypes.Nil, exprBwd(dictBoundedJoinSemilattice)(κ0._1)(ks._1._2._2))),
          Data$dList$dTypes.Nil
        );
      }
      if (κ0._1.tag === "Lambda") { return $0(κ0._1._2); }
    }
    return Effect$dException.throwException(Effect$dException.error("absurd"))();
  }
  if (κ0.tag === "ContElim") {
    if (ks.tag === "Cons" && ks._1._1.tag === "Cons") {
      if (ks._1._1._1.tag === "Left") {
        if (ks._1._1._1._1.tag === "PVar") {
          if (κ0._1.tag === "ElimVar") { return popVarBwd(κ0._1._1)(clausesStateBwd(dictBoundedJoinSemilattice)(κ0._1._2)(Util.defined(popVarFwd1(κ0._1._1)(ks)))); }
          if (κ0._1.tag === "ElimConstr") { return $1(κ0._1._1, ks._1._1._1); }
          return Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))();
        }
        if (ks._1._1._1._1.tag === "PRecord" && κ0._1.tag === "ElimDict") {
          const $2 = ks._1._1._1._1._1;
          return popRecordBwd(Data$dList$dTypes.listMap(Data$dTuple.fst)($2))(clausesStateBwd(dictBoundedJoinSemilattice)(κ0._1._2)(Util.defined(popRecordFwd1(Data$dList$dTypes.listMap(Data$dTuple.fst)($2))(ks))));
        }
        if (κ0._1.tag === "ElimConstr") { return $1(κ0._1._1, ks._1._1._1); }
        return Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))();
      }
      if (ks._1._1._1.tag === "Right" && ks._1._1._1._1.tag === "PListVar" && κ0._1.tag === "ElimVar") {
        return popListVarBwd(κ0._1._1)(clausesStateBwd(dictBoundedJoinSemilattice)(κ0._1._2)(Util.defined(popListVarFwd1(κ0._1._1)(ks))));
      }
      if (κ0._1.tag === "ElimConstr") { return $1(κ0._1._1, ks._1._1._1); }
    }
    return Effect$dException.throwException(Effect$dException.error(Effect$dException.throwException(Effect$dException.error("Shape mismatch"))()))();
  }
  $runtime.fail();
};
const paragraphBwd = dictBoundedJoinSemilattice => v => v1 => {
  if (v.tag === "Constr" && v._3.tag === "Cons" && v._3._2.tag === "Nil" && v._2 === "Paragraph") { return paragraphElemsBwd(dictBoundedJoinSemilattice)(v._3._1)(v1); }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const moduleFwd = dictMonadError => {
  const Monad0 = dictMonadError.MonadThrow0().Monad0();
  const $0 = Monad0.Bind1().Apply0().Functor0();
  const varDefFwd1 = varDefFwd(dictMonadError);
  const recDefsFwd1 = recDefsFwd(dictMonadError);
  const traverse2 = Data$dList$dTypes.traversableList.traverse(Monad0.Applicative0());
  return dictBoundedLattice => {
    const varDefFwd2 = varDefFwd1(dictBoundedLattice);
    const recDefsFwd2 = recDefsFwd1(dictBoundedLattice);
    return v => $0.map(Expr.Module)(traverse2(v1 => {
      if (v1.tag === "Left") { return $0.map(Data$dEither.Left)(varDefFwd2(v1._1)); }
      if (v1.tag === "Right") { return $0.map(Data$dEither.Right)(recDefsFwd2(v1._1)); }
      $runtime.fail();
    })(Data$dList$dTypes.bindList.bind(Data$dList$dTypes.listMap(v1 => {
      if (v1.tag === "Left") { return Data$dList$dTypes.listMap(Data$dEither.Left)(Data$dList$dTypes.$List("Cons", v1._1._1, v1._1._2)); }
      if (v1.tag === "Right") { return Data$dList$dTypes.$List("Cons", Data$dEither.$Either("Right", v1._1), Data$dList$dTypes.Nil); }
      $runtime.fail();
    })(v._1))(Control$dBind.identity)));
  };
};
const desugarModuleFwd = dictMonadError => moduleFwd(dictMonadError);
export {
  $DictEntry,
  $Expr,
  $ListRest,
  
  $Module,
  $ParagraphElem,
  $Pattern,
  $Qualifier,
  $VarDef,
  
  
  
  
  
  
  
  
  Constr,
  
  
  
  
  
  
  
  
  
  
  
  Float,
  
  
  
  Int,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  Next,
  
  
  
  
  PListEmpty,
  PListEnd,
  PListNext,
  
  
  
  
  
  
  
  
  
  
  
  
  Str,
  
  
  
  
  
  
  
  VarKey,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  exprFwd,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  moduleFwd,
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
};

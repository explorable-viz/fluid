import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as Data$dList from "../Data.List/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Util from "../Util/index.js";
import * as Util$dMap from "../Util.Map/index.js";
import * as Val from "../Val/index.js";
const select$p = dictNeg => x => Data$dTuple.$Tuple(dictNeg.neg(x), App$dUtil.Persistent);
const select = dictNeg => dictFunctor => b => Data$dTuple.$Tuple(
  dictFunctor.map(v => {
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: dictNeg.neg(v._1.persistent), transient: v._1.transient}); }
    $runtime.fail();
  })(b),
  App$dUtil.Persistent
);
const persist = δα => v => Data$dTuple.$Tuple(
  (() => {
    if (v.tag === "Inert") { return App$dUtil.Inert; }
    if (v.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1.persistent)._1, transient: v._1.transient}); }
    $runtime.fail();
  })(),
  App$dUtil.Persistent
);
const matrixElement = v => v1 => v2 => v3 => {
  if (v3._3.tag === "Matrix") {
    const $0 = v2(Val.matrixGet(v)(v1)(v3._3._1));
    return Data$dTuple.$Tuple(
      (() => {
        const $1 = $0._1;
        return Val.$Val(v3._1, v3._2, Val.$BaseVal("Matrix", Val.matrixPut(v)(v1)(v$1 => $1)(v3._3._1)));
      })(),
      $0._2
    );
  }
  return Effect$dException.throwException(Effect$dException.error("absurd"))();
};
const listElement = n => δv => v => {
  if (v._3.tag === "Constr" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Cons" && v._3._2._2._2.tag === "Nil") {
    if (n === 0 && v._3._1 === ":") {
      const $0 = δv(v._3._2._1);
      return Data$dTuple.$Tuple(
        Val.$Val(v._1, v._2, Val.$BaseVal("Constr", v._3._1, Data$dList$dTypes.$List("Cons", $0._1, Data$dList$dTypes.$List("Cons", v._3._2._2._1, Data$dList$dTypes.Nil)))),
        $0._2
      );
    }
    if (v._3._1 === ":") {
      const $0 = listElement(n - 1 | 0)(δv)(v._3._2._2._1);
      return Data$dTuple.$Tuple(
        Val.$Val(v._1, v._2, Val.$BaseVal("Constr", v._3._1, Data$dList$dTypes.$List("Cons", v._3._2._1, Data$dList$dTypes.$List("Cons", $0._1, Data$dList$dTypes.Nil)))),
        $0._2
      );
    }
  }
  $runtime.fail();
};
const listCell = n => δα => v => {
  if (v._3.tag === "Constr") {
    if (v._3._2.tag === "Nil") {
      if (n === 0 && v._3._1 === "Nil") {
        return Data$dTuple.$Tuple(
          Val.$Val(
            (() => {
              if (v._1.tag === "Inert") { return App$dUtil.Inert; }
              if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1._1.persistent)._1, transient: v._1._1.transient}); }
              $runtime.fail();
            })(),
            v._2,
            Val.$BaseVal("Constr", v._3._1, Data$dList$dTypes.Nil)
          ),
          App$dUtil.Persistent
        );
      }
      $runtime.fail();
    }
    if (v._3._2.tag === "Cons" && v._3._2._2.tag === "Cons" && v._3._2._2._2.tag === "Nil" && v._3._1 === ":") {
      const $0 = v._3._2._2._1;
      if (n === 0) {
        return Data$dTuple.$Tuple(
          Val.$Val(
            (() => {
              if (v._1.tag === "Inert") { return App$dUtil.Inert; }
              if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1._1.persistent)._1, transient: v._1._1.transient}); }
              $runtime.fail();
            })(),
            v._2,
            Val.$BaseVal("Constr", v._3._1, Data$dList$dTypes.$List("Cons", v._3._2._1, Data$dList$dTypes.$List("Cons", $0, Data$dList$dTypes.Nil)))
          ),
          App$dUtil.Persistent
        );
      }
      const $1 = listCell(n - 1 | 0)(δα)($0);
      return Data$dTuple.$Tuple(
        Val.$Val(v._1, v._2, Val.$BaseVal("Constr", v._3._1, Data$dList$dTypes.$List("Cons", v._3._2._1, Data$dList$dTypes.$List("Cons", $1._1, Data$dList$dTypes.Nil)))),
        $1._2
      );
    }
  }
  $runtime.fail();
};
const envVal = x => δv => γ => Util.assertWith("")(Object.hasOwn(γ, x))((() => {
  const $0 = δv(Util$dMap.get(Data$dShow.showString)(Val.mapEnvStringVal)(x)(γ));
  return Data$dTuple.$Tuple(
    (() => {
      const $1 = $0._1;
      return Util$dMap.update(Data$dShow.showString)(Val.mapEnvStringVal)(v => $1)(x)(γ);
    })(),
    $0._2
  );
})());
const dictVal = s => δv => v => {
  if (v._3.tag === "Dictionary") {
    const $0 = δv(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(s)(v._3._1)._2);
    return Data$dTuple.$Tuple(
      (() => {
        const $1 = $0._1;
        return Val.$Val(v._1, v._2, Val.$BaseVal("Dictionary", Util$dMap.update(Data$dShow.showString)(Dict.mapDictString)(m => Data$dTuple.$Tuple(m._1, $1))(s)(v._3._1)));
      })(),
      $0._2
    );
  }
  $runtime.fail();
};
const nthSegment = n => x => listElement(n)(dictVal("z")(x));
const scatterPoint = i => x => dictVal("points")(listElement(i)(x));
const dictKey = s => δα => v => {
  if (v._3.tag === "Dictionary") {
    const v1 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)(s)(v._3._1);
    const $0 = v1._2;
    const $1 = (() => {
      if (v1._1.tag === "Inert") { return App$dUtil.Inert; }
      if (v1._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v1._1._1.persistent)._1, transient: v1._1._1.transient}); }
      $runtime.fail();
    })();
    return Data$dTuple.$Tuple(
      Val.$Val(
        v._1,
        v._2,
        Val.$BaseVal(
          "Dictionary",
          Foreign$dObject.mutate($2 => () => {
            $2[s] = Data$dTuple.$Tuple($1, $0);
            return $2;
          })(v._3._1)
        )
      ),
      App$dUtil.Persistent
    );
  }
  $runtime.fail();
};
const dict = δα => v => {
  if (v._3.tag === "Dictionary") {
    return Data$dTuple.$Tuple(
      Val.$Val(
        (() => {
          if (v._1.tag === "Inert") { return App$dUtil.Inert; }
          if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1._1.persistent)._1, transient: v._1._1.transient}); }
          $runtime.fail();
        })(),
        v._2,
        Val.$BaseVal("Dictionary", v._3._1)
      ),
      App$dUtil.Persistent
    );
  }
  $runtime.fail();
};
const constrArg = c => n => δv => v => {
  if (v._3.tag === "Constr" && c === v._3._1) {
    const $0 = Util.definitely("constrArg out of bounds")((() => {
      const $0 = Data$dList.index(v._3._2)(n);
      if ($0.tag === "Just") { return Data$dMaybe.$Maybe("Just", δv($0._1)); }
      return Data$dMaybe.Nothing;
    })());
    return Data$dTuple.$Tuple(
      Val.$Val(
        v._1,
        v._2,
        Val.$BaseVal(
          "Constr",
          v._3._1,
          (() => {
            const $1 = Data$dList.updateAt(n)($0._1)(v._3._2);
            if ($1.tag === "Just") { return $1._1; }
            $runtime.fail();
          })()
        )
      ),
      $0._2
    );
  }
  $runtime.fail();
};
const fst = /* #__PURE__ */ constrArg("Pair")(0);
const lineChart = /* #__PURE__ */ constrArg("LineChart")(0);
const linePoint = i => x => constrArg("LinePlot")(0)(dictVal("points")(listElement(i)(x)));
const multiView = /* #__PURE__ */ constrArg("MultiView")(0);
const multiViewEntry = n => x => constrArg("MultiView")(0)(listElement(n)(x));
const paragraph = /* #__PURE__ */ constrArg("Paragraph")(0);
const scatterPlot = /* #__PURE__ */ constrArg("ScatterPlot")(0);
const snd = /* #__PURE__ */ constrArg("Pair")(1);
const constr = c$p => δα => v => {
  if (v._3.tag === "Constr" && v._3._1 === c$p) {
    return Data$dTuple.$Tuple(
      Val.$Val(
        (() => {
          if (v._1.tag === "Inert") { return App$dUtil.Inert; }
          if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1._1.persistent)._1, transient: v._1._1.transient}); }
          $runtime.fail();
        })(),
        v._2,
        Val.$BaseVal("Constr", v._3._1, v._3._2)
      ),
      App$dUtil.Persistent
    );
  }
  $runtime.fail();
};
const some = δα => v => {
  if (v._3.tag === "Constr" && v._3._1 === "Some") {
    return Data$dTuple.$Tuple(
      Val.$Val(
        (() => {
          if (v._1.tag === "Inert") { return App$dUtil.Inert; }
          if (v._1.tag === "Reactive") { return App$dUtil.$SelState("Reactive", {persistent: δα(v._1._1.persistent)._1, transient: v._1._1.transient}); }
          $runtime.fail();
        })(),
        v._2,
        Val.$BaseVal("Constr", v._3._1, v._3._2)
      ),
      App$dUtil.Persistent
    );
  }
  $runtime.fail();
};
const composeSetSel = f => g => x => g(f(x)._1);
const barSegment = i => j => x => dictVal("stackedBars")(listElement(i)(dictVal("segments")(nthSegment(j)(x))));
const barChart = /* #__PURE__ */ constrArg("BarChart")(0);
export {
  
  
  
  
  constrArg,
  
  
  dictVal,
  envVal,
  
  
  linePoint,
  
  listElement,
  matrixElement,
  
  
  nthSegment,
  
  
  
  scatterPoint,
  
  
  
  
};

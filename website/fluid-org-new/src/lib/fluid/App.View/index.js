import * as $runtime from "../runtime.js";
import * as App$dUtil from "../App.Util/index.js";
import * as App$dView$dBarChart from "../App.View.BarChart/index.js";
import * as App$dView$dDocView from "../App.View.DocView/index.js";
import * as App$dView$dLineChart from "../App.View.LineChart/index.js";
import * as App$dView$dMatrixView from "../App.View.MatrixView/index.js";
import * as App$dView$dMultiView from "../App.View.MultiView/index.js";
import * as App$dView$dParagraph from "../App.View.Paragraph/index.js";
import * as App$dView$dScatterPlot from "../App.View.ScatterPlot/index.js";
import * as App$dView$dTableView from "../App.View.TableView/index.js";
import * as App$dView$dText from "../App.View.Text/index.js";
import * as App$dView$dUtil from "../App.View.Util/index.js";
import * as App$dView$dUtil$dAxes from "../App.View.Util.Axes/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Dict from "../Dict/index.js";
import * as Effect$dException from "../Effect.Exception/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Link from "../Link/index.js";
import * as Primitive from "../Primitive/index.js";
import * as Util$dMap from "../Util.Map/index.js";
const pack = x => v => v(App$dView$dText.viewableTextUnit)(x);
const pack1 = x => v => v(App$dView$dBarChart.viewableBarChartUnit)(x);
const pack2 = x => v => v(App$dView$dLineChart.viewableLineChartUnit)(x);
const pack3 = x => v => v(App$dView$dScatterPlot.viewableScatterPlotUnit)(x);
const pack4 = x => v => v(App$dView$dMultiView.viewableMultiViewUnit)(x);
const pack5 = x => v => v(App$dView$dParagraph.viewableParagraphUnit)(x);
const pack6 = x => v => v(Link.viewableLinkUnit)(x);
const identity = x => x;
const pack7 = x => v => v(App$dView$dTableView.viewableTableViewUnit)(x);
const pack8 = x => v => v(App$dView$dMatrixView.viewableMatrixViewUnit)(x);
const pack9 = x => v => v(App$dView$dUtil.viewableDictView$x215ViewUnit)(x);
const pack10 = x => v => v(App$dView$dDocView.viewableDocViewUnit)(x);
const reflectValSelStates𝕊Text = {
  from: () => v => {
    if (v._3.tag === "Str") { return Data$dTuple.$Tuple(v._3._1, v._1); }
    return Primitive.typeError(v._3)("Text expects string");
  }
};
const reflectValSelStates𝕊Link = {
  from: () => v => {
    if (v._3.tag === "Constr" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Cons" && v._3._2._2._1._3.tag === "Str" && v._3._2._2._2.tag === "Nil" && v._3._1 === "Link") {
      return Link.$Link(v._3._2._1, Data$dTuple.$Tuple(v._3._2._2._1._3._1, v._3._2._2._1._1));
    }
    return Primitive.typeError(v._3)("Link expects string as second argument");
  }
};
const reflectValDict$x215Val = {
  from: () => v => {
    if (v._3.tag === "Dictionary") { return v._3._1; }
    $runtime.fail();
  }
};
const reflectDictSelStates𝕊$x215Val = {
  from: () => r => (
    {
      x: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("x")(r);
        return Data$dTuple.$Tuple(App$dView$dUtil$dAxes.orientation.unpack($0._2._3), $0._2._1);
      })(),
      y: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("y")(r);
        return Data$dTuple.$Tuple(App$dView$dUtil$dAxes.orientation.unpack($0._2._3), $0._2._1);
      })()
    }
  )
};
const reflectDictSelStates𝕊$x215Val1 = {
  from: () => r => (
    {
      x: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("x")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      y: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("y")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })()
    }
  )
};
const reflectDictSelStates𝕊$x215Val2 = {from: () => r => ({x: App$dUtil.get_intOrNumber("x")(r), y: App$dUtil.get_intOrNumber("y")(r)})};
const reflectDictSelStates𝕊$x215Val3 = {
  from: () => r => (
    {
      y: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("y")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      z: App$dUtil.get_intOrNumber("z")(r)
    }
  )
};
const reflectDictSelStates𝕊$x215Val4 = {
  from: () => r => (
    {
      width: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("width")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Int" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Int"), $0._2._1);
      })(),
      height: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("height")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Int" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Int"), $0._2._1);
      })()
    }
  )
};
const from = dict => dict.from;
const reflectValArrayVal = {
  from: () => v => {
    if (v._3.tag === "Constr") {
      if (v._3._2.tag === "Nil") {
        if (v._3._1 === "Nil") { return []; }
        $runtime.fail();
      }
      if (v._3._2.tag === "Cons" && v._3._2._2.tag === "Cons" && v._3._2._2._2.tag === "Nil" && v._3._1 === ":") {
        return [v._3._2._1, ...reflectValArrayVal.from()(v._3._2._2._1)];
      }
    }
    $runtime.fail();
  }
};
const reflectDictSelStates𝕊$x215Val5 = {
  from: () => r => (
    {
      name: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("name")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      points: Data$dFunctor.arrayMap(App$dUtil.dict(reflectDictSelStates𝕊$x215Val2.from()))(reflectValArrayVal.from()(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("points")(r)._2))
    }
  )
};
const reflectDictSelStates𝕊$x215Val6 = {
  from: () => r => (
    {
      caption: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("caption")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      points: Data$dFunctor.arrayMap(App$dUtil.dict(reflectDictSelStates𝕊$x215Val2.from()))(reflectValArrayVal.from()(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("points")(r)._2)),
      labels: App$dUtil.dict(reflectDictSelStates𝕊$x215Val1.from())(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("labels")(r)._2)
    }
  )
};
const reflectValNonEmptyArrayVa = {
  from: () => v => {
    if (v._3.tag === "Constr") {
      if (v._3._2.tag === "Nil") {
        if (v._3._1 === "Nil") { return Effect$dException.throwException(Effect$dException.error("expected non-empty list"))(); }
        $runtime.fail();
      }
      if (v._3._2.tag === "Cons" && v._3._2._2.tag === "Cons" && v._3._2._2._2.tag === "Nil" && v._3._1 === ":") {
        return [v._3._2._1, ...reflectValArrayVal.from()(v._3._2._2._1)];
      }
    }
    $runtime.fail();
  }
};
const reflectDictSelStates𝕊$x215Val7 = {
  from: () => r => (
    {
      x: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("x")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      segments: Data$dFunctor.arrayMap(App$dUtil.dict(reflectDictSelStates𝕊$x215Val3.from()))(reflectValNonEmptyArrayVa.from()(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("segments")(r)._2))
    }
  )
};
const reflectDictSelStates𝕊$x215Val8 = {
  from: () => r => (
    {
      caption: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("caption")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      stackedBars: Data$dFunctor.arrayMap(App$dUtil.dict(reflectDictSelStates𝕊$x215Val7.from()))(reflectValNonEmptyArrayVa.from()(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("stackedBars")(r)._2)),
      size: App$dUtil.dict(reflectDictSelStates𝕊$x215Val4.from())(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("size")(r)._2),
      tickLabels: App$dUtil.dict(reflectDictSelStates𝕊$x215Val.from())(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("tickLabels")(r)._2),
      legend: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("legend")(r);
        return Data$dTuple.$Tuple(Primitive.boolean.unpack($0._2._3), $0._2._1);
      })()
    }
  )
};
const reflectValSelStates𝕊LineP = {
  from: () => v => {
    if (v._3.tag === "Constr" && v._3._2.tag === "Cons" && v._3._2._2.tag === "Nil" && v._3._1 === "LinePlot") {
      return App$dUtil.dict(reflectDictSelStates𝕊$x215Val5.from())(v._3._2._1);
    }
    $runtime.fail();
  }
};
const reflectDictSelStates𝕊$x215Val9 = {
  from: () => r => (
    {
      size: App$dUtil.dict(reflectDictSelStates𝕊$x215Val4.from())(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("size")(r)._2),
      tickLabels: App$dUtil.dict(reflectDictSelStates𝕊$x215Val.from())(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("tickLabels")(r)._2),
      caption: (() => {
        const $0 = Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("caption")(r);
        return Data$dTuple.$Tuple($0._2._3.tag === "Str" ? $0._2._3._1 : Primitive.typeError($0._2._3)("Str"), $0._2._1);
      })(),
      plots: Data$dFunctor.arrayMap(reflectValSelStates𝕊LineP.from())(reflectValArrayVal.from()(Util$dMap.get(Data$dShow.showString)(Dict.mapDictString)("plots")(r)._2))
    }
  )
};
const view = () => options => title => v => {
  const $0 = c => {
    const vs = reflectValArrayVal.from()(v);
    if (
      (() => {
        const v2 = Data$dArray.unconsImpl(v$1 => Data$dMaybe.Nothing, x => xs => Data$dMaybe.$Maybe("Just", {head: x, tail: xs}), vs);
        if (v2.tag === "Just") { return v2._1.head._3.tag === "Dictionary"; }
        if (v2.tag === "Nothing") { return true; }
        $runtime.fail();
      })()
    ) {
      const records = Data$dFunctor.arrayMap(App$dUtil.dict(identity))(vs);
      const colNames = App$dView$dTableView.headers(records);
      return pack7({
        title,
        rowFilter: (() => {
          if (options.rowFilter.tag === "Nothing") { return App$dView$dUtil.Interactive; }
          if (options.rowFilter.tag === "Just") { return options.rowFilter._1; }
          $runtime.fail();
        })(),
        colNames,
        rows: Data$dFunctor.arrayMap(Data$dFunctor.arrayMap(Data$dTuple.snd))(App$dView$dTableView.arrayDictToArray2(colNames)(records))
      });
    }
    return pack4(App$dView$dMultiView.$MultiView(Data$dFunctor.arrayMap(view()(options)(""))(vs)));
  };
  if (v._3.tag === "Int") { return pack(Data$dTuple.$Tuple(Data$dShow.showIntImpl(v._3._1), v._1)); }
  if (v._3.tag === "Float") { return pack(Data$dTuple.$Tuple(Data$dShow.showNumberImpl(v._3._1), v._1)); }
  if (v._3.tag === "Str") { return pack(Data$dTuple.$Tuple(v._3._1, v._1)); }
  if (v._3.tag === "Constr") {
    if (v._3._2.tag === "Cons") {
      if (v._3._2._2.tag === "Nil") {
        if (v._3._1 === "Text") { return pack(reflectValSelStates𝕊Text.from()(v._3._2._1)); }
        if (v._3._1 === "BarChart") { return pack1(App$dUtil.dict(reflectDictSelStates𝕊$x215Val8.from())(v._3._2._1)); }
        if (v._3._1 === "LineChart") { return pack2(App$dUtil.dict(reflectDictSelStates𝕊$x215Val9.from())(v._3._2._1)); }
        if (v._3._1 === "ScatterPlot") { return pack3(App$dUtil.dict(reflectDictSelStates𝕊$x215Val6.from())(v._3._2._1)); }
        if (v._3._1 === "MultiView") { return pack4(App$dView$dMultiView.$MultiView(Data$dFunctor.arrayMap(view()(options)(""))(reflectValArrayVal.from()(v._3._2._1)))); }
        if (v._3._1 === "Paragraph") { return pack5(App$dView$dParagraph.$Paragraph(Data$dFunctor.arrayMap(view()(options)(""))(reflectValArrayVal.from()(v._3._2._1)))); }
        if (v._3._1 === "Nil" || v._3._1 === ":") { return $0(v._3._1); }
        return Primitive.typeError(v._3)("Viewable");
      }
      if (v._3._2._2.tag === "Cons" && v._3._2._2._2.tag === "Nil" && v._3._1 === "Link") { return pack6(reflectValSelStates𝕊Link.from()(v)); }
    }
    if (v._3._1 === "Nil" || v._3._1 === ":") { return $0(v._3._1); }
    return Primitive.typeError(v._3)("Viewable");
  }
  if (v._3.tag === "Matrix") { return pack8({title, matrix: App$dView$dMatrixView.matrixRep(v._3._1)}); }
  if (v._3.tag === "Dictionary") {
    return pack9(Foreign$dObject._mapWithKey(v._3._1, k => v2 => Data$dTuple.$Tuple(pack(Data$dTuple.$Tuple(k, v2._1)), view()(options)(k)(v2._2))));
  }
  return Primitive.typeError(v._3)("Viewable");
};
const view$p = () => options => title => v => pack10({
  doc: v._2.tag === "Just"
    ? Data$dMaybe.$Maybe(
        "Just",
        (() => {
          if (v._2._1._3.tag === "Constr" && v._2._1._3._2.tag === "Cons" && v._2._1._3._2._2.tag === "Nil" && v._2._1._3._1 === "Paragraph") {
            return App$dView$dParagraph.$Paragraph(Data$dFunctor.arrayMap(view()(options)(""))(reflectValArrayVal.from()(v._2._1._3._2._1)));
          }
          $runtime.fail();
        })()
      )
    : Data$dMaybe.Nothing,
  view: view()(options)(title)(v)
});
export {
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  view$p
};

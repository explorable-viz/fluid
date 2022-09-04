(() => {
  // node_modules/d3-array/src/ascending.js
  function ascending_default(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-array/src/bisector.js
  function bisector_default(f) {
    let delta = f;
    let compare6 = f;
    if (f.length === 1) {
      delta = (d, x2) => f(d) - x2;
      compare6 = ascendingComparator(f);
    }
    function left3(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      while (lo < hi) {
        const mid = lo + hi >>> 1;
        if (compare6(a[mid], x2) < 0)
          lo = mid + 1;
        else
          hi = mid;
      }
      return lo;
    }
    function right3(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      while (lo < hi) {
        const mid = lo + hi >>> 1;
        if (compare6(a[mid], x2) > 0)
          hi = mid;
        else
          lo = mid + 1;
      }
      return lo;
    }
    function center2(a, x2, lo, hi) {
      if (lo == null)
        lo = 0;
      if (hi == null)
        hi = a.length;
      const i = left3(a, x2, lo, hi - 1);
      return i > lo && delta(a[i - 1], x2) > -delta(a[i], x2) ? i - 1 : i;
    }
    return { left: left3, center: center2, right: right3 };
  }
  function ascendingComparator(f) {
    return (d, x2) => ascending_default(f(d), x2);
  }

  // node_modules/d3-array/src/number.js
  function number_default(x2) {
    return x2 === null ? NaN : +x2;
  }

  // node_modules/d3-array/src/bisect.js
  var ascendingBisect = bisector_default(ascending_default);
  var bisectRight = ascendingBisect.right;
  var bisectLeft = ascendingBisect.left;
  var bisectCenter = bisector_default(number_default).center;
  var bisect_default = bisectRight;

  // node_modules/d3-array/src/ticks.js
  var e10 = Math.sqrt(50);
  var e5 = Math.sqrt(10);
  var e2 = Math.sqrt(2);
  function ticks_default(start2, stop, count) {
    var reverse3, i = -1, n, ticks, step2;
    stop = +stop, start2 = +start2, count = +count;
    if (start2 === stop && count > 0)
      return [start2];
    if (reverse3 = stop < start2)
      n = start2, start2 = stop, stop = n;
    if ((step2 = tickIncrement(start2, stop, count)) === 0 || !isFinite(step2))
      return [];
    if (step2 > 0) {
      let r0 = Math.round(start2 / step2), r1 = Math.round(stop / step2);
      if (r0 * step2 < start2)
        ++r0;
      if (r1 * step2 > stop)
        --r1;
      ticks = new Array(n = r1 - r0 + 1);
      while (++i < n)
        ticks[i] = (r0 + i) * step2;
    } else {
      step2 = -step2;
      let r0 = Math.round(start2 * step2), r1 = Math.round(stop * step2);
      if (r0 / step2 < start2)
        ++r0;
      if (r1 / step2 > stop)
        --r1;
      ticks = new Array(n = r1 - r0 + 1);
      while (++i < n)
        ticks[i] = (r0 + i) / step2;
    }
    if (reverse3)
      ticks.reverse();
    return ticks;
  }
  function tickIncrement(start2, stop, count) {
    var step2 = (stop - start2) / Math.max(0, count), power = Math.floor(Math.log(step2) / Math.LN10), error4 = step2 / Math.pow(10, power);
    return power >= 0 ? (error4 >= e10 ? 10 : error4 >= e5 ? 5 : error4 >= e2 ? 2 : 1) * Math.pow(10, power) : -Math.pow(10, -power) / (error4 >= e10 ? 10 : error4 >= e5 ? 5 : error4 >= e2 ? 2 : 1);
  }
  function tickStep(start2, stop, count) {
    var step0 = Math.abs(stop - start2) / Math.max(0, count), step1 = Math.pow(10, Math.floor(Math.log(step0) / Math.LN10)), error4 = step0 / step1;
    if (error4 >= e10)
      step1 *= 10;
    else if (error4 >= e5)
      step1 *= 5;
    else if (error4 >= e2)
      step1 *= 2;
    return stop < start2 ? -step1 : step1;
  }

  // node_modules/d3-array/src/range.js
  function range_default(start2, stop, step2) {
    start2 = +start2, stop = +stop, step2 = (n = arguments.length) < 2 ? (stop = start2, start2 = 0, 1) : n < 3 ? 1 : +step2;
    var i = -1, n = Math.max(0, Math.ceil((stop - start2) / step2)) | 0, range4 = new Array(n);
    while (++i < n) {
      range4[i] = start2 + i * step2;
    }
    return range4;
  }

  // node_modules/d3-axis/src/array.js
  var slice = Array.prototype.slice;

  // node_modules/d3-axis/src/identity.js
  function identity_default(x2) {
    return x2;
  }

  // node_modules/d3-axis/src/axis.js
  var top = 1;
  var right = 2;
  var bottom = 3;
  var left = 4;
  var epsilon = 1e-6;
  function translateX(x2) {
    return "translate(" + x2 + ",0)";
  }
  function translateY(y2) {
    return "translate(0," + y2 + ")";
  }
  function number(scale) {
    return (d) => +scale(d);
  }
  function center(scale, offset) {
    offset = Math.max(0, scale.bandwidth() - offset * 2) / 2;
    if (scale.round())
      offset = Math.round(offset);
    return (d) => +scale(d) + offset;
  }
  function entering() {
    return !this.__axis;
  }
  function axis(orient, scale) {
    var tickArguments = [], tickValues = null, tickFormat2 = null, tickSizeInner = 6, tickSizeOuter = 6, tickPadding = 3, offset = typeof window !== "undefined" && window.devicePixelRatio > 1 ? 0 : 0.5, k = orient === top || orient === left ? -1 : 1, x2 = orient === left || orient === right ? "x" : "y", transform2 = orient === top || orient === bottom ? translateX : translateY;
    function axis2(context) {
      var values2 = tickValues == null ? scale.ticks ? scale.ticks.apply(scale, tickArguments) : scale.domain() : tickValues, format2 = tickFormat2 == null ? scale.tickFormat ? scale.tickFormat.apply(scale, tickArguments) : identity_default : tickFormat2, spacing = Math.max(tickSizeInner, 0) + tickPadding, range4 = scale.range(), range0 = +range4[0] + offset, range1 = +range4[range4.length - 1] + offset, position2 = (scale.bandwidth ? center : number)(scale.copy(), offset), selection3 = context.selection ? context.selection() : context, path2 = selection3.selectAll(".domain").data([null]), tick = selection3.selectAll(".tick").data(values2, scale).order(), tickExit = tick.exit(), tickEnter = tick.enter().append("g").attr("class", "tick"), line = tick.select("line"), text2 = tick.select("text");
      path2 = path2.merge(path2.enter().insert("path", ".tick").attr("class", "domain").attr("stroke", "currentColor"));
      tick = tick.merge(tickEnter);
      line = line.merge(tickEnter.append("line").attr("stroke", "currentColor").attr(x2 + "2", k * tickSizeInner));
      text2 = text2.merge(tickEnter.append("text").attr("fill", "currentColor").attr(x2, k * spacing).attr("dy", orient === top ? "0em" : orient === bottom ? "0.71em" : "0.32em"));
      if (context !== selection3) {
        path2 = path2.transition(context);
        tick = tick.transition(context);
        line = line.transition(context);
        text2 = text2.transition(context);
        tickExit = tickExit.transition(context).attr("opacity", epsilon).attr("transform", function(d) {
          return isFinite(d = position2(d)) ? transform2(d + offset) : this.getAttribute("transform");
        });
        tickEnter.attr("opacity", epsilon).attr("transform", function(d) {
          var p = this.parentNode.__axis;
          return transform2((p && isFinite(p = p(d)) ? p : position2(d)) + offset);
        });
      }
      tickExit.remove();
      path2.attr("d", orient === left || orient === right ? tickSizeOuter ? "M" + k * tickSizeOuter + "," + range0 + "H" + offset + "V" + range1 + "H" + k * tickSizeOuter : "M" + offset + "," + range0 + "V" + range1 : tickSizeOuter ? "M" + range0 + "," + k * tickSizeOuter + "V" + offset + "H" + range1 + "V" + k * tickSizeOuter : "M" + range0 + "," + offset + "H" + range1);
      tick.attr("opacity", 1).attr("transform", function(d) {
        return transform2(position2(d) + offset);
      });
      line.attr(x2 + "2", k * tickSizeInner);
      text2.attr(x2, k * spacing).text(format2);
      selection3.filter(entering).attr("fill", "none").attr("font-size", 10).attr("font-family", "sans-serif").attr("text-anchor", orient === right ? "start" : orient === left ? "end" : "middle");
      selection3.each(function() {
        this.__axis = position2;
      });
    }
    axis2.scale = function(_) {
      return arguments.length ? (scale = _, axis2) : scale;
    };
    axis2.ticks = function() {
      return tickArguments = slice.call(arguments), axis2;
    };
    axis2.tickArguments = function(_) {
      return arguments.length ? (tickArguments = _ == null ? [] : slice.call(_), axis2) : tickArguments.slice();
    };
    axis2.tickValues = function(_) {
      return arguments.length ? (tickValues = _ == null ? null : slice.call(_), axis2) : tickValues && tickValues.slice();
    };
    axis2.tickFormat = function(_) {
      return arguments.length ? (tickFormat2 = _, axis2) : tickFormat2;
    };
    axis2.tickSize = function(_) {
      return arguments.length ? (tickSizeInner = tickSizeOuter = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeInner = function(_) {
      return arguments.length ? (tickSizeInner = +_, axis2) : tickSizeInner;
    };
    axis2.tickSizeOuter = function(_) {
      return arguments.length ? (tickSizeOuter = +_, axis2) : tickSizeOuter;
    };
    axis2.tickPadding = function(_) {
      return arguments.length ? (tickPadding = +_, axis2) : tickPadding;
    };
    axis2.offset = function(_) {
      return arguments.length ? (offset = +_, axis2) : offset;
    };
    return axis2;
  }
  function axisBottom(scale) {
    return axis(bottom, scale);
  }
  function axisLeft(scale) {
    return axis(left, scale);
  }

  // node_modules/d3-dispatch/src/dispatch.js
  var noop = { value: () => {
  } };
  function dispatch() {
    for (var i = 0, n = arguments.length, _ = {}, t; i < n; ++i) {
      if (!(t = arguments[i] + "") || t in _ || /[\s.]/.test(t))
        throw new Error("illegal type: " + t);
      _[t] = [];
    }
    return new Dispatch(_);
  }
  function Dispatch(_) {
    this._ = _;
  }
  function parseTypenames(typenames, types) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name3 = "", i = t.indexOf(".");
      if (i >= 0)
        name3 = t.slice(i + 1), t = t.slice(0, i);
      if (t && !types.hasOwnProperty(t))
        throw new Error("unknown type: " + t);
      return { type: t, name: name3 };
    });
  }
  Dispatch.prototype = dispatch.prototype = {
    constructor: Dispatch,
    on: function(typename, callback) {
      var _ = this._, T = parseTypenames(typename + "", _), t, i = -1, n = T.length;
      if (arguments.length < 2) {
        while (++i < n)
          if ((t = (typename = T[i]).type) && (t = get(_[t], typename.name)))
            return t;
        return;
      }
      if (callback != null && typeof callback !== "function")
        throw new Error("invalid callback: " + callback);
      while (++i < n) {
        if (t = (typename = T[i]).type)
          _[t] = set(_[t], typename.name, callback);
        else if (callback == null)
          for (t in _)
            _[t] = set(_[t], typename.name, null);
      }
      return this;
    },
    copy: function() {
      var copy2 = {}, _ = this._;
      for (var t in _)
        copy2[t] = _[t].slice();
      return new Dispatch(copy2);
    },
    call: function(type2, that) {
      if ((n = arguments.length - 2) > 0)
        for (var args = new Array(n), i = 0, n, t; i < n; ++i)
          args[i] = arguments[i + 2];
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    },
    apply: function(type2, that, args) {
      if (!this._.hasOwnProperty(type2))
        throw new Error("unknown type: " + type2);
      for (var t = this._[type2], i = 0, n = t.length; i < n; ++i)
        t[i].value.apply(that, args);
    }
  };
  function get(type2, name3) {
    for (var i = 0, n = type2.length, c; i < n; ++i) {
      if ((c = type2[i]).name === name3) {
        return c.value;
      }
    }
  }
  function set(type2, name3, callback) {
    for (var i = 0, n = type2.length; i < n; ++i) {
      if (type2[i].name === name3) {
        type2[i] = noop, type2 = type2.slice(0, i).concat(type2.slice(i + 1));
        break;
      }
    }
    if (callback != null)
      type2.push({ name: name3, value: callback });
    return type2;
  }
  var dispatch_default = dispatch;

  // node_modules/d3-selection/src/namespaces.js
  var xhtml = "http://www.w3.org/1999/xhtml";
  var namespaces_default = {
    svg: "http://www.w3.org/2000/svg",
    xhtml,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-selection/src/namespace.js
  function namespace_default(name3) {
    var prefix2 = name3 += "", i = prefix2.indexOf(":");
    if (i >= 0 && (prefix2 = name3.slice(0, i)) !== "xmlns")
      name3 = name3.slice(i + 1);
    return namespaces_default.hasOwnProperty(prefix2) ? { space: namespaces_default[prefix2], local: name3 } : name3;
  }

  // node_modules/d3-selection/src/creator.js
  function creatorInherit(name3) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml && document2.documentElement.namespaceURI === xhtml ? document2.createElement(name3) : document2.createElementNS(uri, name3);
    };
  }
  function creatorFixed(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default(name3) {
    var fullname = namespace_default(name3);
    return (fullname.local ? creatorFixed : creatorInherit)(fullname);
  }

  // node_modules/d3-selection/src/selector.js
  function none() {
  }
  function selector_default(selector) {
    return selector == null ? none : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-selection/src/selection/select.js
  function select_default(select) {
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group3[i]) && (subnode = select.call(node, node.__data__, i, group3))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/array.js
  function array_default(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-selection/src/selectorAll.js
  function empty() {
    return [];
  }
  function selectorAll_default(selector) {
    return selector == null ? empty : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectAll.js
  function arrayAll(select) {
    return function() {
      var group3 = select.apply(this, arguments);
      return group3 == null ? [] : array_default(group3);
    };
  }
  function selectAll_default(select) {
    if (typeof select === "function")
      select = arrayAll(select);
    else
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          subgroups.push(select.call(node, node.__data__, i, group3));
          parents.push(node);
        }
      }
    }
    return new Selection(subgroups, parents);
  }

  // node_modules/d3-selection/src/matcher.js
  function matcher_default(selector) {
    return function() {
      return this.matches(selector);
    };
  }
  function childMatcher(selector) {
    return function(node) {
      return node.matches(selector);
    };
  }

  // node_modules/d3-selection/src/selection/selectChild.js
  var find = Array.prototype.find;
  function childFind(match5) {
    return function() {
      return find.call(this.children, match5);
    };
  }
  function childFirst() {
    return this.firstElementChild;
  }
  function selectChild_default(match5) {
    return this.select(match5 == null ? childFirst : childFind(typeof match5 === "function" ? match5 : childMatcher(match5)));
  }

  // node_modules/d3-selection/src/selection/selectChildren.js
  var filter = Array.prototype.filter;
  function children() {
    return this.children;
  }
  function childrenFilter(match5) {
    return function() {
      return filter.call(this.children, match5);
    };
  }
  function selectChildren_default(match5) {
    return this.selectAll(match5 == null ? children : childrenFilter(typeof match5 === "function" ? match5 : childMatcher(match5)));
  }

  // node_modules/d3-selection/src/selection/filter.js
  function filter_default(match5) {
    if (typeof match5 !== "function")
      match5 = matcher_default(match5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group3[i]) && match5.call(node, node.__data__, i, group3)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection(subgroups, this._parents);
  }

  // node_modules/d3-selection/src/selection/sparse.js
  function sparse_default(update4) {
    return new Array(update4.length);
  }

  // node_modules/d3-selection/src/selection/enter.js
  function enter_default() {
    return new Selection(this._enter || this._groups.map(sparse_default), this._parents);
  }
  function EnterNode(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode.prototype = {
    constructor: EnterNode,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-selection/src/constant.js
  function constant_default(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-selection/src/selection/data.js
  function bindIndex(parent, group3, enter, update4, exit, data) {
    var i = 0, node, groupLength = group3.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group3[i]) {
        node.__data__ = data[i];
        update4[i] = node;
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group3[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey(parent, group3, enter, update4, exit, data, key) {
    var i, node, nodeByKeyValue = /* @__PURE__ */ new Map(), groupLength = group3.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group3[i]) {
        keyValues[i] = keyValue = key.call(node, node.__data__, i, group3) + "";
        if (nodeByKeyValue.has(keyValue)) {
          exit[i] = node;
        } else {
          nodeByKeyValue.set(keyValue, node);
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = key.call(parent, data[i], i, data) + "";
      if (node = nodeByKeyValue.get(keyValue)) {
        update4[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue.delete(keyValue);
      } else {
        enter[i] = new EnterNode(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group3[i]) && nodeByKeyValue.get(keyValues[i]) === node) {
        exit[i] = node;
      }
    }
  }
  function datum(node) {
    return node.__data__;
  }
  function data_default(value2, key) {
    if (!arguments.length)
      return Array.from(this, datum);
    var bind20 = key ? bindKey : bindIndex, parents = this._parents, groups = this._groups;
    if (typeof value2 !== "function")
      value2 = constant_default(value2);
    for (var m = groups.length, update4 = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent = parents[j], group3 = groups[j], groupLength = group3.length, data = array_default(value2.call(parent, parent && parent.__data__, j, parents)), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update4[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind20(parent, group3, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update4 = new Selection(update4, parents);
    update4._enter = enter;
    update4._exit = exit;
    return update4;
  }

  // node_modules/d3-selection/src/selection/exit.js
  function exit_default() {
    return new Selection(this._exit || this._groups.map(sparse_default), this._parents);
  }

  // node_modules/d3-selection/src/selection/join.js
  function join_default(onenter, onupdate, onexit) {
    var enter = this.enter(), update4 = this, exit = this.exit();
    enter = typeof onenter === "function" ? onenter(enter) : enter.append(onenter + "");
    if (onupdate != null)
      update4 = onupdate(update4);
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update4 ? enter.merge(update4).order() : update4;
  }

  // node_modules/d3-selection/src/selection/merge.js
  function merge_default(selection3) {
    if (!(selection3 instanceof Selection))
      throw new Error("invalid merge");
    for (var groups0 = this._groups, groups1 = selection3._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection(merges, this._parents);
  }

  // node_modules/d3-selection/src/selection/order.js
  function order_default() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group3 = groups[j], i = group3.length - 1, next = group3[i], node; --i >= 0; ) {
        if (node = group3[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/sort.js
  function sort_default(compare6) {
    if (!compare6)
      compare6 = ascending;
    function compareNode(a, b) {
      return a && b ? compare6(a.__data__, b.__data__) : !a - !b;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection(sortgroups, this._parents).order();
  }
  function ascending(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-selection/src/selection/call.js
  function call_default() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-selection/src/selection/nodes.js
  function nodes_default() {
    return Array.from(this);
  }

  // node_modules/d3-selection/src/selection/node.js
  function node_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i = 0, n = group3.length; i < n; ++i) {
        var node = group3[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-selection/src/selection/size.js
  function size_default() {
    let size3 = 0;
    for (const node of this)
      ++size3;
    return size3;
  }

  // node_modules/d3-selection/src/selection/empty.js
  function empty_default() {
    return !this.node();
  }

  // node_modules/d3-selection/src/selection/each.js
  function each_default(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i = 0, n = group3.length, node; i < n; ++i) {
        if (node = group3[i])
          callback.call(node, node.__data__, i, group3);
      }
    }
    return this;
  }

  // node_modules/d3-selection/src/selection/attr.js
  function attrRemove(name3) {
    return function() {
      this.removeAttribute(name3);
    };
  }
  function attrRemoveNS(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant(name3, value2) {
    return function() {
      this.setAttribute(name3, value2);
    };
  }
  function attrConstantNS(fullname, value2) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value2);
    };
  }
  function attrFunction(name3, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name3);
      else
        this.setAttribute(name3, v);
    };
  }
  function attrFunctionNS(fullname, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default(name3, value2) {
    var fullname = namespace_default(name3);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value2 == null ? fullname.local ? attrRemoveNS : attrRemove : typeof value2 === "function" ? fullname.local ? attrFunctionNS : attrFunction : fullname.local ? attrConstantNS : attrConstant)(fullname, value2));
  }

  // node_modules/d3-selection/src/window.js
  function window_default(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-selection/src/selection/style.js
  function styleRemove(name3) {
    return function() {
      this.style.removeProperty(name3);
    };
  }
  function styleConstant(name3, value2, priority) {
    return function() {
      this.style.setProperty(name3, value2, priority);
    };
  }
  function styleFunction(name3, value2, priority) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name3);
      else
        this.style.setProperty(name3, v, priority);
    };
  }
  function style_default(name3, value2, priority) {
    return arguments.length > 1 ? this.each((value2 == null ? styleRemove : typeof value2 === "function" ? styleFunction : styleConstant)(name3, value2, priority == null ? "" : priority)) : styleValue(this.node(), name3);
  }
  function styleValue(node, name3) {
    return node.style.getPropertyValue(name3) || window_default(node).getComputedStyle(node, null).getPropertyValue(name3);
  }

  // node_modules/d3-selection/src/selection/property.js
  function propertyRemove(name3) {
    return function() {
      delete this[name3];
    };
  }
  function propertyConstant(name3, value2) {
    return function() {
      this[name3] = value2;
    };
  }
  function propertyFunction(name3, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        delete this[name3];
      else
        this[name3] = v;
    };
  }
  function property_default(name3, value2) {
    return arguments.length > 1 ? this.each((value2 == null ? propertyRemove : typeof value2 === "function" ? propertyFunction : propertyConstant)(name3, value2)) : this.node()[name3];
  }

  // node_modules/d3-selection/src/selection/classed.js
  function classArray(string3) {
    return string3.trim().split(/^|\s+/);
  }
  function classList(node) {
    return node.classList || new ClassList(node);
  }
  function ClassList(node) {
    this._node = node;
    this._names = classArray(node.getAttribute("class") || "");
  }
  ClassList.prototype = {
    add: function(name3) {
      var i = this._names.indexOf(name3);
      if (i < 0) {
        this._names.push(name3);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name3) {
      var i = this._names.indexOf(name3);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name3) {
      return this._names.indexOf(name3) >= 0;
    }
  };
  function classedAdd(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove(node, names) {
    var list = classList(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue(names) {
    return function() {
      classedAdd(this, names);
    };
  }
  function classedFalse(names) {
    return function() {
      classedRemove(this, names);
    };
  }
  function classedFunction(names, value2) {
    return function() {
      (value2.apply(this, arguments) ? classedAdd : classedRemove)(this, names);
    };
  }
  function classed_default(name3, value2) {
    var names = classArray(name3 + "");
    if (arguments.length < 2) {
      var list = classList(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value2 === "function" ? classedFunction : value2 ? classedTrue : classedFalse)(names, value2));
  }

  // node_modules/d3-selection/src/selection/text.js
  function textRemove() {
    this.textContent = "";
  }
  function textConstant(value2) {
    return function() {
      this.textContent = value2;
    };
  }
  function textFunction(value2) {
    return function() {
      var v = value2.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default(value2) {
    return arguments.length ? this.each(value2 == null ? textRemove : (typeof value2 === "function" ? textFunction : textConstant)(value2)) : this.node().textContent;
  }

  // node_modules/d3-selection/src/selection/html.js
  function htmlRemove() {
    this.innerHTML = "";
  }
  function htmlConstant(value2) {
    return function() {
      this.innerHTML = value2;
    };
  }
  function htmlFunction(value2) {
    return function() {
      var v = value2.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default(value2) {
    return arguments.length ? this.each(value2 == null ? htmlRemove : (typeof value2 === "function" ? htmlFunction : htmlConstant)(value2)) : this.node().innerHTML;
  }

  // node_modules/d3-selection/src/selection/raise.js
  function raise() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default() {
    return this.each(raise);
  }

  // node_modules/d3-selection/src/selection/lower.js
  function lower() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default() {
    return this.each(lower);
  }

  // node_modules/d3-selection/src/selection/append.js
  function append_default(name3) {
    var create2 = typeof name3 === "function" ? name3 : creator_default(name3);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-selection/src/selection/insert.js
  function constantNull() {
    return null;
  }
  function insert_default(name3, before) {
    var create2 = typeof name3 === "function" ? name3 : creator_default(name3), select = before == null ? constantNull : typeof before === "function" ? before : selector_default(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-selection/src/selection/remove.js
  function remove() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default() {
    return this.each(remove);
  }

  // node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default(deep) {
    return this.select(deep ? selection_cloneDeep : selection_cloneShallow);
  }

  // node_modules/d3-selection/src/selection/datum.js
  function datum_default(value2) {
    return arguments.length ? this.property("__data__", value2) : this.node().__data__;
  }

  // node_modules/d3-selection/src/selection/on.js
  function contextListener(listener) {
    return function(event2) {
      listener.call(this, event2, this.__data__);
    };
  }
  function parseTypenames2(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name3 = "", i = t.indexOf(".");
      if (i >= 0)
        name3 = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name: name3 };
    });
  }
  function onRemove(typename) {
    return function() {
      var on2 = this.__on;
      if (!on2)
        return;
      for (var j = 0, i = -1, m = on2.length, o; j < m; ++j) {
        if (o = on2[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.options);
        } else {
          on2[++i] = o;
        }
      }
      if (++i)
        on2.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd(typename, value2, options) {
    return function() {
      var on2 = this.__on, o, listener = contextListener(value2);
      if (on2)
        for (var j = 0, m = on2.length; j < m; ++j) {
          if ((o = on2[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.options);
            this.addEventListener(o.type, o.listener = listener, o.options = options);
            o.value = value2;
            return;
          }
        }
      this.addEventListener(typename.type, listener, options);
      o = { type: typename.type, name: typename.name, value: value2, listener, options };
      if (!on2)
        this.__on = [o];
      else
        on2.push(o);
    };
  }
  function on_default(typename, value2, options) {
    var typenames = parseTypenames2(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on2 = this.node().__on;
      if (on2)
        for (var j = 0, m = on2.length, o; j < m; ++j) {
          for (i = 0, o = on2[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on2 = value2 ? onAdd : onRemove;
    for (i = 0; i < n; ++i)
      this.each(on2(typenames[i], value2, options));
    return this;
  }

  // node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent(node, type2, params) {
    var window2 = window_default(node), event2 = window2.CustomEvent;
    if (typeof event2 === "function") {
      event2 = new event2(type2, params);
    } else {
      event2 = window2.document.createEvent("Event");
      if (params)
        event2.initEvent(type2, params.bubbles, params.cancelable), event2.detail = params.detail;
      else
        event2.initEvent(type2, false, false);
    }
    node.dispatchEvent(event2);
  }
  function dispatchConstant(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params);
    };
  }
  function dispatchFunction(type2, params) {
    return function() {
      return dispatchEvent(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default2(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction : dispatchConstant)(type2, params));
  }

  // node_modules/d3-selection/src/selection/iterator.js
  function* iterator_default() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i = 0, n = group3.length, node; i < n; ++i) {
        if (node = group3[i])
          yield node;
      }
    }
  }

  // node_modules/d3-selection/src/selection/index.js
  var root = [null];
  function Selection(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection() {
    return new Selection([[document.documentElement]], root);
  }
  function selection_selection() {
    return this;
  }
  Selection.prototype = selection.prototype = {
    constructor: Selection,
    select: select_default,
    selectAll: selectAll_default,
    selectChild: selectChild_default,
    selectChildren: selectChildren_default,
    filter: filter_default,
    data: data_default,
    enter: enter_default,
    exit: exit_default,
    join: join_default,
    merge: merge_default,
    selection: selection_selection,
    order: order_default,
    sort: sort_default,
    call: call_default,
    nodes: nodes_default,
    node: node_default,
    size: size_default,
    empty: empty_default,
    each: each_default,
    attr: attr_default,
    style: style_default,
    property: property_default,
    classed: classed_default,
    text: text_default,
    html: html_default,
    raise: raise_default,
    lower: lower_default,
    append: append_default,
    insert: insert_default,
    remove: remove_default,
    clone: clone_default,
    datum: datum_default,
    on: on_default,
    dispatch: dispatch_default2,
    [Symbol.iterator]: iterator_default
  };
  var selection_default = selection;

  // node_modules/d3-selection/src/select.js
  function select_default2(selector) {
    return typeof selector === "string" ? new Selection([[document.querySelector(selector)]], [document.documentElement]) : new Selection([[selector]], root);
  }

  // node_modules/d3-color/src/define.js
  function define_default(constructor, factory, prototype) {
    constructor.prototype = factory.prototype = prototype;
    prototype.constructor = constructor;
  }
  function extend(parent, definition) {
    var prototype = Object.create(parent.prototype);
    for (var key in definition)
      prototype[key] = definition[key];
    return prototype;
  }

  // node_modules/d3-color/src/color.js
  function Color() {
  }
  var darker = 0.7;
  var brighter = 1 / darker;
  var reI = "\\s*([+-]?\\d+)\\s*";
  var reN = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)\\s*";
  var reP = "\\s*([+-]?\\d*\\.?\\d+(?:[eE][+-]?\\d+)?)%\\s*";
  var reHex = /^#([0-9a-f]{3,8})$/;
  var reRgbInteger = new RegExp("^rgb\\(" + [reI, reI, reI] + "\\)$");
  var reRgbPercent = new RegExp("^rgb\\(" + [reP, reP, reP] + "\\)$");
  var reRgbaInteger = new RegExp("^rgba\\(" + [reI, reI, reI, reN] + "\\)$");
  var reRgbaPercent = new RegExp("^rgba\\(" + [reP, reP, reP, reN] + "\\)$");
  var reHslPercent = new RegExp("^hsl\\(" + [reN, reP, reP] + "\\)$");
  var reHslaPercent = new RegExp("^hsla\\(" + [reN, reP, reP, reN] + "\\)$");
  var named = {
    aliceblue: 15792383,
    antiquewhite: 16444375,
    aqua: 65535,
    aquamarine: 8388564,
    azure: 15794175,
    beige: 16119260,
    bisque: 16770244,
    black: 0,
    blanchedalmond: 16772045,
    blue: 255,
    blueviolet: 9055202,
    brown: 10824234,
    burlywood: 14596231,
    cadetblue: 6266528,
    chartreuse: 8388352,
    chocolate: 13789470,
    coral: 16744272,
    cornflowerblue: 6591981,
    cornsilk: 16775388,
    crimson: 14423100,
    cyan: 65535,
    darkblue: 139,
    darkcyan: 35723,
    darkgoldenrod: 12092939,
    darkgray: 11119017,
    darkgreen: 25600,
    darkgrey: 11119017,
    darkkhaki: 12433259,
    darkmagenta: 9109643,
    darkolivegreen: 5597999,
    darkorange: 16747520,
    darkorchid: 10040012,
    darkred: 9109504,
    darksalmon: 15308410,
    darkseagreen: 9419919,
    darkslateblue: 4734347,
    darkslategray: 3100495,
    darkslategrey: 3100495,
    darkturquoise: 52945,
    darkviolet: 9699539,
    deeppink: 16716947,
    deepskyblue: 49151,
    dimgray: 6908265,
    dimgrey: 6908265,
    dodgerblue: 2003199,
    firebrick: 11674146,
    floralwhite: 16775920,
    forestgreen: 2263842,
    fuchsia: 16711935,
    gainsboro: 14474460,
    ghostwhite: 16316671,
    gold: 16766720,
    goldenrod: 14329120,
    gray: 8421504,
    green: 32768,
    greenyellow: 11403055,
    grey: 8421504,
    honeydew: 15794160,
    hotpink: 16738740,
    indianred: 13458524,
    indigo: 4915330,
    ivory: 16777200,
    khaki: 15787660,
    lavender: 15132410,
    lavenderblush: 16773365,
    lawngreen: 8190976,
    lemonchiffon: 16775885,
    lightblue: 11393254,
    lightcoral: 15761536,
    lightcyan: 14745599,
    lightgoldenrodyellow: 16448210,
    lightgray: 13882323,
    lightgreen: 9498256,
    lightgrey: 13882323,
    lightpink: 16758465,
    lightsalmon: 16752762,
    lightseagreen: 2142890,
    lightskyblue: 8900346,
    lightslategray: 7833753,
    lightslategrey: 7833753,
    lightsteelblue: 11584734,
    lightyellow: 16777184,
    lime: 65280,
    limegreen: 3329330,
    linen: 16445670,
    magenta: 16711935,
    maroon: 8388608,
    mediumaquamarine: 6737322,
    mediumblue: 205,
    mediumorchid: 12211667,
    mediumpurple: 9662683,
    mediumseagreen: 3978097,
    mediumslateblue: 8087790,
    mediumspringgreen: 64154,
    mediumturquoise: 4772300,
    mediumvioletred: 13047173,
    midnightblue: 1644912,
    mintcream: 16121850,
    mistyrose: 16770273,
    moccasin: 16770229,
    navajowhite: 16768685,
    navy: 128,
    oldlace: 16643558,
    olive: 8421376,
    olivedrab: 7048739,
    orange: 16753920,
    orangered: 16729344,
    orchid: 14315734,
    palegoldenrod: 15657130,
    palegreen: 10025880,
    paleturquoise: 11529966,
    palevioletred: 14381203,
    papayawhip: 16773077,
    peachpuff: 16767673,
    peru: 13468991,
    pink: 16761035,
    plum: 14524637,
    powderblue: 11591910,
    purple: 8388736,
    rebeccapurple: 6697881,
    red: 16711680,
    rosybrown: 12357519,
    royalblue: 4286945,
    saddlebrown: 9127187,
    salmon: 16416882,
    sandybrown: 16032864,
    seagreen: 3050327,
    seashell: 16774638,
    sienna: 10506797,
    silver: 12632256,
    skyblue: 8900331,
    slateblue: 6970061,
    slategray: 7372944,
    slategrey: 7372944,
    snow: 16775930,
    springgreen: 65407,
    steelblue: 4620980,
    tan: 13808780,
    teal: 32896,
    thistle: 14204888,
    tomato: 16737095,
    turquoise: 4251856,
    violet: 15631086,
    wheat: 16113331,
    white: 16777215,
    whitesmoke: 16119285,
    yellow: 16776960,
    yellowgreen: 10145074
  };
  define_default(Color, color, {
    copy: function(channels) {
      return Object.assign(new this.constructor(), this, channels);
    },
    displayable: function() {
      return this.rgb().displayable();
    },
    hex: color_formatHex,
    formatHex: color_formatHex,
    formatHsl: color_formatHsl,
    formatRgb: color_formatRgb,
    toString: color_formatRgb
  });
  function color_formatHex() {
    return this.rgb().formatHex();
  }
  function color_formatHsl() {
    return hslConvert(this).formatHsl();
  }
  function color_formatRgb() {
    return this.rgb().formatRgb();
  }
  function color(format2) {
    var m, l;
    format2 = (format2 + "").trim().toLowerCase();
    return (m = reHex.exec(format2)) ? (l = m[1].length, m = parseInt(m[1], 16), l === 6 ? rgbn(m) : l === 3 ? new Rgb(m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, (m & 15) << 4 | m & 15, 1) : l === 8 ? rgba(m >> 24 & 255, m >> 16 & 255, m >> 8 & 255, (m & 255) / 255) : l === 4 ? rgba(m >> 12 & 15 | m >> 8 & 240, m >> 8 & 15 | m >> 4 & 240, m >> 4 & 15 | m & 240, ((m & 15) << 4 | m & 15) / 255) : null) : (m = reRgbInteger.exec(format2)) ? new Rgb(m[1], m[2], m[3], 1) : (m = reRgbPercent.exec(format2)) ? new Rgb(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, 1) : (m = reRgbaInteger.exec(format2)) ? rgba(m[1], m[2], m[3], m[4]) : (m = reRgbaPercent.exec(format2)) ? rgba(m[1] * 255 / 100, m[2] * 255 / 100, m[3] * 255 / 100, m[4]) : (m = reHslPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, 1) : (m = reHslaPercent.exec(format2)) ? hsla(m[1], m[2] / 100, m[3] / 100, m[4]) : named.hasOwnProperty(format2) ? rgbn(named[format2]) : format2 === "transparent" ? new Rgb(NaN, NaN, NaN, 0) : null;
  }
  function rgbn(n) {
    return new Rgb(n >> 16 & 255, n >> 8 & 255, n & 255, 1);
  }
  function rgba(r, g, b, a) {
    if (a <= 0)
      r = g = b = NaN;
    return new Rgb(r, g, b, a);
  }
  function rgbConvert(o) {
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Rgb();
    o = o.rgb();
    return new Rgb(o.r, o.g, o.b, o.opacity);
  }
  function rgb(r, g, b, opacity) {
    return arguments.length === 1 ? rgbConvert(r) : new Rgb(r, g, b, opacity == null ? 1 : opacity);
  }
  function Rgb(r, g, b, opacity) {
    this.r = +r;
    this.g = +g;
    this.b = +b;
    this.opacity = +opacity;
  }
  define_default(Rgb, rgb, extend(Color, {
    brighter: function(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    darker: function(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Rgb(this.r * k, this.g * k, this.b * k, this.opacity);
    },
    rgb: function() {
      return this;
    },
    displayable: function() {
      return -0.5 <= this.r && this.r < 255.5 && (-0.5 <= this.g && this.g < 255.5) && (-0.5 <= this.b && this.b < 255.5) && (0 <= this.opacity && this.opacity <= 1);
    },
    hex: rgb_formatHex,
    formatHex: rgb_formatHex,
    formatRgb: rgb_formatRgb,
    toString: rgb_formatRgb
  }));
  function rgb_formatHex() {
    return "#" + hex(this.r) + hex(this.g) + hex(this.b);
  }
  function rgb_formatRgb() {
    var a = this.opacity;
    a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
    return (a === 1 ? "rgb(" : "rgba(") + Math.max(0, Math.min(255, Math.round(this.r) || 0)) + ", " + Math.max(0, Math.min(255, Math.round(this.g) || 0)) + ", " + Math.max(0, Math.min(255, Math.round(this.b) || 0)) + (a === 1 ? ")" : ", " + a + ")");
  }
  function hex(value2) {
    value2 = Math.max(0, Math.min(255, Math.round(value2) || 0));
    return (value2 < 16 ? "0" : "") + value2.toString(16);
  }
  function hsla(h, s, l, a) {
    if (a <= 0)
      h = s = l = NaN;
    else if (l <= 0 || l >= 1)
      h = s = NaN;
    else if (s <= 0)
      h = NaN;
    return new Hsl(h, s, l, a);
  }
  function hslConvert(o) {
    if (o instanceof Hsl)
      return new Hsl(o.h, o.s, o.l, o.opacity);
    if (!(o instanceof Color))
      o = color(o);
    if (!o)
      return new Hsl();
    if (o instanceof Hsl)
      return o;
    o = o.rgb();
    var r = o.r / 255, g = o.g / 255, b = o.b / 255, min4 = Math.min(r, g, b), max5 = Math.max(r, g, b), h = NaN, s = max5 - min4, l = (max5 + min4) / 2;
    if (s) {
      if (r === max5)
        h = (g - b) / s + (g < b) * 6;
      else if (g === max5)
        h = (b - r) / s + 2;
      else
        h = (r - g) / s + 4;
      s /= l < 0.5 ? max5 + min4 : 2 - max5 - min4;
      h *= 60;
    } else {
      s = l > 0 && l < 1 ? 0 : h;
    }
    return new Hsl(h, s, l, o.opacity);
  }
  function hsl(h, s, l, opacity) {
    return arguments.length === 1 ? hslConvert(h) : new Hsl(h, s, l, opacity == null ? 1 : opacity);
  }
  function Hsl(h, s, l, opacity) {
    this.h = +h;
    this.s = +s;
    this.l = +l;
    this.opacity = +opacity;
  }
  define_default(Hsl, hsl, extend(Color, {
    brighter: function(k) {
      k = k == null ? brighter : Math.pow(brighter, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    darker: function(k) {
      k = k == null ? darker : Math.pow(darker, k);
      return new Hsl(this.h, this.s, this.l * k, this.opacity);
    },
    rgb: function() {
      var h = this.h % 360 + (this.h < 0) * 360, s = isNaN(h) || isNaN(this.s) ? 0 : this.s, l = this.l, m2 = l + (l < 0.5 ? l : 1 - l) * s, m1 = 2 * l - m2;
      return new Rgb(
        hsl2rgb(h >= 240 ? h - 240 : h + 120, m1, m2),
        hsl2rgb(h, m1, m2),
        hsl2rgb(h < 120 ? h + 240 : h - 120, m1, m2),
        this.opacity
      );
    },
    displayable: function() {
      return (0 <= this.s && this.s <= 1 || isNaN(this.s)) && (0 <= this.l && this.l <= 1) && (0 <= this.opacity && this.opacity <= 1);
    },
    formatHsl: function() {
      var a = this.opacity;
      a = isNaN(a) ? 1 : Math.max(0, Math.min(1, a));
      return (a === 1 ? "hsl(" : "hsla(") + (this.h || 0) + ", " + (this.s || 0) * 100 + "%, " + (this.l || 0) * 100 + "%" + (a === 1 ? ")" : ", " + a + ")");
    }
  }));
  function hsl2rgb(h, m1, m2) {
    return (h < 60 ? m1 + (m2 - m1) * h / 60 : h < 180 ? m2 : h < 240 ? m1 + (m2 - m1) * (240 - h) / 60 : m1) * 255;
  }

  // node_modules/d3-interpolate/src/basis.js
  function basis(t1, v0, v1, v2, v3) {
    var t2 = t1 * t1, t3 = t2 * t1;
    return ((1 - 3 * t1 + 3 * t2 - t3) * v0 + (4 - 6 * t2 + 3 * t3) * v1 + (1 + 3 * t1 + 3 * t2 - 3 * t3) * v2 + t3 * v3) / 6;
  }
  function basis_default(values2) {
    var n = values2.length - 1;
    return function(t) {
      var i = t <= 0 ? t = 0 : t >= 1 ? (t = 1, n - 1) : Math.floor(t * n), v1 = values2[i], v2 = values2[i + 1], v0 = i > 0 ? values2[i - 1] : 2 * v1 - v2, v3 = i < n - 1 ? values2[i + 2] : 2 * v2 - v1;
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/basisClosed.js
  function basisClosed_default(values2) {
    var n = values2.length;
    return function(t) {
      var i = Math.floor(((t %= 1) < 0 ? ++t : t) * n), v0 = values2[(i + n - 1) % n], v1 = values2[i % n], v2 = values2[(i + 1) % n], v3 = values2[(i + 2) % n];
      return basis((t - i / n) * n, v0, v1, v2, v3);
    };
  }

  // node_modules/d3-interpolate/src/constant.js
  var constant_default2 = (x2) => () => x2;

  // node_modules/d3-interpolate/src/color.js
  function linear(a, d) {
    return function(t) {
      return a + t * d;
    };
  }
  function exponential(a, b, y2) {
    return a = Math.pow(a, y2), b = Math.pow(b, y2) - a, y2 = 1 / y2, function(t) {
      return Math.pow(a + t * b, y2);
    };
  }
  function gamma(y2) {
    return (y2 = +y2) === 1 ? nogamma : function(a, b) {
      return b - a ? exponential(a, b, y2) : constant_default2(isNaN(a) ? b : a);
    };
  }
  function nogamma(a, b) {
    var d = b - a;
    return d ? linear(a, d) : constant_default2(isNaN(a) ? b : a);
  }

  // node_modules/d3-interpolate/src/rgb.js
  var rgb_default = function rgbGamma(y2) {
    var color2 = gamma(y2);
    function rgb2(start2, end) {
      var r = color2((start2 = rgb(start2)).r, (end = rgb(end)).r), g = color2(start2.g, end.g), b = color2(start2.b, end.b), opacity = nogamma(start2.opacity, end.opacity);
      return function(t) {
        start2.r = r(t);
        start2.g = g(t);
        start2.b = b(t);
        start2.opacity = opacity(t);
        return start2 + "";
      };
    }
    rgb2.gamma = rgbGamma;
    return rgb2;
  }(1);
  function rgbSpline(spline) {
    return function(colors) {
      var n = colors.length, r = new Array(n), g = new Array(n), b = new Array(n), i, color2;
      for (i = 0; i < n; ++i) {
        color2 = rgb(colors[i]);
        r[i] = color2.r || 0;
        g[i] = color2.g || 0;
        b[i] = color2.b || 0;
      }
      r = spline(r);
      g = spline(g);
      b = spline(b);
      color2.opacity = 1;
      return function(t) {
        color2.r = r(t);
        color2.g = g(t);
        color2.b = b(t);
        return color2 + "";
      };
    };
  }
  var rgbBasis = rgbSpline(basis_default);
  var rgbBasisClosed = rgbSpline(basisClosed_default);

  // node_modules/d3-interpolate/src/numberArray.js
  function numberArray_default(a, b) {
    if (!b)
      b = [];
    var n = a ? Math.min(b.length, a.length) : 0, c = b.slice(), i;
    return function(t) {
      for (i = 0; i < n; ++i)
        c[i] = a[i] * (1 - t) + b[i] * t;
      return c;
    };
  }
  function isNumberArray(x2) {
    return ArrayBuffer.isView(x2) && !(x2 instanceof DataView);
  }

  // node_modules/d3-interpolate/src/array.js
  function genericArray(a, b) {
    var nb = b ? b.length : 0, na = a ? Math.min(nb, a.length) : 0, x2 = new Array(na), c = new Array(nb), i;
    for (i = 0; i < na; ++i)
      x2[i] = value_default(a[i], b[i]);
    for (; i < nb; ++i)
      c[i] = b[i];
    return function(t) {
      for (i = 0; i < na; ++i)
        c[i] = x2[i](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/date.js
  function date_default(a, b) {
    var d = new Date();
    return a = +a, b = +b, function(t) {
      return d.setTime(a * (1 - t) + b * t), d;
    };
  }

  // node_modules/d3-interpolate/src/number.js
  function number_default2(a, b) {
    return a = +a, b = +b, function(t) {
      return a * (1 - t) + b * t;
    };
  }

  // node_modules/d3-interpolate/src/object.js
  function object_default(a, b) {
    var i = {}, c = {}, k;
    if (a === null || typeof a !== "object")
      a = {};
    if (b === null || typeof b !== "object")
      b = {};
    for (k in b) {
      if (k in a) {
        i[k] = value_default(a[k], b[k]);
      } else {
        c[k] = b[k];
      }
    }
    return function(t) {
      for (k in i)
        c[k] = i[k](t);
      return c;
    };
  }

  // node_modules/d3-interpolate/src/string.js
  var reA = /[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g;
  var reB = new RegExp(reA.source, "g");
  function zero(b) {
    return function() {
      return b;
    };
  }
  function one(b) {
    return function(t) {
      return b(t) + "";
    };
  }
  function string_default(a, b) {
    var bi = reA.lastIndex = reB.lastIndex = 0, am, bm, bs, i = -1, s = [], q = [];
    a = a + "", b = b + "";
    while ((am = reA.exec(a)) && (bm = reB.exec(b))) {
      if ((bs = bm.index) > bi) {
        bs = b.slice(bi, bs);
        if (s[i])
          s[i] += bs;
        else
          s[++i] = bs;
      }
      if ((am = am[0]) === (bm = bm[0])) {
        if (s[i])
          s[i] += bm;
        else
          s[++i] = bm;
      } else {
        s[++i] = null;
        q.push({ i, x: number_default2(am, bm) });
      }
      bi = reB.lastIndex;
    }
    if (bi < b.length) {
      bs = b.slice(bi);
      if (s[i])
        s[i] += bs;
      else
        s[++i] = bs;
    }
    return s.length < 2 ? q[0] ? one(q[0].x) : zero(b) : (b = q.length, function(t) {
      for (var i2 = 0, o; i2 < b; ++i2)
        s[(o = q[i2]).i] = o.x(t);
      return s.join("");
    });
  }

  // node_modules/d3-interpolate/src/value.js
  function value_default(a, b) {
    var t = typeof b, c;
    return b == null || t === "boolean" ? constant_default2(b) : (t === "number" ? number_default2 : t === "string" ? (c = color(b)) ? (b = c, rgb_default) : string_default : b instanceof color ? rgb_default : b instanceof Date ? date_default : isNumberArray(b) ? numberArray_default : Array.isArray(b) ? genericArray : typeof b.valueOf !== "function" && typeof b.toString !== "function" || isNaN(b) ? object_default : number_default2)(a, b);
  }

  // node_modules/d3-interpolate/src/round.js
  function round_default(a, b) {
    return a = +a, b = +b, function(t) {
      return Math.round(a * (1 - t) + b * t);
    };
  }

  // node_modules/d3-interpolate/src/transform/decompose.js
  var degrees = 180 / Math.PI;
  var identity = {
    translateX: 0,
    translateY: 0,
    rotate: 0,
    skewX: 0,
    scaleX: 1,
    scaleY: 1
  };
  function decompose_default(a, b, c, d, e, f) {
    var scaleX, scaleY, skewX;
    if (scaleX = Math.sqrt(a * a + b * b))
      a /= scaleX, b /= scaleX;
    if (skewX = a * c + b * d)
      c -= a * skewX, d -= b * skewX;
    if (scaleY = Math.sqrt(c * c + d * d))
      c /= scaleY, d /= scaleY, skewX /= scaleY;
    if (a * d < b * c)
      a = -a, b = -b, skewX = -skewX, scaleX = -scaleX;
    return {
      translateX: e,
      translateY: f,
      rotate: Math.atan2(b, a) * degrees,
      skewX: Math.atan(skewX) * degrees,
      scaleX,
      scaleY
    };
  }

  // node_modules/d3-interpolate/src/transform/parse.js
  var svgNode;
  function parseCss(value2) {
    const m = new (typeof DOMMatrix === "function" ? DOMMatrix : WebKitCSSMatrix)(value2 + "");
    return m.isIdentity ? identity : decompose_default(m.a, m.b, m.c, m.d, m.e, m.f);
  }
  function parseSvg(value2) {
    if (value2 == null)
      return identity;
    if (!svgNode)
      svgNode = document.createElementNS("http://www.w3.org/2000/svg", "g");
    svgNode.setAttribute("transform", value2);
    if (!(value2 = svgNode.transform.baseVal.consolidate()))
      return identity;
    value2 = value2.matrix;
    return decompose_default(value2.a, value2.b, value2.c, value2.d, value2.e, value2.f);
  }

  // node_modules/d3-interpolate/src/transform/index.js
  function interpolateTransform(parse2, pxComma, pxParen, degParen) {
    function pop3(s) {
      return s.length ? s.pop() + " " : "";
    }
    function translate(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push("translate(", null, pxComma, null, pxParen);
        q.push({ i: i - 4, x: number_default2(xa, xb) }, { i: i - 2, x: number_default2(ya, yb) });
      } else if (xb || yb) {
        s.push("translate(" + xb + pxComma + yb + pxParen);
      }
    }
    function rotate(a, b, s, q) {
      if (a !== b) {
        if (a - b > 180)
          b += 360;
        else if (b - a > 180)
          a += 360;
        q.push({ i: s.push(pop3(s) + "rotate(", null, degParen) - 2, x: number_default2(a, b) });
      } else if (b) {
        s.push(pop3(s) + "rotate(" + b + degParen);
      }
    }
    function skewX(a, b, s, q) {
      if (a !== b) {
        q.push({ i: s.push(pop3(s) + "skewX(", null, degParen) - 2, x: number_default2(a, b) });
      } else if (b) {
        s.push(pop3(s) + "skewX(" + b + degParen);
      }
    }
    function scale(xa, ya, xb, yb, s, q) {
      if (xa !== xb || ya !== yb) {
        var i = s.push(pop3(s) + "scale(", null, ",", null, ")");
        q.push({ i: i - 4, x: number_default2(xa, xb) }, { i: i - 2, x: number_default2(ya, yb) });
      } else if (xb !== 1 || yb !== 1) {
        s.push(pop3(s) + "scale(" + xb + "," + yb + ")");
      }
    }
    return function(a, b) {
      var s = [], q = [];
      a = parse2(a), b = parse2(b);
      translate(a.translateX, a.translateY, b.translateX, b.translateY, s, q);
      rotate(a.rotate, b.rotate, s, q);
      skewX(a.skewX, b.skewX, s, q);
      scale(a.scaleX, a.scaleY, b.scaleX, b.scaleY, s, q);
      a = b = null;
      return function(t) {
        var i = -1, n = q.length, o;
        while (++i < n)
          s[(o = q[i]).i] = o.x(t);
        return s.join("");
      };
    };
  }
  var interpolateTransformCss = interpolateTransform(parseCss, "px, ", "px)", "deg)");
  var interpolateTransformSvg = interpolateTransform(parseSvg, ", ", ")", ")");

  // node_modules/d3-timer/src/timer.js
  var frame = 0;
  var timeout = 0;
  var interval = 0;
  var pokeDelay = 1e3;
  var taskHead;
  var taskTail;
  var clockLast = 0;
  var clockNow = 0;
  var clockSkew = 0;
  var clock = typeof performance === "object" && performance.now ? performance : Date;
  var setFrame = typeof window === "object" && window.requestAnimationFrame ? window.requestAnimationFrame.bind(window) : function(f) {
    setTimeout(f, 17);
  };
  function now() {
    return clockNow || (setFrame(clearNow), clockNow = clock.now() + clockSkew);
  }
  function clearNow() {
    clockNow = 0;
  }
  function Timer() {
    this._call = this._time = this._next = null;
  }
  Timer.prototype = timer.prototype = {
    constructor: Timer,
    restart: function(callback, delay, time2) {
      if (typeof callback !== "function")
        throw new TypeError("callback is not a function");
      time2 = (time2 == null ? now() : +time2) + (delay == null ? 0 : +delay);
      if (!this._next && taskTail !== this) {
        if (taskTail)
          taskTail._next = this;
        else
          taskHead = this;
        taskTail = this;
      }
      this._call = callback;
      this._time = time2;
      sleep();
    },
    stop: function() {
      if (this._call) {
        this._call = null;
        this._time = Infinity;
        sleep();
      }
    }
  };
  function timer(callback, delay, time2) {
    var t = new Timer();
    t.restart(callback, delay, time2);
    return t;
  }
  function timerFlush() {
    now();
    ++frame;
    var t = taskHead, e;
    while (t) {
      if ((e = clockNow - t._time) >= 0)
        t._call.call(null, e);
      t = t._next;
    }
    --frame;
  }
  function wake() {
    clockNow = (clockLast = clock.now()) + clockSkew;
    frame = timeout = 0;
    try {
      timerFlush();
    } finally {
      frame = 0;
      nap();
      clockNow = 0;
    }
  }
  function poke() {
    var now3 = clock.now(), delay = now3 - clockLast;
    if (delay > pokeDelay)
      clockSkew -= delay, clockLast = now3;
  }
  function nap() {
    var t0, t1 = taskHead, t2, time2 = Infinity;
    while (t1) {
      if (t1._call) {
        if (time2 > t1._time)
          time2 = t1._time;
        t0 = t1, t1 = t1._next;
      } else {
        t2 = t1._next, t1._next = null;
        t1 = t0 ? t0._next = t2 : taskHead = t2;
      }
    }
    taskTail = t0;
    sleep(time2);
  }
  function sleep(time2) {
    if (frame)
      return;
    if (timeout)
      timeout = clearTimeout(timeout);
    var delay = time2 - clockNow;
    if (delay > 24) {
      if (time2 < Infinity)
        timeout = setTimeout(wake, time2 - clock.now() - clockSkew);
      if (interval)
        interval = clearInterval(interval);
    } else {
      if (!interval)
        clockLast = clock.now(), interval = setInterval(poke, pokeDelay);
      frame = 1, setFrame(wake);
    }
  }

  // node_modules/d3-timer/src/timeout.js
  function timeout_default(callback, delay, time2) {
    var t = new Timer();
    delay = delay == null ? 0 : +delay;
    t.restart((elapsed) => {
      t.stop();
      callback(elapsed + delay);
    }, delay, time2);
    return t;
  }

  // node_modules/d3-transition/src/transition/schedule.js
  var emptyOn = dispatch_default("start", "end", "cancel", "interrupt");
  var emptyTween = [];
  var CREATED = 0;
  var SCHEDULED = 1;
  var STARTING = 2;
  var STARTED = 3;
  var RUNNING = 4;
  var ENDING = 5;
  var ENDED = 6;
  function schedule_default(node, name3, id3, index3, group3, timing) {
    var schedules = node.__transition;
    if (!schedules)
      node.__transition = {};
    else if (id3 in schedules)
      return;
    create(node, id3, {
      name: name3,
      index: index3,
      group: group3,
      on: emptyOn,
      tween: emptyTween,
      time: timing.time,
      delay: timing.delay,
      duration: timing.duration,
      ease: timing.ease,
      timer: null,
      state: CREATED
    });
  }
  function init(node, id3) {
    var schedule = get2(node, id3);
    if (schedule.state > CREATED)
      throw new Error("too late; already scheduled");
    return schedule;
  }
  function set2(node, id3) {
    var schedule = get2(node, id3);
    if (schedule.state > STARTED)
      throw new Error("too late; already running");
    return schedule;
  }
  function get2(node, id3) {
    var schedule = node.__transition;
    if (!schedule || !(schedule = schedule[id3]))
      throw new Error("transition not found");
    return schedule;
  }
  function create(node, id3, self) {
    var schedules = node.__transition, tween;
    schedules[id3] = self;
    self.timer = timer(schedule, 0, self.time);
    function schedule(elapsed) {
      self.state = SCHEDULED;
      self.timer.restart(start2, self.delay, self.time);
      if (self.delay <= elapsed)
        start2(elapsed - self.delay);
    }
    function start2(elapsed) {
      var i, j, n, o;
      if (self.state !== SCHEDULED)
        return stop();
      for (i in schedules) {
        o = schedules[i];
        if (o.name !== self.name)
          continue;
        if (o.state === STARTED)
          return timeout_default(start2);
        if (o.state === RUNNING) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("interrupt", node, node.__data__, o.index, o.group);
          delete schedules[i];
        } else if (+i < id3) {
          o.state = ENDED;
          o.timer.stop();
          o.on.call("cancel", node, node.__data__, o.index, o.group);
          delete schedules[i];
        }
      }
      timeout_default(function() {
        if (self.state === STARTED) {
          self.state = RUNNING;
          self.timer.restart(tick, self.delay, self.time);
          tick(elapsed);
        }
      });
      self.state = STARTING;
      self.on.call("start", node, node.__data__, self.index, self.group);
      if (self.state !== STARTING)
        return;
      self.state = STARTED;
      tween = new Array(n = self.tween.length);
      for (i = 0, j = -1; i < n; ++i) {
        if (o = self.tween[i].value.call(node, node.__data__, self.index, self.group)) {
          tween[++j] = o;
        }
      }
      tween.length = j + 1;
    }
    function tick(elapsed) {
      var t = elapsed < self.duration ? self.ease.call(null, elapsed / self.duration) : (self.timer.restart(stop), self.state = ENDING, 1), i = -1, n = tween.length;
      while (++i < n) {
        tween[i].call(node, t);
      }
      if (self.state === ENDING) {
        self.on.call("end", node, node.__data__, self.index, self.group);
        stop();
      }
    }
    function stop() {
      self.state = ENDED;
      self.timer.stop();
      delete schedules[id3];
      for (var i in schedules)
        return;
      delete node.__transition;
    }
  }

  // node_modules/d3-transition/src/interrupt.js
  function interrupt_default(node, name3) {
    var schedules = node.__transition, schedule, active, empty10 = true, i;
    if (!schedules)
      return;
    name3 = name3 == null ? null : name3 + "";
    for (i in schedules) {
      if ((schedule = schedules[i]).name !== name3) {
        empty10 = false;
        continue;
      }
      active = schedule.state > STARTING && schedule.state < ENDING;
      schedule.state = ENDED;
      schedule.timer.stop();
      schedule.on.call(active ? "interrupt" : "cancel", node, node.__data__, schedule.index, schedule.group);
      delete schedules[i];
    }
    if (empty10)
      delete node.__transition;
  }

  // node_modules/d3-transition/src/selection/interrupt.js
  function interrupt_default2(name3) {
    return this.each(function() {
      interrupt_default(this, name3);
    });
  }

  // node_modules/d3-transition/src/transition/tween.js
  function tweenRemove(id3, name3) {
    var tween0, tween1;
    return function() {
      var schedule = set2(this, id3), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = tween0 = tween;
        for (var i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name3) {
            tween1 = tween1.slice();
            tween1.splice(i, 1);
            break;
          }
        }
      }
      schedule.tween = tween1;
    };
  }
  function tweenFunction(id3, name3, value2) {
    var tween0, tween1;
    if (typeof value2 !== "function")
      throw new Error();
    return function() {
      var schedule = set2(this, id3), tween = schedule.tween;
      if (tween !== tween0) {
        tween1 = (tween0 = tween).slice();
        for (var t = { name: name3, value: value2 }, i = 0, n = tween1.length; i < n; ++i) {
          if (tween1[i].name === name3) {
            tween1[i] = t;
            break;
          }
        }
        if (i === n)
          tween1.push(t);
      }
      schedule.tween = tween1;
    };
  }
  function tween_default(name3, value2) {
    var id3 = this._id;
    name3 += "";
    if (arguments.length < 2) {
      var tween = get2(this.node(), id3).tween;
      for (var i = 0, n = tween.length, t; i < n; ++i) {
        if ((t = tween[i]).name === name3) {
          return t.value;
        }
      }
      return null;
    }
    return this.each((value2 == null ? tweenRemove : tweenFunction)(id3, name3, value2));
  }
  function tweenValue(transition2, name3, value2) {
    var id3 = transition2._id;
    transition2.each(function() {
      var schedule = set2(this, id3);
      (schedule.value || (schedule.value = {}))[name3] = value2.apply(this, arguments);
    });
    return function(node) {
      return get2(node, id3).value[name3];
    };
  }

  // node_modules/d3-transition/src/transition/interpolate.js
  function interpolate_default(a, b) {
    var c;
    return (typeof b === "number" ? number_default2 : b instanceof color ? rgb_default : (c = color(b)) ? (b = c, rgb_default) : string_default)(a, b);
  }

  // node_modules/d3-transition/src/transition/attr.js
  function attrRemove2(name3) {
    return function() {
      this.removeAttribute(name3);
    };
  }
  function attrRemoveNS2(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant2(name3, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttribute(name3);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrConstantNS2(fullname, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = this.getAttributeNS(fullname.space, fullname.local);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function attrFunction2(name3, interpolate, value2) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value2(this), string1;
      if (value1 == null)
        return void this.removeAttribute(name3);
      string0 = this.getAttribute(name3);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attrFunctionNS2(fullname, interpolate, value2) {
    var string00, string10, interpolate0;
    return function() {
      var string0, value1 = value2(this), string1;
      if (value1 == null)
        return void this.removeAttributeNS(fullname.space, fullname.local);
      string0 = this.getAttributeNS(fullname.space, fullname.local);
      string1 = value1 + "";
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function attr_default2(name3, value2) {
    var fullname = namespace_default(name3), i = fullname === "transform" ? interpolateTransformSvg : interpolate_default;
    return this.attrTween(name3, typeof value2 === "function" ? (fullname.local ? attrFunctionNS2 : attrFunction2)(fullname, i, tweenValue(this, "attr." + name3, value2)) : value2 == null ? (fullname.local ? attrRemoveNS2 : attrRemove2)(fullname) : (fullname.local ? attrConstantNS2 : attrConstant2)(fullname, i, value2));
  }

  // node_modules/d3-transition/src/transition/attrTween.js
  function attrInterpolate(name3, i) {
    return function(t) {
      this.setAttribute(name3, i.call(this, t));
    };
  }
  function attrInterpolateNS(fullname, i) {
    return function(t) {
      this.setAttributeNS(fullname.space, fullname.local, i.call(this, t));
    };
  }
  function attrTweenNS(fullname, value2) {
    var t0, i0;
    function tween() {
      var i = value2.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolateNS(fullname, i);
      return t0;
    }
    tween._value = value2;
    return tween;
  }
  function attrTween(name3, value2) {
    var t0, i0;
    function tween() {
      var i = value2.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && attrInterpolate(name3, i);
      return t0;
    }
    tween._value = value2;
    return tween;
  }
  function attrTween_default(name3, value2) {
    var key = "attr." + name3;
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value2 == null)
      return this.tween(key, null);
    if (typeof value2 !== "function")
      throw new Error();
    var fullname = namespace_default(name3);
    return this.tween(key, (fullname.local ? attrTweenNS : attrTween)(fullname, value2));
  }

  // node_modules/d3-transition/src/transition/delay.js
  function delayFunction(id3, value2) {
    return function() {
      init(this, id3).delay = +value2.apply(this, arguments);
    };
  }
  function delayConstant(id3, value2) {
    return value2 = +value2, function() {
      init(this, id3).delay = value2;
    };
  }
  function delay_default(value2) {
    var id3 = this._id;
    return arguments.length ? this.each((typeof value2 === "function" ? delayFunction : delayConstant)(id3, value2)) : get2(this.node(), id3).delay;
  }

  // node_modules/d3-transition/src/transition/duration.js
  function durationFunction(id3, value2) {
    return function() {
      set2(this, id3).duration = +value2.apply(this, arguments);
    };
  }
  function durationConstant(id3, value2) {
    return value2 = +value2, function() {
      set2(this, id3).duration = value2;
    };
  }
  function duration_default(value2) {
    var id3 = this._id;
    return arguments.length ? this.each((typeof value2 === "function" ? durationFunction : durationConstant)(id3, value2)) : get2(this.node(), id3).duration;
  }

  // node_modules/d3-transition/src/transition/ease.js
  function easeConstant(id3, value2) {
    if (typeof value2 !== "function")
      throw new Error();
    return function() {
      set2(this, id3).ease = value2;
    };
  }
  function ease_default(value2) {
    var id3 = this._id;
    return arguments.length ? this.each(easeConstant(id3, value2)) : get2(this.node(), id3).ease;
  }

  // node_modules/d3-transition/src/transition/easeVarying.js
  function easeVarying(id3, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (typeof v !== "function")
        throw new Error();
      set2(this, id3).ease = v;
    };
  }
  function easeVarying_default(value2) {
    if (typeof value2 !== "function")
      throw new Error();
    return this.each(easeVarying(this._id, value2));
  }

  // node_modules/d3-transition/src/transition/filter.js
  function filter_default2(match5) {
    if (typeof match5 !== "function")
      match5 = matcher_default(match5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group3[i]) && match5.call(node, node.__data__, i, group3)) {
          subgroup.push(node);
        }
      }
    }
    return new Transition(subgroups, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/merge.js
  function merge_default2(transition2) {
    if (transition2._id !== this._id)
      throw new Error();
    for (var groups0 = this._groups, groups1 = transition2._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Transition(merges, this._parents, this._name, this._id);
  }

  // node_modules/d3-transition/src/transition/on.js
  function start(name3) {
    return (name3 + "").trim().split(/^|\s+/).every(function(t) {
      var i = t.indexOf(".");
      if (i >= 0)
        t = t.slice(0, i);
      return !t || t === "start";
    });
  }
  function onFunction(id3, name3, listener) {
    var on0, on1, sit = start(name3) ? init : set2;
    return function() {
      var schedule = sit(this, id3), on2 = schedule.on;
      if (on2 !== on0)
        (on1 = (on0 = on2).copy()).on(name3, listener);
      schedule.on = on1;
    };
  }
  function on_default2(name3, listener) {
    var id3 = this._id;
    return arguments.length < 2 ? get2(this.node(), id3).on.on(name3) : this.each(onFunction(id3, name3, listener));
  }

  // node_modules/d3-transition/src/transition/remove.js
  function removeFunction(id3) {
    return function() {
      var parent = this.parentNode;
      for (var i in this.__transition)
        if (+i !== id3)
          return;
      if (parent)
        parent.removeChild(this);
    };
  }
  function remove_default2() {
    return this.on("end.remove", removeFunction(this._id));
  }

  // node_modules/d3-transition/src/transition/select.js
  function select_default3(select) {
    var name3 = this._name, id3 = this._id;
    if (typeof select !== "function")
      select = selector_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group3[i]) && (subnode = select.call(node, node.__data__, i, group3))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
          schedule_default(subgroup[i], name3, id3, i, subgroup, get2(node, id3));
        }
      }
    }
    return new Transition(subgroups, this._parents, name3, id3);
  }

  // node_modules/d3-transition/src/transition/selectAll.js
  function selectAll_default2(select) {
    var name3 = this._name, id3 = this._id;
    if (typeof select !== "function")
      select = selectorAll_default(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          for (var children2 = select.call(node, node.__data__, i, group3), child, inherit2 = get2(node, id3), k = 0, l = children2.length; k < l; ++k) {
            if (child = children2[k]) {
              schedule_default(child, name3, id3, k, children2, inherit2);
            }
          }
          subgroups.push(children2);
          parents.push(node);
        }
      }
    }
    return new Transition(subgroups, parents, name3, id3);
  }

  // node_modules/d3-transition/src/transition/selection.js
  var Selection2 = selection_default.prototype.constructor;
  function selection_default2() {
    return new Selection2(this._groups, this._parents);
  }

  // node_modules/d3-transition/src/transition/style.js
  function styleNull(name3, interpolate) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name3), string1 = (this.style.removeProperty(name3), styleValue(this, name3));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : interpolate0 = interpolate(string00 = string0, string10 = string1);
    };
  }
  function styleRemove2(name3) {
    return function() {
      this.style.removeProperty(name3);
    };
  }
  function styleConstant2(name3, interpolate, value1) {
    var string00, string1 = value1 + "", interpolate0;
    return function() {
      var string0 = styleValue(this, name3);
      return string0 === string1 ? null : string0 === string00 ? interpolate0 : interpolate0 = interpolate(string00 = string0, value1);
    };
  }
  function styleFunction2(name3, interpolate, value2) {
    var string00, string10, interpolate0;
    return function() {
      var string0 = styleValue(this, name3), value1 = value2(this), string1 = value1 + "";
      if (value1 == null)
        string1 = value1 = (this.style.removeProperty(name3), styleValue(this, name3));
      return string0 === string1 ? null : string0 === string00 && string1 === string10 ? interpolate0 : (string10 = string1, interpolate0 = interpolate(string00 = string0, value1));
    };
  }
  function styleMaybeRemove(id3, name3) {
    var on0, on1, listener0, key = "style." + name3, event2 = "end." + key, remove3;
    return function() {
      var schedule = set2(this, id3), on2 = schedule.on, listener = schedule.value[key] == null ? remove3 || (remove3 = styleRemove2(name3)) : void 0;
      if (on2 !== on0 || listener0 !== listener)
        (on1 = (on0 = on2).copy()).on(event2, listener0 = listener);
      schedule.on = on1;
    };
  }
  function style_default2(name3, value2, priority) {
    var i = (name3 += "") === "transform" ? interpolateTransformCss : interpolate_default;
    return value2 == null ? this.styleTween(name3, styleNull(name3, i)).on("end.style." + name3, styleRemove2(name3)) : typeof value2 === "function" ? this.styleTween(name3, styleFunction2(name3, i, tweenValue(this, "style." + name3, value2))).each(styleMaybeRemove(this._id, name3)) : this.styleTween(name3, styleConstant2(name3, i, value2), priority).on("end.style." + name3, null);
  }

  // node_modules/d3-transition/src/transition/styleTween.js
  function styleInterpolate(name3, i, priority) {
    return function(t) {
      this.style.setProperty(name3, i.call(this, t), priority);
    };
  }
  function styleTween(name3, value2, priority) {
    var t, i0;
    function tween() {
      var i = value2.apply(this, arguments);
      if (i !== i0)
        t = (i0 = i) && styleInterpolate(name3, i, priority);
      return t;
    }
    tween._value = value2;
    return tween;
  }
  function styleTween_default(name3, value2, priority) {
    var key = "style." + (name3 += "");
    if (arguments.length < 2)
      return (key = this.tween(key)) && key._value;
    if (value2 == null)
      return this.tween(key, null);
    if (typeof value2 !== "function")
      throw new Error();
    return this.tween(key, styleTween(name3, value2, priority == null ? "" : priority));
  }

  // node_modules/d3-transition/src/transition/text.js
  function textConstant2(value2) {
    return function() {
      this.textContent = value2;
    };
  }
  function textFunction2(value2) {
    return function() {
      var value1 = value2(this);
      this.textContent = value1 == null ? "" : value1;
    };
  }
  function text_default2(value2) {
    return this.tween("text", typeof value2 === "function" ? textFunction2(tweenValue(this, "text", value2)) : textConstant2(value2 == null ? "" : value2 + ""));
  }

  // node_modules/d3-transition/src/transition/textTween.js
  function textInterpolate(i) {
    return function(t) {
      this.textContent = i.call(this, t);
    };
  }
  function textTween(value2) {
    var t0, i0;
    function tween() {
      var i = value2.apply(this, arguments);
      if (i !== i0)
        t0 = (i0 = i) && textInterpolate(i);
      return t0;
    }
    tween._value = value2;
    return tween;
  }
  function textTween_default(value2) {
    var key = "text";
    if (arguments.length < 1)
      return (key = this.tween(key)) && key._value;
    if (value2 == null)
      return this.tween(key, null);
    if (typeof value2 !== "function")
      throw new Error();
    return this.tween(key, textTween(value2));
  }

  // node_modules/d3-transition/src/transition/transition.js
  function transition_default() {
    var name3 = this._name, id0 = this._id, id1 = newId();
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          var inherit2 = get2(node, id0);
          schedule_default(node, name3, id1, i, group3, {
            time: inherit2.time + inherit2.delay + inherit2.duration,
            delay: 0,
            duration: inherit2.duration,
            ease: inherit2.ease
          });
        }
      }
    }
    return new Transition(groups, this._parents, name3, id1);
  }

  // node_modules/d3-transition/src/transition/end.js
  function end_default() {
    var on0, on1, that = this, id3 = that._id, size3 = that.size();
    return new Promise(function(resolve, reject) {
      var cancel = { value: reject }, end = { value: function() {
        if (--size3 === 0)
          resolve();
      } };
      that.each(function() {
        var schedule = set2(this, id3), on2 = schedule.on;
        if (on2 !== on0) {
          on1 = (on0 = on2).copy();
          on1._.cancel.push(cancel);
          on1._.interrupt.push(cancel);
          on1._.end.push(end);
        }
        schedule.on = on1;
      });
      if (size3 === 0)
        resolve();
    });
  }

  // node_modules/d3-transition/src/transition/index.js
  var id = 0;
  function Transition(groups, parents, name3, id3) {
    this._groups = groups;
    this._parents = parents;
    this._name = name3;
    this._id = id3;
  }
  function transition(name3) {
    return selection_default().transition(name3);
  }
  function newId() {
    return ++id;
  }
  var selection_prototype = selection_default.prototype;
  Transition.prototype = transition.prototype = {
    constructor: Transition,
    select: select_default3,
    selectAll: selectAll_default2,
    filter: filter_default2,
    merge: merge_default2,
    selection: selection_default2,
    transition: transition_default,
    call: selection_prototype.call,
    nodes: selection_prototype.nodes,
    node: selection_prototype.node,
    size: selection_prototype.size,
    empty: selection_prototype.empty,
    each: selection_prototype.each,
    on: on_default2,
    attr: attr_default2,
    attrTween: attrTween_default,
    style: style_default2,
    styleTween: styleTween_default,
    text: text_default2,
    textTween: textTween_default,
    remove: remove_default2,
    tween: tween_default,
    delay: delay_default,
    duration: duration_default,
    ease: ease_default,
    easeVarying: easeVarying_default,
    end: end_default,
    [Symbol.iterator]: selection_prototype[Symbol.iterator]
  };

  // node_modules/d3-ease/src/cubic.js
  function cubicInOut(t) {
    return ((t *= 2) <= 1 ? t * t * t : (t -= 2) * t * t + 2) / 2;
  }

  // node_modules/d3-transition/src/selection/transition.js
  var defaultTiming = {
    time: null,
    delay: 0,
    duration: 250,
    ease: cubicInOut
  };
  function inherit(node, id3) {
    var timing;
    while (!(timing = node.__transition) || !(timing = timing[id3])) {
      if (!(node = node.parentNode)) {
        throw new Error(`transition ${id3} not found`);
      }
    }
    return timing;
  }
  function transition_default2(name3) {
    var id3, timing;
    if (name3 instanceof Transition) {
      id3 = name3._id, name3 = name3._name;
    } else {
      id3 = newId(), (timing = defaultTiming).time = now(), name3 = name3 == null ? null : name3 + "";
    }
    for (var groups = this._groups, m = groups.length, j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          schedule_default(node, name3, id3, i, group3, timing || inherit(node, id3));
        }
      }
    }
    return new Transition(groups, this._parents, name3, id3);
  }

  // node_modules/d3-transition/src/selection/index.js
  selection_default.prototype.interrupt = interrupt_default2;
  selection_default.prototype.transition = transition_default2;

  // node_modules/d3-brush/src/brush.js
  var { abs, max, min } = Math;
  function number1(e) {
    return [+e[0], +e[1]];
  }
  function number2(e) {
    return [number1(e[0]), number1(e[1])];
  }
  var X = {
    name: "x",
    handles: ["w", "e"].map(type),
    input: function(x2, e) {
      return x2 == null ? null : [[+x2[0], e[0][1]], [+x2[1], e[1][1]]];
    },
    output: function(xy) {
      return xy && [xy[0][0], xy[1][0]];
    }
  };
  var Y = {
    name: "y",
    handles: ["n", "s"].map(type),
    input: function(y2, e) {
      return y2 == null ? null : [[e[0][0], +y2[0]], [e[1][0], +y2[1]]];
    },
    output: function(xy) {
      return xy && [xy[0][1], xy[1][1]];
    }
  };
  var XY = {
    name: "xy",
    handles: ["n", "w", "e", "s", "nw", "ne", "sw", "se"].map(type),
    input: function(xy) {
      return xy == null ? null : number2(xy);
    },
    output: function(xy) {
      return xy;
    }
  };
  function type(t) {
    return { type: t };
  }

  // node_modules/d3-path/src/path.js
  var pi = Math.PI;
  var tau = 2 * pi;
  var epsilon2 = 1e-6;
  var tauEpsilon = tau - epsilon2;
  function Path() {
    this._x0 = this._y0 = this._x1 = this._y1 = null;
    this._ = "";
  }
  function path() {
    return new Path();
  }
  Path.prototype = path.prototype = {
    constructor: Path,
    moveTo: function(x2, y2) {
      this._ += "M" + (this._x0 = this._x1 = +x2) + "," + (this._y0 = this._y1 = +y2);
    },
    closePath: function() {
      if (this._x1 !== null) {
        this._x1 = this._x0, this._y1 = this._y0;
        this._ += "Z";
      }
    },
    lineTo: function(x2, y2) {
      this._ += "L" + (this._x1 = +x2) + "," + (this._y1 = +y2);
    },
    quadraticCurveTo: function(x1, y1, x2, y2) {
      this._ += "Q" + +x1 + "," + +y1 + "," + (this._x1 = +x2) + "," + (this._y1 = +y2);
    },
    bezierCurveTo: function(x1, y1, x2, y2, x3, y3) {
      this._ += "C" + +x1 + "," + +y1 + "," + +x2 + "," + +y2 + "," + (this._x1 = +x3) + "," + (this._y1 = +y3);
    },
    arcTo: function(x1, y1, x2, y2, r) {
      x1 = +x1, y1 = +y1, x2 = +x2, y2 = +y2, r = +r;
      var x0 = this._x1, y0 = this._y1, x21 = x2 - x1, y21 = y2 - y1, x01 = x0 - x1, y01 = y0 - y1, l01_2 = x01 * x01 + y01 * y01;
      if (r < 0)
        throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else if (!(l01_2 > epsilon2))
        ;
      else if (!(Math.abs(y01 * x21 - y21 * x01) > epsilon2) || !r) {
        this._ += "L" + (this._x1 = x1) + "," + (this._y1 = y1);
      } else {
        var x20 = x2 - x0, y20 = y2 - y0, l21_2 = x21 * x21 + y21 * y21, l20_2 = x20 * x20 + y20 * y20, l21 = Math.sqrt(l21_2), l01 = Math.sqrt(l01_2), l = r * Math.tan((pi - Math.acos((l21_2 + l01_2 - l20_2) / (2 * l21 * l01))) / 2), t01 = l / l01, t21 = l / l21;
        if (Math.abs(t01 - 1) > epsilon2) {
          this._ += "L" + (x1 + t01 * x01) + "," + (y1 + t01 * y01);
        }
        this._ += "A" + r + "," + r + ",0,0," + +(y01 * x20 > x01 * y20) + "," + (this._x1 = x1 + t21 * x21) + "," + (this._y1 = y1 + t21 * y21);
      }
    },
    arc: function(x2, y2, r, a0, a1, ccw) {
      x2 = +x2, y2 = +y2, r = +r, ccw = !!ccw;
      var dx = r * Math.cos(a0), dy = r * Math.sin(a0), x0 = x2 + dx, y0 = y2 + dy, cw = 1 ^ ccw, da = ccw ? a0 - a1 : a1 - a0;
      if (r < 0)
        throw new Error("negative radius: " + r);
      if (this._x1 === null) {
        this._ += "M" + x0 + "," + y0;
      } else if (Math.abs(this._x1 - x0) > epsilon2 || Math.abs(this._y1 - y0) > epsilon2) {
        this._ += "L" + x0 + "," + y0;
      }
      if (!r)
        return;
      if (da < 0)
        da = da % tau + tau;
      if (da > tauEpsilon) {
        this._ += "A" + r + "," + r + ",0,1," + cw + "," + (x2 - dx) + "," + (y2 - dy) + "A" + r + "," + r + ",0,1," + cw + "," + (this._x1 = x0) + "," + (this._y1 = y0);
      } else if (da > epsilon2) {
        this._ += "A" + r + "," + r + ",0," + +(da >= pi) + "," + cw + "," + (this._x1 = x2 + r * Math.cos(a1)) + "," + (this._y1 = y2 + r * Math.sin(a1));
      }
    },
    rect: function(x2, y2, w, h) {
      this._ += "M" + (this._x0 = this._x1 = +x2) + "," + (this._y0 = this._y1 = +y2) + "h" + +w + "v" + +h + "h" + -w + "Z";
    },
    toString: function() {
      return this._;
    }
  };
  var path_default = path;

  // node_modules/d3-format/src/formatDecimal.js
  function formatDecimal_default(x2) {
    return Math.abs(x2 = Math.round(x2)) >= 1e21 ? x2.toLocaleString("en").replace(/,/g, "") : x2.toString(10);
  }
  function formatDecimalParts(x2, p) {
    if ((i = (x2 = p ? x2.toExponential(p - 1) : x2.toExponential()).indexOf("e")) < 0)
      return null;
    var i, coefficient = x2.slice(0, i);
    return [
      coefficient.length > 1 ? coefficient[0] + coefficient.slice(2) : coefficient,
      +x2.slice(i + 1)
    ];
  }

  // node_modules/d3-format/src/exponent.js
  function exponent_default(x2) {
    return x2 = formatDecimalParts(Math.abs(x2)), x2 ? x2[1] : NaN;
  }

  // node_modules/d3-format/src/formatGroup.js
  function formatGroup_default(grouping, thousands) {
    return function(value2, width) {
      var i = value2.length, t = [], j = 0, g = grouping[0], length9 = 0;
      while (i > 0 && g > 0) {
        if (length9 + g + 1 > width)
          g = Math.max(1, width - length9);
        t.push(value2.substring(i -= g, i + g));
        if ((length9 += g + 1) > width)
          break;
        g = grouping[j = (j + 1) % grouping.length];
      }
      return t.reverse().join(thousands);
    };
  }

  // node_modules/d3-format/src/formatNumerals.js
  function formatNumerals_default(numerals) {
    return function(value2) {
      return value2.replace(/[0-9]/g, function(i) {
        return numerals[+i];
      });
    };
  }

  // node_modules/d3-format/src/formatSpecifier.js
  var re = /^(?:(.)?([<>=^]))?([+\-( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?(~)?([a-z%])?$/i;
  function formatSpecifier(specifier) {
    if (!(match5 = re.exec(specifier)))
      throw new Error("invalid format: " + specifier);
    var match5;
    return new FormatSpecifier({
      fill: match5[1],
      align: match5[2],
      sign: match5[3],
      symbol: match5[4],
      zero: match5[5],
      width: match5[6],
      comma: match5[7],
      precision: match5[8] && match5[8].slice(1),
      trim: match5[9],
      type: match5[10]
    });
  }
  formatSpecifier.prototype = FormatSpecifier.prototype;
  function FormatSpecifier(specifier) {
    this.fill = specifier.fill === void 0 ? " " : specifier.fill + "";
    this.align = specifier.align === void 0 ? ">" : specifier.align + "";
    this.sign = specifier.sign === void 0 ? "-" : specifier.sign + "";
    this.symbol = specifier.symbol === void 0 ? "" : specifier.symbol + "";
    this.zero = !!specifier.zero;
    this.width = specifier.width === void 0 ? void 0 : +specifier.width;
    this.comma = !!specifier.comma;
    this.precision = specifier.precision === void 0 ? void 0 : +specifier.precision;
    this.trim = !!specifier.trim;
    this.type = specifier.type === void 0 ? "" : specifier.type + "";
  }
  FormatSpecifier.prototype.toString = function() {
    return this.fill + this.align + this.sign + this.symbol + (this.zero ? "0" : "") + (this.width === void 0 ? "" : Math.max(1, this.width | 0)) + (this.comma ? "," : "") + (this.precision === void 0 ? "" : "." + Math.max(0, this.precision | 0)) + (this.trim ? "~" : "") + this.type;
  };

  // node_modules/d3-format/src/formatTrim.js
  function formatTrim_default(s) {
    out:
      for (var n = s.length, i = 1, i0 = -1, i1; i < n; ++i) {
        switch (s[i]) {
          case ".":
            i0 = i1 = i;
            break;
          case "0":
            if (i0 === 0)
              i0 = i;
            i1 = i;
            break;
          default:
            if (!+s[i])
              break out;
            if (i0 > 0)
              i0 = 0;
            break;
        }
      }
    return i0 > 0 ? s.slice(0, i0) + s.slice(i1 + 1) : s;
  }

  // node_modules/d3-format/src/formatPrefixAuto.js
  var prefixExponent;
  function formatPrefixAuto_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1], i = exponent - (prefixExponent = Math.max(-8, Math.min(8, Math.floor(exponent / 3))) * 3) + 1, n = coefficient.length;
    return i === n ? coefficient : i > n ? coefficient + new Array(i - n + 1).join("0") : i > 0 ? coefficient.slice(0, i) + "." + coefficient.slice(i) : "0." + new Array(1 - i).join("0") + formatDecimalParts(x2, Math.max(0, p + i - 1))[0];
  }

  // node_modules/d3-format/src/formatRounded.js
  function formatRounded_default(x2, p) {
    var d = formatDecimalParts(x2, p);
    if (!d)
      return x2 + "";
    var coefficient = d[0], exponent = d[1];
    return exponent < 0 ? "0." + new Array(-exponent).join("0") + coefficient : coefficient.length > exponent + 1 ? coefficient.slice(0, exponent + 1) + "." + coefficient.slice(exponent + 1) : coefficient + new Array(exponent - coefficient.length + 2).join("0");
  }

  // node_modules/d3-format/src/formatTypes.js
  var formatTypes_default = {
    "%": (x2, p) => (x2 * 100).toFixed(p),
    "b": (x2) => Math.round(x2).toString(2),
    "c": (x2) => x2 + "",
    "d": formatDecimal_default,
    "e": (x2, p) => x2.toExponential(p),
    "f": (x2, p) => x2.toFixed(p),
    "g": (x2, p) => x2.toPrecision(p),
    "o": (x2) => Math.round(x2).toString(8),
    "p": (x2, p) => formatRounded_default(x2 * 100, p),
    "r": formatRounded_default,
    "s": formatPrefixAuto_default,
    "X": (x2) => Math.round(x2).toString(16).toUpperCase(),
    "x": (x2) => Math.round(x2).toString(16)
  };

  // node_modules/d3-format/src/identity.js
  function identity_default2(x2) {
    return x2;
  }

  // node_modules/d3-format/src/locale.js
  var map = Array.prototype.map;
  var prefixes = ["y", "z", "a", "f", "p", "n", "\xB5", "m", "", "k", "M", "G", "T", "P", "E", "Z", "Y"];
  function locale_default(locale2) {
    var group3 = locale2.grouping === void 0 || locale2.thousands === void 0 ? identity_default2 : formatGroup_default(map.call(locale2.grouping, Number), locale2.thousands + ""), currencyPrefix = locale2.currency === void 0 ? "" : locale2.currency[0] + "", currencySuffix = locale2.currency === void 0 ? "" : locale2.currency[1] + "", decimal = locale2.decimal === void 0 ? "." : locale2.decimal + "", numerals = locale2.numerals === void 0 ? identity_default2 : formatNumerals_default(map.call(locale2.numerals, String)), percent = locale2.percent === void 0 ? "%" : locale2.percent + "", minus2 = locale2.minus === void 0 ? "\u2212" : locale2.minus + "", nan2 = locale2.nan === void 0 ? "NaN" : locale2.nan + "";
    function newFormat(specifier) {
      specifier = formatSpecifier(specifier);
      var fill = specifier.fill, align = specifier.align, sign2 = specifier.sign, symbol = specifier.symbol, zero3 = specifier.zero, width = specifier.width, comma2 = specifier.comma, precision = specifier.precision, trim2 = specifier.trim, type2 = specifier.type;
      if (type2 === "n")
        comma2 = true, type2 = "g";
      else if (!formatTypes_default[type2])
        precision === void 0 && (precision = 12), trim2 = true, type2 = "g";
      if (zero3 || fill === "0" && align === "=")
        zero3 = true, fill = "0", align = "=";
      var prefix2 = symbol === "$" ? currencyPrefix : symbol === "#" && /[boxX]/.test(type2) ? "0" + type2.toLowerCase() : "", suffix = symbol === "$" ? currencySuffix : /[%p]/.test(type2) ? percent : "";
      var formatType = formatTypes_default[type2], maybeSuffix = /[defgprs%]/.test(type2);
      precision = precision === void 0 ? 6 : /[gprs]/.test(type2) ? Math.max(1, Math.min(21, precision)) : Math.max(0, Math.min(20, precision));
      function format2(value2) {
        var valuePrefix = prefix2, valueSuffix = suffix, i, n, c;
        if (type2 === "c") {
          valueSuffix = formatType(value2) + valueSuffix;
          value2 = "";
        } else {
          value2 = +value2;
          var valueNegative = value2 < 0 || 1 / value2 < 0;
          value2 = isNaN(value2) ? nan2 : formatType(Math.abs(value2), precision);
          if (trim2)
            value2 = formatTrim_default(value2);
          if (valueNegative && +value2 === 0 && sign2 !== "+")
            valueNegative = false;
          valuePrefix = (valueNegative ? sign2 === "(" ? sign2 : minus2 : sign2 === "-" || sign2 === "(" ? "" : sign2) + valuePrefix;
          valueSuffix = (type2 === "s" ? prefixes[8 + prefixExponent / 3] : "") + valueSuffix + (valueNegative && sign2 === "(" ? ")" : "");
          if (maybeSuffix) {
            i = -1, n = value2.length;
            while (++i < n) {
              if (c = value2.charCodeAt(i), 48 > c || c > 57) {
                valueSuffix = (c === 46 ? decimal + value2.slice(i + 1) : value2.slice(i)) + valueSuffix;
                value2 = value2.slice(0, i);
                break;
              }
            }
          }
        }
        if (comma2 && !zero3)
          value2 = group3(value2, Infinity);
        var length9 = valuePrefix.length + value2.length + valueSuffix.length, padding = length9 < width ? new Array(width - length9 + 1).join(fill) : "";
        if (comma2 && zero3)
          value2 = group3(padding + value2, padding.length ? width - valueSuffix.length : Infinity), padding = "";
        switch (align) {
          case "<":
            value2 = valuePrefix + value2 + valueSuffix + padding;
            break;
          case "=":
            value2 = valuePrefix + padding + value2 + valueSuffix;
            break;
          case "^":
            value2 = padding.slice(0, length9 = padding.length >> 1) + valuePrefix + value2 + valueSuffix + padding.slice(length9);
            break;
          default:
            value2 = padding + valuePrefix + value2 + valueSuffix;
            break;
        }
        return numerals(value2);
      }
      format2.toString = function() {
        return specifier + "";
      };
      return format2;
    }
    function formatPrefix2(specifier, value2) {
      var f = newFormat((specifier = formatSpecifier(specifier), specifier.type = "f", specifier)), e = Math.max(-8, Math.min(8, Math.floor(exponent_default(value2) / 3))) * 3, k = Math.pow(10, -e), prefix2 = prefixes[8 + e / 3];
      return function(value3) {
        return f(k * value3) + prefix2;
      };
    }
    return {
      format: newFormat,
      formatPrefix: formatPrefix2
    };
  }

  // node_modules/d3-format/src/defaultLocale.js
  var locale;
  var format;
  var formatPrefix;
  defaultLocale({
    thousands: ",",
    grouping: [3],
    currency: ["$", ""]
  });
  function defaultLocale(definition) {
    locale = locale_default(definition);
    format = locale.format;
    formatPrefix = locale.formatPrefix;
    return locale;
  }

  // node_modules/d3-format/src/precisionFixed.js
  function precisionFixed_default(step2) {
    return Math.max(0, -exponent_default(Math.abs(step2)));
  }

  // node_modules/d3-format/src/precisionPrefix.js
  function precisionPrefix_default(step2, value2) {
    return Math.max(0, Math.max(-8, Math.min(8, Math.floor(exponent_default(value2) / 3))) * 3 - exponent_default(Math.abs(step2)));
  }

  // node_modules/d3-format/src/precisionRound.js
  function precisionRound_default(step2, max5) {
    step2 = Math.abs(step2), max5 = Math.abs(max5) - step2;
    return Math.max(0, exponent_default(max5) - exponent_default(step2)) + 1;
  }

  // node_modules/d3-scale/src/init.js
  function initRange(domain, range4) {
    switch (arguments.length) {
      case 0:
        break;
      case 1:
        this.range(domain);
        break;
      default:
        this.range(range4).domain(domain);
        break;
    }
    return this;
  }

  // node_modules/d3-scale/src/ordinal.js
  var implicit = Symbol("implicit");
  function ordinal() {
    var index3 = /* @__PURE__ */ new Map(), domain = [], range4 = [], unknown = implicit;
    function scale(d) {
      var key = d + "", i = index3.get(key);
      if (!i) {
        if (unknown !== implicit)
          return unknown;
        index3.set(key, i = domain.push(d));
      }
      return range4[(i - 1) % range4.length];
    }
    scale.domain = function(_) {
      if (!arguments.length)
        return domain.slice();
      domain = [], index3 = /* @__PURE__ */ new Map();
      for (const value2 of _) {
        const key = value2 + "";
        if (index3.has(key))
          continue;
        index3.set(key, domain.push(value2));
      }
      return scale;
    };
    scale.range = function(_) {
      return arguments.length ? (range4 = Array.from(_), scale) : range4.slice();
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    scale.copy = function() {
      return ordinal(domain, range4).unknown(unknown);
    };
    initRange.apply(scale, arguments);
    return scale;
  }

  // node_modules/d3-scale/src/band.js
  function band() {
    var scale = ordinal().unknown(void 0), domain = scale.domain, ordinalRange = scale.range, r0 = 0, r1 = 1, step2, bandwidth, round2 = false, paddingInner = 0, paddingOuter = 0, align = 0.5;
    delete scale.unknown;
    function rescale() {
      var n = domain().length, reverse3 = r1 < r0, start2 = reverse3 ? r1 : r0, stop = reverse3 ? r0 : r1;
      step2 = (stop - start2) / Math.max(1, n - paddingInner + paddingOuter * 2);
      if (round2)
        step2 = Math.floor(step2);
      start2 += (stop - start2 - step2 * (n - paddingInner)) * align;
      bandwidth = step2 * (1 - paddingInner);
      if (round2)
        start2 = Math.round(start2), bandwidth = Math.round(bandwidth);
      var values2 = range_default(n).map(function(i) {
        return start2 + step2 * i;
      });
      return ordinalRange(reverse3 ? values2.reverse() : values2);
    }
    scale.domain = function(_) {
      return arguments.length ? (domain(_), rescale()) : domain();
    };
    scale.range = function(_) {
      return arguments.length ? ([r0, r1] = _, r0 = +r0, r1 = +r1, rescale()) : [r0, r1];
    };
    scale.rangeRound = function(_) {
      return [r0, r1] = _, r0 = +r0, r1 = +r1, round2 = true, rescale();
    };
    scale.bandwidth = function() {
      return bandwidth;
    };
    scale.step = function() {
      return step2;
    };
    scale.round = function(_) {
      return arguments.length ? (round2 = !!_, rescale()) : round2;
    };
    scale.padding = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, paddingOuter = +_), rescale()) : paddingInner;
    };
    scale.paddingInner = function(_) {
      return arguments.length ? (paddingInner = Math.min(1, _), rescale()) : paddingInner;
    };
    scale.paddingOuter = function(_) {
      return arguments.length ? (paddingOuter = +_, rescale()) : paddingOuter;
    };
    scale.align = function(_) {
      return arguments.length ? (align = Math.max(0, Math.min(1, _)), rescale()) : align;
    };
    scale.copy = function() {
      return band(domain(), [r0, r1]).round(round2).paddingInner(paddingInner).paddingOuter(paddingOuter).align(align);
    };
    return initRange.apply(rescale(), arguments);
  }

  // node_modules/d3-scale/src/constant.js
  function constants(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-scale/src/number.js
  function number3(x2) {
    return +x2;
  }

  // node_modules/d3-scale/src/continuous.js
  var unit = [0, 1];
  function identity2(x2) {
    return x2;
  }
  function normalize(a, b) {
    return (b -= a = +a) ? function(x2) {
      return (x2 - a) / b;
    } : constants(isNaN(b) ? NaN : 0.5);
  }
  function clamper(a, b) {
    var t;
    if (a > b)
      t = a, a = b, b = t;
    return function(x2) {
      return Math.max(a, Math.min(b, x2));
    };
  }
  function bimap(domain, range4, interpolate) {
    var d0 = domain[0], d1 = domain[1], r0 = range4[0], r1 = range4[1];
    if (d1 < d0)
      d0 = normalize(d1, d0), r0 = interpolate(r1, r0);
    else
      d0 = normalize(d0, d1), r0 = interpolate(r0, r1);
    return function(x2) {
      return r0(d0(x2));
    };
  }
  function polymap(domain, range4, interpolate) {
    var j = Math.min(domain.length, range4.length) - 1, d = new Array(j), r = new Array(j), i = -1;
    if (domain[j] < domain[0]) {
      domain = domain.slice().reverse();
      range4 = range4.slice().reverse();
    }
    while (++i < j) {
      d[i] = normalize(domain[i], domain[i + 1]);
      r[i] = interpolate(range4[i], range4[i + 1]);
    }
    return function(x2) {
      var i2 = bisect_default(domain, x2, 1, j) - 1;
      return r[i2](d[i2](x2));
    };
  }
  function copy(source2, target2) {
    return target2.domain(source2.domain()).range(source2.range()).interpolate(source2.interpolate()).clamp(source2.clamp()).unknown(source2.unknown());
  }
  function transformer() {
    var domain = unit, range4 = unit, interpolate = value_default, transform2, untransform, unknown, clamp = identity2, piecewise, output, input;
    function rescale() {
      var n = Math.min(domain.length, range4.length);
      if (clamp !== identity2)
        clamp = clamper(domain[0], domain[n - 1]);
      piecewise = n > 2 ? polymap : bimap;
      output = input = null;
      return scale;
    }
    function scale(x2) {
      return x2 == null || isNaN(x2 = +x2) ? unknown : (output || (output = piecewise(domain.map(transform2), range4, interpolate)))(transform2(clamp(x2)));
    }
    scale.invert = function(y2) {
      return clamp(untransform((input || (input = piecewise(range4, domain.map(transform2), number_default2)))(y2)));
    };
    scale.domain = function(_) {
      return arguments.length ? (domain = Array.from(_, number3), rescale()) : domain.slice();
    };
    scale.range = function(_) {
      return arguments.length ? (range4 = Array.from(_), rescale()) : range4.slice();
    };
    scale.rangeRound = function(_) {
      return range4 = Array.from(_), interpolate = round_default, rescale();
    };
    scale.clamp = function(_) {
      return arguments.length ? (clamp = _ ? true : identity2, rescale()) : clamp !== identity2;
    };
    scale.interpolate = function(_) {
      return arguments.length ? (interpolate = _, rescale()) : interpolate;
    };
    scale.unknown = function(_) {
      return arguments.length ? (unknown = _, scale) : unknown;
    };
    return function(t, u) {
      transform2 = t, untransform = u;
      return rescale();
    };
  }
  function continuous() {
    return transformer()(identity2, identity2);
  }

  // node_modules/d3-scale/src/tickFormat.js
  function tickFormat(start2, stop, count, specifier) {
    var step2 = tickStep(start2, stop, count), precision;
    specifier = formatSpecifier(specifier == null ? ",f" : specifier);
    switch (specifier.type) {
      case "s": {
        var value2 = Math.max(Math.abs(start2), Math.abs(stop));
        if (specifier.precision == null && !isNaN(precision = precisionPrefix_default(step2, value2)))
          specifier.precision = precision;
        return formatPrefix(specifier, value2);
      }
      case "":
      case "e":
      case "g":
      case "p":
      case "r": {
        if (specifier.precision == null && !isNaN(precision = precisionRound_default(step2, Math.max(Math.abs(start2), Math.abs(stop)))))
          specifier.precision = precision - (specifier.type === "e");
        break;
      }
      case "f":
      case "%": {
        if (specifier.precision == null && !isNaN(precision = precisionFixed_default(step2)))
          specifier.precision = precision - (specifier.type === "%") * 2;
        break;
      }
    }
    return format(specifier);
  }

  // node_modules/d3-scale/src/linear.js
  function linearish(scale) {
    var domain = scale.domain;
    scale.ticks = function(count) {
      var d = domain();
      return ticks_default(d[0], d[d.length - 1], count == null ? 10 : count);
    };
    scale.tickFormat = function(count, specifier) {
      var d = domain();
      return tickFormat(d[0], d[d.length - 1], count == null ? 10 : count, specifier);
    };
    scale.nice = function(count) {
      if (count == null)
        count = 10;
      var d = domain();
      var i0 = 0;
      var i1 = d.length - 1;
      var start2 = d[i0];
      var stop = d[i1];
      var prestep;
      var step2;
      var maxIter = 10;
      if (stop < start2) {
        step2 = start2, start2 = stop, stop = step2;
        step2 = i0, i0 = i1, i1 = step2;
      }
      while (maxIter-- > 0) {
        step2 = tickIncrement(start2, stop, count);
        if (step2 === prestep) {
          d[i0] = start2;
          d[i1] = stop;
          return domain(d);
        } else if (step2 > 0) {
          start2 = Math.floor(start2 / step2) * step2;
          stop = Math.ceil(stop / step2) * step2;
        } else if (step2 < 0) {
          start2 = Math.ceil(start2 * step2) / step2;
          stop = Math.floor(stop * step2) / step2;
        } else {
          break;
        }
        prestep = step2;
      }
      return scale;
    };
    return scale;
  }
  function linear2() {
    var scale = continuous();
    scale.copy = function() {
      return copy(scale, linear2());
    };
    initRange.apply(scale, arguments);
    return linearish(scale);
  }

  // node_modules/d3-scale-chromatic/src/colors.js
  function colors_default(specifier) {
    var n = specifier.length / 6 | 0, colors = new Array(n), i = 0;
    while (i < n)
      colors[i] = "#" + specifier.slice(i * 6, ++i * 6);
    return colors;
  }

  // node_modules/d3-scale-chromatic/src/categorical/Pastel1.js
  var Pastel1_default = colors_default("fbb4aeb3cde3ccebc5decbe4fed9a6ffffcce5d8bdfddaecf2f2f2");

  // node_modules/d3-shape/src/constant.js
  function constant_default4(x2) {
    return function constant() {
      return x2;
    };
  }

  // node_modules/d3-shape/src/array.js
  var slice2 = Array.prototype.slice;
  function array_default2(x2) {
    return typeof x2 === "object" && "length" in x2 ? x2 : Array.from(x2);
  }

  // node_modules/d3-shape/src/curve/linear.js
  function Linear(context) {
    this._context = context;
  }
  Linear.prototype = {
    areaStart: function() {
      this._line = 0;
    },
    areaEnd: function() {
      this._line = NaN;
    },
    lineStart: function() {
      this._point = 0;
    },
    lineEnd: function() {
      if (this._line || this._line !== 0 && this._point === 1)
        this._context.closePath();
      this._line = 1 - this._line;
    },
    point: function(x2, y2) {
      x2 = +x2, y2 = +y2;
      switch (this._point) {
        case 0:
          this._point = 1;
          this._line ? this._context.lineTo(x2, y2) : this._context.moveTo(x2, y2);
          break;
        case 1:
          this._point = 2;
        default:
          this._context.lineTo(x2, y2);
          break;
      }
    }
  };
  function linear_default(context) {
    return new Linear(context);
  }

  // node_modules/d3-shape/src/point.js
  function x(p) {
    return p[0];
  }
  function y(p) {
    return p[1];
  }

  // node_modules/d3-shape/src/line.js
  function line_default(x2, y2) {
    var defined = constant_default4(true), context = null, curve = linear_default, output = null;
    x2 = typeof x2 === "function" ? x2 : x2 === void 0 ? x : constant_default4(x2);
    y2 = typeof y2 === "function" ? y2 : y2 === void 0 ? y : constant_default4(y2);
    function line(data) {
      var i, n = (data = array_default2(data)).length, d, defined0 = false, buffer;
      if (context == null)
        output = curve(buffer = path_default());
      for (i = 0; i <= n; ++i) {
        if (!(i < n && defined(d = data[i], i, data)) === defined0) {
          if (defined0 = !defined0)
            output.lineStart();
          else
            output.lineEnd();
        }
        if (defined0)
          output.point(+x2(d, i, data), +y2(d, i, data));
      }
      if (buffer)
        return output = null, buffer + "" || null;
    }
    line.x = function(_) {
      return arguments.length ? (x2 = typeof _ === "function" ? _ : constant_default4(+_), line) : x2;
    };
    line.y = function(_) {
      return arguments.length ? (y2 = typeof _ === "function" ? _ : constant_default4(+_), line) : y2;
    };
    line.defined = function(_) {
      return arguments.length ? (defined = typeof _ === "function" ? _ : constant_default4(!!_), line) : defined;
    };
    line.curve = function(_) {
      return arguments.length ? (curve = _, context != null && (output = curve(context)), line) : curve;
    };
    line.context = function(_) {
      return arguments.length ? (_ == null ? context = output = null : output = curve(context = _), line) : context;
    };
    return line;
  }

  // node_modules/d3-zoom/src/transform.js
  function Transform(k, x2, y2) {
    this.k = k;
    this.x = x2;
    this.y = y2;
  }
  Transform.prototype = {
    constructor: Transform,
    scale: function(k) {
      return k === 1 ? this : new Transform(this.k * k, this.x, this.y);
    },
    translate: function(x2, y2) {
      return x2 === 0 & y2 === 0 ? this : new Transform(this.k, this.x + this.k * x2, this.y + this.k * y2);
    },
    apply: function(point2) {
      return [point2[0] * this.k + this.x, point2[1] * this.k + this.y];
    },
    applyX: function(x2) {
      return x2 * this.k + this.x;
    },
    applyY: function(y2) {
      return y2 * this.k + this.y;
    },
    invert: function(location) {
      return [(location[0] - this.x) / this.k, (location[1] - this.y) / this.k];
    },
    invertX: function(x2) {
      return (x2 - this.x) / this.k;
    },
    invertY: function(y2) {
      return (y2 - this.y) / this.k;
    },
    rescaleX: function(x2) {
      return x2.copy().domain(x2.range().map(this.invertX, this).map(x2.invert, x2));
    },
    rescaleY: function(y2) {
      return y2.copy().domain(y2.range().map(this.invertY, this).map(y2.invert, y2));
    },
    toString: function() {
      return "translate(" + this.x + "," + this.y + ") scale(" + this.k + ")";
    }
  };
  var identity3 = new Transform(1, 0, 0);
  transform.prototype = Transform.prototype;
  function transform(node) {
    while (!node.__zoom)
      if (!(node = node.parentNode))
        return identity3;
    return node.__zoom;
  }

  // node_modules/d3-collection/src/map.js
  var prefix = "$";
  function Map2() {
  }
  Map2.prototype = map2.prototype = {
    constructor: Map2,
    has: function(key) {
      return prefix + key in this;
    },
    get: function(key) {
      return this[prefix + key];
    },
    set: function(key, value2) {
      this[prefix + key] = value2;
      return this;
    },
    remove: function(key) {
      var property = prefix + key;
      return property in this && delete this[property];
    },
    clear: function() {
      for (var property in this)
        if (property[0] === prefix)
          delete this[property];
    },
    keys: function() {
      var keys4 = [];
      for (var property in this)
        if (property[0] === prefix)
          keys4.push(property.slice(1));
      return keys4;
    },
    values: function() {
      var values2 = [];
      for (var property in this)
        if (property[0] === prefix)
          values2.push(this[property]);
      return values2;
    },
    entries: function() {
      var entries = [];
      for (var property in this)
        if (property[0] === prefix)
          entries.push({ key: property.slice(1), value: this[property] });
      return entries;
    },
    size: function() {
      var size3 = 0;
      for (var property in this)
        if (property[0] === prefix)
          ++size3;
      return size3;
    },
    empty: function() {
      for (var property in this)
        if (property[0] === prefix)
          return false;
      return true;
    },
    each: function(f) {
      for (var property in this)
        if (property[0] === prefix)
          f(this[property], property.slice(1), this);
    }
  };
  function map2(object, f) {
    var map50 = new Map2();
    if (object instanceof Map2)
      object.each(function(value2, key2) {
        map50.set(key2, value2);
      });
    else if (Array.isArray(object)) {
      var i = -1, n = object.length, o;
      if (f == null)
        while (++i < n)
          map50.set(i, object[i]);
      else
        while (++i < n)
          map50.set(f(o = object[i], i, object), o);
    } else if (object)
      for (var key in object)
        map50.set(key, object[key]);
    return map50;
  }
  var map_default = map2;

  // node_modules/d3-collection/src/set.js
  function Set() {
  }
  var proto = map_default.prototype;
  Set.prototype = set3.prototype = {
    constructor: Set,
    has: proto.has,
    add: function(value2) {
      value2 += "";
      this[prefix + value2] = value2;
      return this;
    },
    remove: proto.remove,
    clear: proto.clear,
    values: proto.keys,
    size: proto.size,
    empty: proto.empty,
    each: proto.each
  };
  function set3(object, f) {
    var set4 = new Set();
    if (object instanceof Set)
      object.each(function(value2) {
        set4.add(value2);
      });
    else if (object) {
      var i = -1, n = object.length;
      if (f == null)
        while (++i < n)
          set4.add(object[i]);
      else
        while (++i < n)
          set4.add(f(object[i], i, object));
    }
    return set4;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/namespaces.js
  var xhtml2 = "http://www.w3.org/1999/xhtml";
  var namespaces_default2 = {
    svg: "http://www.w3.org/2000/svg",
    xhtml: xhtml2,
    xlink: "http://www.w3.org/1999/xlink",
    xml: "http://www.w3.org/XML/1998/namespace",
    xmlns: "http://www.w3.org/2000/xmlns/"
  };

  // node_modules/d3-tip/node_modules/d3-selection/src/namespace.js
  function namespace_default2(name3) {
    var prefix2 = name3 += "", i = prefix2.indexOf(":");
    if (i >= 0 && (prefix2 = name3.slice(0, i)) !== "xmlns")
      name3 = name3.slice(i + 1);
    return namespaces_default2.hasOwnProperty(prefix2) ? { space: namespaces_default2[prefix2], local: name3 } : name3;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/creator.js
  function creatorInherit2(name3) {
    return function() {
      var document2 = this.ownerDocument, uri = this.namespaceURI;
      return uri === xhtml2 && document2.documentElement.namespaceURI === xhtml2 ? document2.createElement(name3) : document2.createElementNS(uri, name3);
    };
  }
  function creatorFixed2(fullname) {
    return function() {
      return this.ownerDocument.createElementNS(fullname.space, fullname.local);
    };
  }
  function creator_default2(name3) {
    var fullname = namespace_default2(name3);
    return (fullname.local ? creatorFixed2 : creatorInherit2)(fullname);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selector.js
  function none2() {
  }
  function selector_default2(selector) {
    return selector == null ? none2 : function() {
      return this.querySelector(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/select.js
  function select_default4(select) {
    if (typeof select !== "function")
      select = selector_default2(select);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = new Array(n), node, subnode, i = 0; i < n; ++i) {
        if ((node = group3[i]) && (subnode = select.call(node, node.__data__, i, group3))) {
          if ("__data__" in node)
            subnode.__data__ = node.__data__;
          subgroup[i] = subnode;
        }
      }
    }
    return new Selection3(subgroups, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selectorAll.js
  function empty2() {
    return [];
  }
  function selectorAll_default2(selector) {
    return selector == null ? empty2 : function() {
      return this.querySelectorAll(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/selectAll.js
  function selectAll_default3(select) {
    if (typeof select !== "function")
      select = selectorAll_default2(select);
    for (var groups = this._groups, m = groups.length, subgroups = [], parents = [], j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          subgroups.push(select.call(node, node.__data__, i, group3));
          parents.push(node);
        }
      }
    }
    return new Selection3(subgroups, parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/matcher.js
  function matcher_default2(selector) {
    return function() {
      return this.matches(selector);
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/filter.js
  function filter_default3(match5) {
    if (typeof match5 !== "function")
      match5 = matcher_default2(match5);
    for (var groups = this._groups, m = groups.length, subgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, subgroup = subgroups[j] = [], node, i = 0; i < n; ++i) {
        if ((node = group3[i]) && match5.call(node, node.__data__, i, group3)) {
          subgroup.push(node);
        }
      }
    }
    return new Selection3(subgroups, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/sparse.js
  function sparse_default2(update4) {
    return new Array(update4.length);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/enter.js
  function enter_default2() {
    return new Selection3(this._enter || this._groups.map(sparse_default2), this._parents);
  }
  function EnterNode2(parent, datum2) {
    this.ownerDocument = parent.ownerDocument;
    this.namespaceURI = parent.namespaceURI;
    this._next = null;
    this._parent = parent;
    this.__data__ = datum2;
  }
  EnterNode2.prototype = {
    constructor: EnterNode2,
    appendChild: function(child) {
      return this._parent.insertBefore(child, this._next);
    },
    insertBefore: function(child, next) {
      return this._parent.insertBefore(child, next);
    },
    querySelector: function(selector) {
      return this._parent.querySelector(selector);
    },
    querySelectorAll: function(selector) {
      return this._parent.querySelectorAll(selector);
    }
  };

  // node_modules/d3-tip/node_modules/d3-selection/src/constant.js
  function constant_default6(x2) {
    return function() {
      return x2;
    };
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/data.js
  var keyPrefix = "$";
  function bindIndex2(parent, group3, enter, update4, exit, data) {
    var i = 0, node, groupLength = group3.length, dataLength = data.length;
    for (; i < dataLength; ++i) {
      if (node = group3[i]) {
        node.__data__ = data[i];
        update4[i] = node;
      } else {
        enter[i] = new EnterNode2(parent, data[i]);
      }
    }
    for (; i < groupLength; ++i) {
      if (node = group3[i]) {
        exit[i] = node;
      }
    }
  }
  function bindKey2(parent, group3, enter, update4, exit, data, key) {
    var i, node, nodeByKeyValue = {}, groupLength = group3.length, dataLength = data.length, keyValues = new Array(groupLength), keyValue;
    for (i = 0; i < groupLength; ++i) {
      if (node = group3[i]) {
        keyValues[i] = keyValue = keyPrefix + key.call(node, node.__data__, i, group3);
        if (keyValue in nodeByKeyValue) {
          exit[i] = node;
        } else {
          nodeByKeyValue[keyValue] = node;
        }
      }
    }
    for (i = 0; i < dataLength; ++i) {
      keyValue = keyPrefix + key.call(parent, data[i], i, data);
      if (node = nodeByKeyValue[keyValue]) {
        update4[i] = node;
        node.__data__ = data[i];
        nodeByKeyValue[keyValue] = null;
      } else {
        enter[i] = new EnterNode2(parent, data[i]);
      }
    }
    for (i = 0; i < groupLength; ++i) {
      if ((node = group3[i]) && nodeByKeyValue[keyValues[i]] === node) {
        exit[i] = node;
      }
    }
  }
  function data_default2(value2, key) {
    if (!value2) {
      data = new Array(this.size()), j = -1;
      this.each(function(d) {
        data[++j] = d;
      });
      return data;
    }
    var bind20 = key ? bindKey2 : bindIndex2, parents = this._parents, groups = this._groups;
    if (typeof value2 !== "function")
      value2 = constant_default6(value2);
    for (var m = groups.length, update4 = new Array(m), enter = new Array(m), exit = new Array(m), j = 0; j < m; ++j) {
      var parent = parents[j], group3 = groups[j], groupLength = group3.length, data = value2.call(parent, parent && parent.__data__, j, parents), dataLength = data.length, enterGroup = enter[j] = new Array(dataLength), updateGroup = update4[j] = new Array(dataLength), exitGroup = exit[j] = new Array(groupLength);
      bind20(parent, group3, enterGroup, updateGroup, exitGroup, data, key);
      for (var i0 = 0, i1 = 0, previous, next; i0 < dataLength; ++i0) {
        if (previous = enterGroup[i0]) {
          if (i0 >= i1)
            i1 = i0 + 1;
          while (!(next = updateGroup[i1]) && ++i1 < dataLength)
            ;
          previous._next = next || null;
        }
      }
    }
    update4 = new Selection3(update4, parents);
    update4._enter = enter;
    update4._exit = exit;
    return update4;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/exit.js
  function exit_default2() {
    return new Selection3(this._exit || this._groups.map(sparse_default2), this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/join.js
  function join_default2(onenter, onupdate, onexit) {
    var enter = this.enter(), update4 = this, exit = this.exit();
    enter = typeof onenter === "function" ? onenter(enter) : enter.append(onenter + "");
    if (onupdate != null)
      update4 = onupdate(update4);
    if (onexit == null)
      exit.remove();
    else
      onexit(exit);
    return enter && update4 ? enter.merge(update4).order() : update4;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/merge.js
  function merge_default3(selection3) {
    for (var groups0 = this._groups, groups1 = selection3._groups, m0 = groups0.length, m1 = groups1.length, m = Math.min(m0, m1), merges = new Array(m0), j = 0; j < m; ++j) {
      for (var group0 = groups0[j], group1 = groups1[j], n = group0.length, merge = merges[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group0[i] || group1[i]) {
          merge[i] = node;
        }
      }
    }
    for (; j < m0; ++j) {
      merges[j] = groups0[j];
    }
    return new Selection3(merges, this._parents);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/order.js
  function order_default2() {
    for (var groups = this._groups, j = -1, m = groups.length; ++j < m; ) {
      for (var group3 = groups[j], i = group3.length - 1, next = group3[i], node; --i >= 0; ) {
        if (node = group3[i]) {
          if (next && node.compareDocumentPosition(next) ^ 4)
            next.parentNode.insertBefore(node, next);
          next = node;
        }
      }
    }
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/sort.js
  function sort_default2(compare6) {
    if (!compare6)
      compare6 = ascending2;
    function compareNode(a, b) {
      return a && b ? compare6(a.__data__, b.__data__) : !a - !b;
    }
    for (var groups = this._groups, m = groups.length, sortgroups = new Array(m), j = 0; j < m; ++j) {
      for (var group3 = groups[j], n = group3.length, sortgroup = sortgroups[j] = new Array(n), node, i = 0; i < n; ++i) {
        if (node = group3[i]) {
          sortgroup[i] = node;
        }
      }
      sortgroup.sort(compareNode);
    }
    return new Selection3(sortgroups, this._parents).order();
  }
  function ascending2(a, b) {
    return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/call.js
  function call_default2() {
    var callback = arguments[0];
    arguments[0] = this;
    callback.apply(null, arguments);
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/nodes.js
  function nodes_default2() {
    var nodes = new Array(this.size()), i = -1;
    this.each(function() {
      nodes[++i] = this;
    });
    return nodes;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/node.js
  function node_default2() {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i = 0, n = group3.length; i < n; ++i) {
        var node = group3[i];
        if (node)
          return node;
      }
    }
    return null;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/size.js
  function size_default2() {
    var size3 = 0;
    this.each(function() {
      ++size3;
    });
    return size3;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/empty.js
  function empty_default2() {
    return !this.node();
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/each.js
  function each_default2(callback) {
    for (var groups = this._groups, j = 0, m = groups.length; j < m; ++j) {
      for (var group3 = groups[j], i = 0, n = group3.length, node; i < n; ++i) {
        if (node = group3[i])
          callback.call(node, node.__data__, i, group3);
      }
    }
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/attr.js
  function attrRemove3(name3) {
    return function() {
      this.removeAttribute(name3);
    };
  }
  function attrRemoveNS3(fullname) {
    return function() {
      this.removeAttributeNS(fullname.space, fullname.local);
    };
  }
  function attrConstant3(name3, value2) {
    return function() {
      this.setAttribute(name3, value2);
    };
  }
  function attrConstantNS3(fullname, value2) {
    return function() {
      this.setAttributeNS(fullname.space, fullname.local, value2);
    };
  }
  function attrFunction3(name3, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.removeAttribute(name3);
      else
        this.setAttribute(name3, v);
    };
  }
  function attrFunctionNS3(fullname, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.removeAttributeNS(fullname.space, fullname.local);
      else
        this.setAttributeNS(fullname.space, fullname.local, v);
    };
  }
  function attr_default3(name3, value2) {
    var fullname = namespace_default2(name3);
    if (arguments.length < 2) {
      var node = this.node();
      return fullname.local ? node.getAttributeNS(fullname.space, fullname.local) : node.getAttribute(fullname);
    }
    return this.each((value2 == null ? fullname.local ? attrRemoveNS3 : attrRemove3 : typeof value2 === "function" ? fullname.local ? attrFunctionNS3 : attrFunction3 : fullname.local ? attrConstantNS3 : attrConstant3)(fullname, value2));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/window.js
  function window_default2(node) {
    return node.ownerDocument && node.ownerDocument.defaultView || node.document && node || node.defaultView;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/style.js
  function styleRemove3(name3) {
    return function() {
      this.style.removeProperty(name3);
    };
  }
  function styleConstant3(name3, value2, priority) {
    return function() {
      this.style.setProperty(name3, value2, priority);
    };
  }
  function styleFunction3(name3, value2, priority) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        this.style.removeProperty(name3);
      else
        this.style.setProperty(name3, v, priority);
    };
  }
  function style_default3(name3, value2, priority) {
    return arguments.length > 1 ? this.each((value2 == null ? styleRemove3 : typeof value2 === "function" ? styleFunction3 : styleConstant3)(name3, value2, priority == null ? "" : priority)) : styleValue2(this.node(), name3);
  }
  function styleValue2(node, name3) {
    return node.style.getPropertyValue(name3) || window_default2(node).getComputedStyle(node, null).getPropertyValue(name3);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/property.js
  function propertyRemove2(name3) {
    return function() {
      delete this[name3];
    };
  }
  function propertyConstant2(name3, value2) {
    return function() {
      this[name3] = value2;
    };
  }
  function propertyFunction2(name3, value2) {
    return function() {
      var v = value2.apply(this, arguments);
      if (v == null)
        delete this[name3];
      else
        this[name3] = v;
    };
  }
  function property_default2(name3, value2) {
    return arguments.length > 1 ? this.each((value2 == null ? propertyRemove2 : typeof value2 === "function" ? propertyFunction2 : propertyConstant2)(name3, value2)) : this.node()[name3];
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/classed.js
  function classArray2(string3) {
    return string3.trim().split(/^|\s+/);
  }
  function classList2(node) {
    return node.classList || new ClassList2(node);
  }
  function ClassList2(node) {
    this._node = node;
    this._names = classArray2(node.getAttribute("class") || "");
  }
  ClassList2.prototype = {
    add: function(name3) {
      var i = this._names.indexOf(name3);
      if (i < 0) {
        this._names.push(name3);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    remove: function(name3) {
      var i = this._names.indexOf(name3);
      if (i >= 0) {
        this._names.splice(i, 1);
        this._node.setAttribute("class", this._names.join(" "));
      }
    },
    contains: function(name3) {
      return this._names.indexOf(name3) >= 0;
    }
  };
  function classedAdd2(node, names) {
    var list = classList2(node), i = -1, n = names.length;
    while (++i < n)
      list.add(names[i]);
  }
  function classedRemove2(node, names) {
    var list = classList2(node), i = -1, n = names.length;
    while (++i < n)
      list.remove(names[i]);
  }
  function classedTrue2(names) {
    return function() {
      classedAdd2(this, names);
    };
  }
  function classedFalse2(names) {
    return function() {
      classedRemove2(this, names);
    };
  }
  function classedFunction2(names, value2) {
    return function() {
      (value2.apply(this, arguments) ? classedAdd2 : classedRemove2)(this, names);
    };
  }
  function classed_default2(name3, value2) {
    var names = classArray2(name3 + "");
    if (arguments.length < 2) {
      var list = classList2(this.node()), i = -1, n = names.length;
      while (++i < n)
        if (!list.contains(names[i]))
          return false;
      return true;
    }
    return this.each((typeof value2 === "function" ? classedFunction2 : value2 ? classedTrue2 : classedFalse2)(names, value2));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/text.js
  function textRemove2() {
    this.textContent = "";
  }
  function textConstant3(value2) {
    return function() {
      this.textContent = value2;
    };
  }
  function textFunction3(value2) {
    return function() {
      var v = value2.apply(this, arguments);
      this.textContent = v == null ? "" : v;
    };
  }
  function text_default3(value2) {
    return arguments.length ? this.each(value2 == null ? textRemove2 : (typeof value2 === "function" ? textFunction3 : textConstant3)(value2)) : this.node().textContent;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/html.js
  function htmlRemove2() {
    this.innerHTML = "";
  }
  function htmlConstant2(value2) {
    return function() {
      this.innerHTML = value2;
    };
  }
  function htmlFunction2(value2) {
    return function() {
      var v = value2.apply(this, arguments);
      this.innerHTML = v == null ? "" : v;
    };
  }
  function html_default2(value2) {
    return arguments.length ? this.each(value2 == null ? htmlRemove2 : (typeof value2 === "function" ? htmlFunction2 : htmlConstant2)(value2)) : this.node().innerHTML;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/raise.js
  function raise2() {
    if (this.nextSibling)
      this.parentNode.appendChild(this);
  }
  function raise_default2() {
    return this.each(raise2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/lower.js
  function lower2() {
    if (this.previousSibling)
      this.parentNode.insertBefore(this, this.parentNode.firstChild);
  }
  function lower_default2() {
    return this.each(lower2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/append.js
  function append_default2(name3) {
    var create2 = typeof name3 === "function" ? name3 : creator_default2(name3);
    return this.select(function() {
      return this.appendChild(create2.apply(this, arguments));
    });
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/insert.js
  function constantNull2() {
    return null;
  }
  function insert_default2(name3, before) {
    var create2 = typeof name3 === "function" ? name3 : creator_default2(name3), select = before == null ? constantNull2 : typeof before === "function" ? before : selector_default2(before);
    return this.select(function() {
      return this.insertBefore(create2.apply(this, arguments), select.apply(this, arguments) || null);
    });
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/remove.js
  function remove2() {
    var parent = this.parentNode;
    if (parent)
      parent.removeChild(this);
  }
  function remove_default3() {
    return this.each(remove2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/clone.js
  function selection_cloneShallow2() {
    var clone = this.cloneNode(false), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function selection_cloneDeep2() {
    var clone = this.cloneNode(true), parent = this.parentNode;
    return parent ? parent.insertBefore(clone, this.nextSibling) : clone;
  }
  function clone_default2(deep) {
    return this.select(deep ? selection_cloneDeep2 : selection_cloneShallow2);
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/datum.js
  function datum_default2(value2) {
    return arguments.length ? this.property("__data__", value2) : this.node().__data__;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/on.js
  var filterEvents = {};
  var event = null;
  if (typeof document !== "undefined") {
    element = document.documentElement;
    if (!("onmouseenter" in element)) {
      filterEvents = { mouseenter: "mouseover", mouseleave: "mouseout" };
    }
  }
  var element;
  function filterContextListener(listener, index3, group3) {
    listener = contextListener2(listener, index3, group3);
    return function(event2) {
      var related = event2.relatedTarget;
      if (!related || related !== this && !(related.compareDocumentPosition(this) & 8)) {
        listener.call(this, event2);
      }
    };
  }
  function contextListener2(listener, index3, group3) {
    return function(event1) {
      var event0 = event;
      event = event1;
      try {
        listener.call(this, this.__data__, index3, group3);
      } finally {
        event = event0;
      }
    };
  }
  function parseTypenames3(typenames) {
    return typenames.trim().split(/^|\s+/).map(function(t) {
      var name3 = "", i = t.indexOf(".");
      if (i >= 0)
        name3 = t.slice(i + 1), t = t.slice(0, i);
      return { type: t, name: name3 };
    });
  }
  function onRemove2(typename) {
    return function() {
      var on2 = this.__on;
      if (!on2)
        return;
      for (var j = 0, i = -1, m = on2.length, o; j < m; ++j) {
        if (o = on2[j], (!typename.type || o.type === typename.type) && o.name === typename.name) {
          this.removeEventListener(o.type, o.listener, o.capture);
        } else {
          on2[++i] = o;
        }
      }
      if (++i)
        on2.length = i;
      else
        delete this.__on;
    };
  }
  function onAdd2(typename, value2, capture) {
    var wrap3 = filterEvents.hasOwnProperty(typename.type) ? filterContextListener : contextListener2;
    return function(d, i, group3) {
      var on2 = this.__on, o, listener = wrap3(value2, i, group3);
      if (on2)
        for (var j = 0, m = on2.length; j < m; ++j) {
          if ((o = on2[j]).type === typename.type && o.name === typename.name) {
            this.removeEventListener(o.type, o.listener, o.capture);
            this.addEventListener(o.type, o.listener = listener, o.capture = capture);
            o.value = value2;
            return;
          }
        }
      this.addEventListener(typename.type, listener, capture);
      o = { type: typename.type, name: typename.name, value: value2, listener, capture };
      if (!on2)
        this.__on = [o];
      else
        on2.push(o);
    };
  }
  function on_default3(typename, value2, capture) {
    var typenames = parseTypenames3(typename + ""), i, n = typenames.length, t;
    if (arguments.length < 2) {
      var on2 = this.node().__on;
      if (on2)
        for (var j = 0, m = on2.length, o; j < m; ++j) {
          for (i = 0, o = on2[j]; i < n; ++i) {
            if ((t = typenames[i]).type === o.type && t.name === o.name) {
              return o.value;
            }
          }
        }
      return;
    }
    on2 = value2 ? onAdd2 : onRemove2;
    if (capture == null)
      capture = false;
    for (i = 0; i < n; ++i)
      this.each(on2(typenames[i], value2, capture));
    return this;
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/dispatch.js
  function dispatchEvent2(node, type2, params) {
    var window2 = window_default2(node), event2 = window2.CustomEvent;
    if (typeof event2 === "function") {
      event2 = new event2(type2, params);
    } else {
      event2 = window2.document.createEvent("Event");
      if (params)
        event2.initEvent(type2, params.bubbles, params.cancelable), event2.detail = params.detail;
      else
        event2.initEvent(type2, false, false);
    }
    node.dispatchEvent(event2);
  }
  function dispatchConstant2(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params);
    };
  }
  function dispatchFunction2(type2, params) {
    return function() {
      return dispatchEvent2(this, type2, params.apply(this, arguments));
    };
  }
  function dispatch_default3(type2, params) {
    return this.each((typeof params === "function" ? dispatchFunction2 : dispatchConstant2)(type2, params));
  }

  // node_modules/d3-tip/node_modules/d3-selection/src/selection/index.js
  var root2 = [null];
  function Selection3(groups, parents) {
    this._groups = groups;
    this._parents = parents;
  }
  function selection2() {
    return new Selection3([[document.documentElement]], root2);
  }
  Selection3.prototype = selection2.prototype = {
    constructor: Selection3,
    select: select_default4,
    selectAll: selectAll_default3,
    filter: filter_default3,
    data: data_default2,
    enter: enter_default2,
    exit: exit_default2,
    join: join_default2,
    merge: merge_default3,
    order: order_default2,
    sort: sort_default2,
    call: call_default2,
    nodes: nodes_default2,
    node: node_default2,
    size: size_default2,
    empty: empty_default2,
    each: each_default2,
    attr: attr_default3,
    style: style_default3,
    property: property_default2,
    classed: classed_default2,
    text: text_default3,
    html: html_default2,
    raise: raise_default2,
    lower: lower_default2,
    append: append_default2,
    insert: insert_default2,
    remove: remove_default3,
    clone: clone_default2,
    datum: datum_default2,
    on: on_default3,
    dispatch: dispatch_default3
  };
  var selection_default3 = selection2;

  // node_modules/d3-tip/node_modules/d3-selection/src/select.js
  function select_default5(selector) {
    return typeof selector === "string" ? new Selection3([[document.querySelector(selector)]], [document.documentElement]) : new Selection3([[selector]], root2);
  }

  // node_modules/d3-tip/index.js
  function d3_tip_default() {
    var direction = d3TipDirection, offset = d3TipOffset, html = d3TipHTML, rootElement = document.body, node = initNode(), svg = null, point2 = null, target2 = null;
    function tip(vis) {
      svg = getSVGNode(vis);
      if (!svg)
        return;
      point2 = svg.createSVGPoint();
      rootElement.appendChild(node);
    }
    tip.show = function() {
      var args = Array.prototype.slice.call(arguments);
      if (args[args.length - 1] instanceof SVGElement)
        target2 = args.pop();
      var content = html.apply(this, args), poffset = offset.apply(this, args), dir = direction.apply(this, args), nodel = getNodeEl(), i = directions.length, coords, scrollTop = document.documentElement.scrollTop || rootElement.scrollTop, scrollLeft = document.documentElement.scrollLeft || rootElement.scrollLeft;
      nodel.html(content).style("opacity", 1).style("pointer-events", "all");
      while (i--)
        nodel.classed(directions[i], false);
      coords = directionCallbacks.get(dir).apply(this);
      nodel.classed(dir, true).style("top", coords.top + poffset[0] + scrollTop + "px").style("left", coords.left + poffset[1] + scrollLeft + "px");
      return tip;
    };
    tip.hide = function() {
      var nodel = getNodeEl();
      nodel.style("opacity", 0).style("pointer-events", "none");
      return tip;
    };
    tip.attr = function(n, v) {
      if (arguments.length < 2 && typeof n === "string") {
        return getNodeEl().attr(n);
      }
      var args = Array.prototype.slice.call(arguments);
      selection_default3.prototype.attr.apply(getNodeEl(), args);
      return tip;
    };
    tip.style = function(n, v) {
      if (arguments.length < 2 && typeof n === "string") {
        return getNodeEl().style(n);
      }
      var args = Array.prototype.slice.call(arguments);
      selection_default3.prototype.style.apply(getNodeEl(), args);
      return tip;
    };
    tip.direction = function(v) {
      if (!arguments.length)
        return direction;
      direction = v == null ? v : functor(v);
      return tip;
    };
    tip.offset = function(v) {
      if (!arguments.length)
        return offset;
      offset = v == null ? v : functor(v);
      return tip;
    };
    tip.html = function(v) {
      if (!arguments.length)
        return html;
      html = v == null ? v : functor(v);
      return tip;
    };
    tip.rootElement = function(v) {
      if (!arguments.length)
        return rootElement;
      rootElement = v == null ? v : functor(v);
      return tip;
    };
    tip.destroy = function() {
      if (node) {
        getNodeEl().remove();
        node = null;
      }
      return tip;
    };
    function d3TipDirection() {
      return "n";
    }
    function d3TipOffset() {
      return [0, 0];
    }
    function d3TipHTML() {
      return " ";
    }
    var directionCallbacks = map_default({
      n: directionNorth,
      s: directionSouth,
      e: directionEast,
      w: directionWest,
      nw: directionNorthWest,
      ne: directionNorthEast,
      sw: directionSouthWest,
      se: directionSouthEast
    }), directions = directionCallbacks.keys();
    function directionNorth() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.n.y - node.offsetHeight,
        left: bbox.n.x - node.offsetWidth / 2
      };
    }
    function directionSouth() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.s.y,
        left: bbox.s.x - node.offsetWidth / 2
      };
    }
    function directionEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.e.y - node.offsetHeight / 2,
        left: bbox.e.x
      };
    }
    function directionWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.w.y - node.offsetHeight / 2,
        left: bbox.w.x - node.offsetWidth
      };
    }
    function directionNorthWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.nw.y - node.offsetHeight,
        left: bbox.nw.x - node.offsetWidth
      };
    }
    function directionNorthEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.ne.y - node.offsetHeight,
        left: bbox.ne.x
      };
    }
    function directionSouthWest() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.sw.y,
        left: bbox.sw.x - node.offsetWidth
      };
    }
    function directionSouthEast() {
      var bbox = getScreenBBox(this);
      return {
        top: bbox.se.y,
        left: bbox.se.x
      };
    }
    function initNode() {
      var div4 = select_default5(document.createElement("div"));
      div4.style("position", "absolute").style("top", 0).style("opacity", 0).style("pointer-events", "none").style("box-sizing", "border-box");
      return div4.node();
    }
    function getSVGNode(element) {
      var svgNode2 = element.node();
      if (!svgNode2)
        return null;
      if (svgNode2.tagName.toLowerCase() === "svg")
        return svgNode2;
      return svgNode2.ownerSVGElement;
    }
    function getNodeEl() {
      if (node == null) {
        node = initNode();
        rootElement.appendChild(node);
      }
      return select_default5(node);
    }
    function getScreenBBox(targetShape) {
      var targetel = target2 || targetShape;
      while (targetel.getScreenCTM == null && targetel.parentNode != null) {
        targetel = targetel.parentNode;
      }
      var bbox = {}, matrix = targetel.getScreenCTM(), tbbox = targetel.getBBox(), width = tbbox.width, height = tbbox.height, x2 = tbbox.x, y2 = tbbox.y;
      point2.x = x2;
      point2.y = y2;
      bbox.nw = point2.matrixTransform(matrix);
      point2.x += width;
      bbox.ne = point2.matrixTransform(matrix);
      point2.y += height;
      bbox.se = point2.matrixTransform(matrix);
      point2.x -= width;
      bbox.sw = point2.matrixTransform(matrix);
      point2.y -= height / 2;
      bbox.w = point2.matrixTransform(matrix);
      point2.x += width;
      bbox.e = point2.matrixTransform(matrix);
      point2.x -= width / 2;
      point2.y -= height / 2;
      bbox.n = point2.matrixTransform(matrix);
      point2.y += height;
      bbox.s = point2.matrixTransform(matrix);
      return bbox;
    }
    function functor(v) {
      return typeof v === "function" ? v : function() {
        return v;
      };
    }
    return tip;
  }

  // output/App.BarChart/foreign.js
  function curry4(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function drawBarChart_(id3, childIndex, {
    caption,
    data
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const margin = { top: 15, right: 0, bottom: 40, left: 30 }, width = 200 - margin.left - margin.right, height = 185 - margin.top - margin.bottom;
      const div4 = select_default2("#" + id3);
      div4.selectAll("#" + childId).remove();
      const svg = div4.append("svg").attr("width", width + margin.left + margin.right).attr("height", height + margin.top + margin.bottom).attr("id", childId).append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
      const tip = d3_tip_default().attr("class", "d3-tip").offset([0, 0]).html((_, d) => d.y.value0);
      svg.call(tip);
      const x2 = band().range([0, width]).domain(data.map((d) => d.x.value0)).padding(0.2);
      svg.append("g").attr("transform", "translate(0," + height + ")").call(axisBottom(x2)).selectAll("text").style("text-anchor", "middle");
      const nearest = 100, y_max = Math.ceil(Math.max(...data.map((d) => d.y.value0)) / nearest) * nearest;
      const y2 = linear2().domain([0, y_max]).range([height, 0]);
      const tickEvery = nearest / 2, ticks = Array.from(Array(y_max / tickEvery + 1).keys()).map((n) => n * tickEvery);
      const yAxis = axisLeft(y2).tickValues(ticks);
      svg.append("g").call(yAxis);
      const barFill = "#dcdcdc";
      svg.selectAll("rect").data([...data.entries()]).enter().append("rect").attr("x", ([, d]) => x2(d.x.value0)).attr("y", ([, d]) => y2(d.y.value0 + 1)).attr("width", x2.bandwidth()).attr("height", ([, d]) => height - y2(d.y.value0)).attr("fill", ([, d]) => d.y.value1 ? colorShade(barFill, -40) : barFill).attr("class", ([, d]) => d.y.value1 ? "bar-selected" : "bar-unselected").on("mousedown", (e, d) => {
        console.log(`mousedown ${d[0]}`);
        listener(e);
      });
      svg.append("text").text(caption.value0).attr("x", width / 2).attr("y", height + 35).attr("class", "title-text").attr("dominant-baseline", "bottom").attr("text-anchor", "middle");
    };
  }
  var drawBarChart = curry4(drawBarChart_);

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x2) {
          return f(g(x2));
        };
      };
    }
  };
  var compose = function(dict) {
    return dict.compose;
  };
  var composeFlipped = function(dictSemigroupoid) {
    var compose1 = compose(dictSemigroupoid);
    return function(f) {
      return function(g) {
        return compose1(g)(f);
      };
    };
  };

  // output/Control.Category/index.js
  var identity4 = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x2) {
      return x2;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var on = function(f) {
    return function(g) {
      return function(x2) {
        return function(y2) {
          return f(g(x2))(g(y2));
        };
      };
    };
  };
  var flip = function(f) {
    return function(b) {
      return function(a) {
        return f(a)(b);
      };
    };
  };
  var $$const = function(a) {
    return function(v) {
      return a;
    };
  };
  var applyN = function(f) {
    var go = function($copy_n) {
      return function($copy_acc) {
        var $tco_var_n = $copy_n;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(n, acc) {
          if (n <= 0) {
            $tco_done = true;
            return acc;
          }
          ;
          if (otherwise) {
            $tco_var_n = n - 1 | 0;
            $copy_acc = f(acc);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Function (line 107, column 3 - line 109, column 37): " + [n.constructor.name, acc.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_n, $copy_acc);
        }
        ;
        return $tco_result;
      };
    };
    return go;
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit2 = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map3 = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map118 = map3(dictFunctor);
    return function(fa) {
      return function(f) {
        return map118(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map3(dictFunctor)($$const(unit2));
  };
  var voidLeft = function(dictFunctor) {
    var map118 = map3(dictFunctor);
    return function(f) {
      return function(x2) {
        return map118($$const(x2))(f);
      };
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
  };
  var flap = function(dictFunctor) {
    var map118 = map3(dictFunctor);
    return function(ff2) {
      return function(x2) {
        return map118(function(f) {
          return f(x2);
        })(ff2);
      };
    };
  };

  // output/Control.Apply/index.js
  var identity5 = /* @__PURE__ */ identity4(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applyFirst = function(dictApply) {
    var apply12 = apply(dictApply);
    var map50 = map3(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply12(map50($$const)(a))(b);
      };
    };
  };
  var applySecond = function(dictApply) {
    var apply12 = apply(dictApply);
    var map50 = map3(dictApply.Functor0());
    return function(a) {
      return function(b) {
        return apply12(map50($$const(identity5))(a))(b);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply12 = apply(dictApply);
    var map50 = map3(dictApply.Functor0());
    return function(f) {
      return function(a) {
        return function(b) {
          return apply12(map50(f)(a))(b);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply10 = apply(dictApplicative.Apply0());
    var pure112 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply10(pure112(f))(a);
      };
    };
  };

  // output/Control.Bind/index.js
  var identity6 = /* @__PURE__ */ identity4(categoryFn);
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped1 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a) {
          return bindFlipped1(f)(g(a));
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };
  var join = function(dictBind) {
    var bind110 = bind(dictBind);
    return function(m) {
      return bind110(m)(identity6);
    };
  };

  // output/Data.Array/foreign.js
  var range = function(start2) {
    return function(end) {
      var step2 = start2 > end ? -1 : 1;
      var result = new Array(step2 * (end - start2) + 1);
      var i = start2, n = 0;
      while (i !== end) {
        result[n++] = i;
        i += step2;
      }
      result[n] = i;
      return result;
    };
  };
  var replicateFill = function(count) {
    return function(value2) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value2);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value2) {
      var result = [];
      var n = 0;
      for (var i = 0; i < count; i++) {
        result[n++] = value2;
      }
      return result;
    };
  };
  var replicate = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head6) {
      return function(tail2) {
        return new Cons3(head6, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr8) {
      return function(xs) {
        return listToArray(foldr8(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var unconsImpl = function(empty10) {
    return function(next) {
      return function(xs) {
        return xs.length === 0 ? empty10({}) : next(xs[0])(xs.slice(1));
      };
    };
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i) {
          return i < 0 || i >= xs.length ? nothing : just(xs[i]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i = 0, l = xs.length; i < l; i++) {
            if (f(xs[i]))
              return just(i);
          }
          return nothing;
        };
      };
    };
  };
  var _updateAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(a) {
          return function(l) {
            if (i < 0 || i >= l.length)
              return nothing;
            var l1 = l.slice();
            l1[i] = a;
            return just(l1);
          };
        };
      };
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare6, fromOrdering, xs1, xs2, from7, to) {
      var mid;
      var i;
      var j;
      var k;
      var x2;
      var y2;
      var c;
      mid = from7 + (to - from7 >> 1);
      if (mid - from7 > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, from7, mid);
      if (to - mid > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, mid, to);
      i = from7;
      j = mid;
      k = from7;
      while (i < mid && j < to) {
        x2 = xs2[i];
        y2 = xs2[j];
        c = fromOrdering(compare6(x2)(y2));
        if (c > 0) {
          xs1[k++] = y2;
          ++j;
        } else {
          xs1[k++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare6) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare6, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice3 = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };
  var zipWith = function(f) {
    return function(xs) {
      return function(ys) {
        var l = xs.length < ys.length ? xs.length : ys.length;
        var result = new Array(l);
        for (var i = 0; i < l; i++) {
          result[i] = f(xs[i])(ys[i]);
        }
        return result;
      };
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Lazy/index.js
  var $runtime_lazy = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var defer = function(dict) {
    return dict.defer;
  };
  var fix = function(dictLazy) {
    var defer1 = defer(dictLazy);
    return function(f) {
      var $lazy_go = $runtime_lazy("go", "Control.Lazy", function() {
        return defer1(function(v) {
          return f($lazy_go(25));
        });
      });
      var go = $lazy_go(25);
      return go;
    };
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind20 = bind(dictMonad.Bind1());
    var pure24 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind20(f)(function(f$prime) {
          return bind20(a)(function(a$prime) {
            return pure24(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq17) {
      return function(gt) {
        return function(x2) {
          return function(y2) {
            return x2 < y2 ? lt : x2 === y2 ? eq17 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;
  var eqArrayImpl = function(f) {
    return function(xs) {
      return function(ys) {
        if (xs.length !== ys.length)
          return false;
        for (var i = 0; i < xs.length; i++) {
          if (!f(xs[i])(ys[i]))
            return false;
        }
        return true;
      };
    };
  };

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var eqArray = function(dictEq) {
    return {
      eq: eqArrayImpl(eq(dictEq))
    };
  };
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x2) {
      return function(y2) {
        return eq2(eq32(x2)(y2))(false);
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();
  var invert = function(v) {
    if (v instanceof GT) {
      return LT.value;
    }
    ;
    if (v instanceof EQ) {
      return EQ.value;
    }
    ;
    if (v instanceof LT) {
      return GT.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Ordering (line 33, column 1 - line 33, column 31): " + [v.constructor.name]);
  };
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x2) {
    return function(y2) {
      return x2 - y2 | 0;
    };
  };
  var numSub = function(n1) {
    return function(n2) {
      return n1 - n2;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x2) {
    return function(y2) {
      return x2 + y2 | 0;
    };
  };
  var intMul = function(x2) {
    return function(y2) {
      return x2 * y2 | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Semiring/index.js
  var zero2 = function(dict) {
    return dict.zero;
  };
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var one2 = function(dict) {
    return dict.one;
  };
  var mul = function(dict) {
    return dict.mul;
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringNumber = {
    sub: numSub,
    Semiring0: function() {
      return semiringNumber;
    }
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero3 = zero2(dictRing.Semiring0());
    return function(a) {
      return sub1(zero3)(a);
    };
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordNumber = /* @__PURE__ */ function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var comparing = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(f) {
      return function(x2) {
        return function(y2) {
          return compare32(f(x2))(f(y2));
        };
      };
    };
  };
  var greaterThan = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare32(a1)(a2);
        if (v instanceof GT) {
          return true;
        }
        ;
        return false;
      };
    };
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare32(a1)(a2);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var lessThan = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare32(a1)(a2);
        if (v instanceof LT) {
          return true;
        }
        ;
        return false;
      };
    };
  };
  var lessThanOrEq = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare32(a1)(a2);
        if (v instanceof GT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var max2 = function(dictOrd) {
    var compare32 = compare(dictOrd);
    return function(x2) {
      return function(y2) {
        var v = compare32(x2)(y2);
        if (v instanceof LT) {
          return y2;
        }
        ;
        if (v instanceof EQ) {
          return x2;
        }
        ;
        if (v instanceof GT) {
          return x2;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top2 = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom2 = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str2 = n.toString();
    return isNaN(str2 + ".0") ? str2 : str2 + ".0";
  };
  var showCharImpl = function(c) {
    var code = c.charCodeAt(0);
    if (code < 32 || code === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty10 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty10;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };
  var cons = function(head6) {
    return function(tail2) {
      return [head6].concat(tail2);
    };
  };
  var intercalate = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showRecordFieldsNil = {
    showRecordFields: function(v) {
      return function(v1) {
        return [];
      };
    }
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record2) {
            var v = showRecordFields1($$Proxy.value)(record2);
            if (v.length === 0) {
              return "{}";
            }
            ;
            return intercalate(" ")(["{", intercalate(", ")(v), "}"]);
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show17 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record2) {
              var tail2 = showRecordFields1($$Proxy.value)(record2);
              var key = reflectSymbol2($$Proxy.value);
              var focus = unsafeGet(key)(record2);
              return cons(intercalate(": ")([key, show17(focus)]))(tail2);
            };
          }
        };
      };
    };
  };

  // output/Data.Generic.Rep/index.js
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var identity7 = /* @__PURE__ */ identity4(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map4 = /* @__PURE__ */ map3(functorMaybe);
  var fromMaybe = function(a) {
    return maybe(a)(identity7);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map4(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var note = function(a) {
    return maybe(new Left(a))(Right.create);
  };
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map5 = /* @__PURE__ */ map3(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var choose = function(dictAlt) {
    var alt9 = alt(dictAlt);
    var map118 = map3(dictAlt.Functor0());
    return function(a) {
      return function(b) {
        return alt9(map118(Left.create)(a))(map118(Right.create)(b));
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map5(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a) {
      return function(f) {
        return f(a);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();
  var monadEither = {
    Applicative0: function() {
      return applicativeEither;
    },
    Bind1: function() {
      return bindEither;
    }
  };

  // output/Data.Identity/index.js
  var Identity = function(x2) {
    return x2;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x2) {
    return Math.min(Math.abs(x2), 2147483647);
  };
  var intDiv = function(x2) {
    return function(y2) {
      if (y2 === 0)
        return 0;
      return y2 > 0 ? Math.floor(x2 / y2) : -Math.floor(x2 / -y2);
    };
  };
  var intMod = function(x2) {
    return function(y2) {
      if (y2 === 0)
        return 0;
      var yy = Math.abs(y2);
      return (x2 % yy + yy) % yy;
    };
  };
  var numDiv = function(n1) {
    return function(n2) {
      return n1 / n2;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingNumber = {
    Ring0: function() {
      return ringNumber;
    }
  };
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingNumber = {
    degree: function(v) {
      return 1;
    },
    div: numDiv,
    mod: function(v) {
      return function(v1) {
        return 0;
      };
    },
    CommutativeRing0: function() {
      return commutativeRingNumber;
    }
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy2 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy2("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy2("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Control.Monad.Rec.Class/index.js
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRecM2 = function(dictMonadRec) {
    var tailRecM1 = tailRecM(dictMonadRec);
    return function(f) {
      return function(a) {
        return function(b) {
          return tailRecM1(function(o) {
            return f(o.a)(o.b);
          })({
            a,
            b
          });
        };
      };
    };
  };
  var tailRec = function(f) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 93, column 3 - line 93, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var bifunctorStep = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Loop) {
            return new Loop(v(v2.value0));
          }
          ;
          if (v2 instanceof Done) {
            return new Done(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 29, column 1 - line 31, column 34): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a) {
      return function() {
        return f(a());
      };
    };
  };
  var pure_ = function(a) {
    return function() {
      return a;
    };
  };
  var bind_ = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  function whileST(f) {
    return function(a) {
      return function() {
        while (f()) {
          a();
        }
      };
    };
  }
  function newSTRef(val2) {
    return function() {
      return { value: val2 };
    };
  }
  var read2 = function(ref) {
    return function() {
      return ref.value;
    };
  };
  var write2 = function(a) {
    return function(ref) {
      return function() {
        return ref.value = a;
      };
    };
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy3 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var functorST = {
    map: map_
  };
  var map6 = /* @__PURE__ */ map3(functorST);
  var $$void2 = /* @__PURE__ */ $$void(functorST);
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy3("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });
  var bind2 = /* @__PURE__ */ bind(bindST);
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindST);
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindST);
  var pure2 = /* @__PURE__ */ pure(applicativeST);
  var monadRecST = {
    tailRecM: function(f) {
      return function(a) {
        var isLooping = function(v) {
          if (v instanceof Loop) {
            return true;
          }
          ;
          return false;
        };
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 69, column 32 - line 69, column 46): " + [v.constructor.name]);
        };
        return bind2(bindFlipped2(newSTRef)(f(a)))(function(r) {
          return discard2(whileST(map6(isLooping)(read2(r)))(bind2(read2(r))(function(v) {
            if (v instanceof Loop) {
              return bind2(f(v.value0))(function(e) {
                return $$void2(write2(e)(r));
              });
            }
            ;
            if (v instanceof Done) {
              return pure2(unit2);
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 61, column 18 - line 65, column 28): " + [v.constructor.name]);
          })))(function() {
            return map6(fromDone)(read2(r));
          });
        });
      };
    },
    Monad0: function() {
      return monadST;
    }
  };

  // output/Data.Array.ST/foreign.js
  function newSTArray() {
    return [];
  }
  var pushAll = function(as3) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as3);
      };
    };
  };
  var unsafeFreeze = function(xs) {
    return function() {
      return xs;
    };
  };
  function copyImpl(xs) {
    return function() {
      return xs.slice();
    };
  }
  var thaw = copyImpl;
  var sortByImpl2 = function() {
    function mergeFromTo(compare6, fromOrdering, xs1, xs2, from7, to) {
      var mid;
      var i;
      var j;
      var k;
      var x2;
      var y2;
      var c;
      mid = from7 + (to - from7 >> 1);
      if (mid - from7 > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, from7, mid);
      if (to - mid > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, mid, to);
      i = from7;
      j = mid;
      k = from7;
      while (i < mid && j < to) {
        x2 = xs2[i];
        y2 = xs2[j];
        c = fromOrdering(compare6(x2)(y2));
        if (c > 0) {
          xs1[k++] = y2;
          ++j;
        } else {
          xs1[k++] = x2;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare6) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare6, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Array.ST/index.js
  var withArray = function(f) {
    return function(xs) {
      return function __do2() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = function(a) {
    return pushAll([a]);
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b) {
    return !b;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a) {
      return function(b) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a))(b);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a) {
            return implies1(f(a))(g(a));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a) {
            return conj1(f(a))(g(a));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a) {
            return disj1(f(a))(g(a));
          };
        };
      },
      not: function(f) {
        return function(a) {
          return not1(f(a));
        };
      }
    };
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init4) {
      return function(xs) {
        var acc = init4;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var empty3 = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var showTuple = function(dictShow) {
    var show11 = show(dictShow);
    return function(dictShow1) {
      var show17 = show(dictShow1);
      return {
        show: function(v) {
          return "(Tuple " + (show11(v.value0) + (" " + (show17(v.value1) + ")")));
        }
      };
    };
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq17 = eq(dictEq);
    return function(dictEq1) {
      var eq18 = eq(dictEq1);
      return {
        eq: function(x2) {
          return function(y2) {
            return eq17(x2.value0)(y2.value0) && eq18(x2.value1)(y2.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare6 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x2) {
          return function(y2) {
            var v = compare6(x2.value0)(y2.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x2.value1)(y2.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap2 = function(dict) {
    return dict.bimap;
  };
  var bifunctorTuple = {
    bimap: function(f) {
      return function(g) {
        return function(v) {
          return new Tuple(f(v.value0), g(v.value1));
        };
      };
    }
  };
  var bifunctorEither = {
    bimap: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Left) {
            return new Left(v(v2.value0));
          }
          ;
          if (v2 instanceof Right) {
            return new Right(v1(v2.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    }
  };

  // output/Data.Monoid.Disj/index.js
  var Disj = function(x2) {
    return x2;
  };
  var semigroupDisj = function(dictHeytingAlgebra) {
    var disj2 = disj(dictHeytingAlgebra);
    return {
      append: function(v) {
        return function(v1) {
          return disj2(v)(v1);
        };
      }
    };
  };
  var monoidDisj = function(dictHeytingAlgebra) {
    var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
    return {
      mempty: ff(dictHeytingAlgebra),
      Semigroup0: function() {
        return semigroupDisj1;
      }
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x2) {
    return x2;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var wrap = function() {
    return coerce2;
  };
  var wrap1 = /* @__PURE__ */ wrap();
  var unwrap = function() {
    return coerce2;
  };
  var alaF = function() {
    return function() {
      return function() {
        return function() {
          return function(v) {
            return coerce2;
          };
        };
      };
    };
  };
  var ala = function() {
    return function() {
      return function() {
        return function(v) {
          return function(f) {
            return coerce2(f(wrap1));
          };
        };
      };
    };
  };

  // output/Data.Foldable/index.js
  var identity8 = /* @__PURE__ */ identity4(categoryFn);
  var alaF2 = /* @__PURE__ */ alaF()()()();
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond7 = applySecond(dictApplicative.Apply0());
    var pure24 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($449) {
          return applySecond7(f($449));
        })(pure24(unit2));
      };
    };
  };
  var sequence_ = function(dictApplicative) {
    var traverse_1 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return traverse_1(dictFoldable)(identity8);
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append10 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go = function(v) {
            return function(x2) {
              if (v.init) {
                return {
                  init: false,
                  acc: x2
                };
              }
              ;
              return {
                init: false,
                acc: append10(v.acc)(append10(sep)(x2))
              };
            };
          };
          return foldl22(go)({
            init: true,
            acc: mempty2
          })(xs).acc;
        };
      };
    };
  };
  var length2 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictSemiring) {
      var add1 = add(dictSemiring);
      var one3 = one2(dictSemiring);
      return foldl22(function(c) {
        return function(v) {
          return add1(one3)(c);
        };
      })(zero2(dictSemiring));
    };
  };
  var foldableTuple = {
    foldr: function(f) {
      return function(z) {
        return function(v) {
          return f(v.value1)(z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(v) {
          return f(z)(v.value1);
        };
      };
    },
    foldMap: function(dictMonoid) {
      return function(f) {
        return function(v) {
          return f(v.value1);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append10 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x2) {
          return function(acc) {
            return append10(f(x2))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var foldM = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonad) {
      var bind20 = bind(dictMonad.Bind1());
      var pure24 = pure(dictMonad.Applicative0());
      return function(f) {
        return function(b0) {
          return foldl22(function(b) {
            return function(a) {
              return bind20(b)(flip(f)(a));
            };
          })(pure24(b0));
        };
      };
    };
  };
  var any = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    return function(dictHeytingAlgebra) {
      return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat22(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply10) {
      return function(map50) {
        return function(pure24) {
          return function(f) {
            return function(array) {
              function go(bot5, top5) {
                switch (top5 - bot5) {
                  case 0:
                    return pure24([]);
                  case 1:
                    return map50(array1)(f(array[bot5]));
                  case 2:
                    return apply10(map50(array2)(f(array[bot5])))(f(array[bot5 + 1]));
                  case 3:
                    return apply10(apply10(map50(array3)(f(array[bot5])))(f(array[bot5 + 1])))(f(array[bot5 + 2]));
                  default:
                    var pivot = bot5 + Math.floor((top5 - bot5) / 4) * 2;
                    return apply10(map50(concat22)(go(bot5, pivot)))(go(pivot, top5));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity9 = /* @__PURE__ */ identity4(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var traversableTuple = {
    traverse: function(dictApplicative) {
      var map50 = map3(dictApplicative.Apply0().Functor0());
      return function(f) {
        return function(v) {
          return map50(Tuple.create(v.value0))(f(v.value1));
        };
      };
    },
    sequence: function(dictApplicative) {
      var map50 = map3(dictApplicative.Apply0().Functor0());
      return function(v) {
        return map50(Tuple.create(v.value0))(v.value1);
      };
    },
    Functor0: function() {
      return functorTuple;
    },
    Foldable1: function() {
      return foldableTuple;
    }
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse23 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse23(dictApplicative)(identity9);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map3(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value2 = b;
              while (true) {
                var maybe2 = f(value2);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value2 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value2 = b;
              while (true) {
                var tuple = f(value2);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value2 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Semigroup.Foldable/index.js
  var foldl1 = function(dict) {
    return dict.foldl1;
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var replicate2 = function(dictUnfoldable) {
    var unfoldr12 = unfoldr(dictUnfoldable);
    return function(n) {
      return function(v) {
        var step2 = function(i) {
          var $17 = i <= 0;
          if ($17) {
            return Nothing.value;
          }
          ;
          return new Just(new Tuple(v, i - 1 | 0));
        };
        return unfoldr12(step2)(n);
      };
    };
  };

  // output/Data.Array/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var zip = /* @__PURE__ */ function() {
    return zipWith(Tuple.create);
  }();
  var updateAt = /* @__PURE__ */ function() {
    return _updateAt(Just.create)(Nothing.value);
  }();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var uncons = /* @__PURE__ */ function() {
    return unconsImpl($$const(Nothing.value))(function(x2) {
      return function(xs) {
        return new Just({
          head: x2,
          tail: xs
        });
      };
    });
  }();
  var take = function(n) {
    return function(xs) {
      var $145 = n < 1;
      if ($145) {
        return [];
      }
      ;
      return slice3(0)(n)(xs);
    };
  };
  var sortBy = function(comp) {
    return sortByImpl(comp)(function(v) {
      if (v instanceof GT) {
        return 1;
      }
      ;
      if (v instanceof EQ) {
        return 0;
      }
      ;
      if (v instanceof LT) {
        return -1 | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.Array (line 829, column 31 - line 832, column 11): " + [v.constructor.name]);
    });
  };
  var sort = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(xs) {
      return sortBy(compare6)(xs);
    };
  };
  var snoc = function(xs) {
    return function(x2) {
      return withArray(push(x2))(xs)();
    };
  };
  var index = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var fromFoldable = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    var eq22 = eq(dictEq);
    return function(x2) {
      return findIndex(function(v) {
        return eq22(v)(x2);
      });
    };
  };
  var notElem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a) {
      return function(arr) {
        return isNothing(elemIndex1(a)(arr));
      };
    };
  };
  var elem2 = function(dictEq) {
    var elemIndex1 = elemIndex(dictEq);
    return function(a) {
      return function(arr) {
        return isJust(elemIndex1(a)(arr));
      };
    };
  };
  var cons2 = function(x2) {
    return function(xs) {
      return append2([x2])(xs);
    };
  };
  var some = function(dictAlternative) {
    var apply12 = apply(dictAlternative.Applicative0().Apply0());
    var map311 = map3(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer5 = defer(dictLazy);
      return function(v) {
        return apply12(map311(cons2)(v))(defer5(function(v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many = function(dictAlternative) {
    var alt9 = alt(dictAlternative.Plus1().Alt0());
    var pure112 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt9(some(dictAlternative)(dictLazy)(v))(pure112([]));
      };
    };
  };

  // output/Data.FoldableWithIndex/index.js
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldlWithIndex = function(dict) {
    return dict.foldlWithIndex;
  };
  var foldMapWithIndex = function(dict) {
    return dict.foldMapWithIndex;
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    var empty10 = empty3(dictPlus);
    return function(a) {
      return new NonEmpty(a, empty10);
    };
  };
  var functorNonEmpty = function(dictFunctor) {
    var map213 = map3(dictFunctor);
    return {
      map: function(f) {
        return function(m) {
          return new NonEmpty(f(m.value0), map213(f)(m.value1));
        };
      }
    };
  };
  var foldableNonEmpty = function(dictFoldable) {
    var foldMap2 = foldMap(dictFoldable);
    var foldl8 = foldl(dictFoldable);
    var foldr8 = foldr(dictFoldable);
    return {
      foldMap: function(dictMonoid) {
        var append15 = append(dictMonoid.Semigroup0());
        var foldMap12 = foldMap2(dictMonoid);
        return function(f) {
          return function(v) {
            return append15(f(v.value0))(foldMap12(f)(v.value1));
          };
        };
      },
      foldl: function(f) {
        return function(b) {
          return function(v) {
            return foldl8(f)(f(b)(v.value0))(v.value1);
          };
        };
      },
      foldr: function(f) {
        return function(b) {
          return function(v) {
            return f(v.value0)(foldr8(f)(b)(v.value1));
          };
        };
      }
    };
  };
  var traversableNonEmpty = function(dictTraversable) {
    var sequence6 = sequence(dictTraversable);
    var traverse5 = traverse(dictTraversable);
    var functorNonEmpty1 = functorNonEmpty(dictTraversable.Functor0());
    var foldableNonEmpty1 = foldableNonEmpty(dictTraversable.Foldable1());
    return {
      sequence: function(dictApplicative) {
        var Apply0 = dictApplicative.Apply0();
        var apply10 = apply(Apply0);
        var map213 = map3(Apply0.Functor0());
        var sequence13 = sequence6(dictApplicative);
        return function(v) {
          return apply10(map213(NonEmpty.create)(v.value0))(sequence13(v.value1));
        };
      },
      traverse: function(dictApplicative) {
        var Apply0 = dictApplicative.Apply0();
        var apply10 = apply(Apply0);
        var map213 = map3(Apply0.Functor0());
        var traverse14 = traverse5(dictApplicative);
        return function(f) {
          return function(v) {
            return apply10(map213(NonEmpty.create)(f(v.value0)))(traverse14(f)(v.value1));
          };
        };
      },
      Functor0: function() {
        return functorNonEmpty1;
      },
      Foldable1: function() {
        return foldableNonEmpty1;
      }
    };
  };
  var foldable1NonEmpty = function(dictFoldable) {
    var foldl8 = foldl(dictFoldable);
    var foldr8 = foldr(dictFoldable);
    var foldableNonEmpty1 = foldableNonEmpty(dictFoldable);
    return {
      foldMap1: function(dictSemigroup) {
        var append15 = append(dictSemigroup);
        return function(f) {
          return function(v) {
            return foldl8(function(s) {
              return function(a1) {
                return append15(s)(f(a1));
              };
            })(f(v.value0))(v.value1);
          };
        };
      },
      foldr1: function(f) {
        return function(v) {
          return maybe(v.value0)(f(v.value0))(foldr8(function(a1) {
            var $250 = maybe(a1)(f(a1));
            return function($251) {
              return Just.create($250($251));
            };
          })(Nothing.value)(v.value1));
        };
      },
      foldl1: function(f) {
        return function(v) {
          return foldl8(f)(v.value0)(v.value1);
        };
      },
      Foldable0: function() {
        return foldableNonEmpty1;
      }
    };
  };
  var foldl12 = function(dictFoldable) {
    return foldl1(foldable1NonEmpty(dictFoldable));
  };

  // output/Data.List.Types/index.js
  var identity10 = /* @__PURE__ */ identity4(categoryFn);
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x2) {
    return x2;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_chunksAcc) {
      return function($copy_v) {
        var $tco_var_chunksAcc = $copy_chunksAcc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(chunksAcc, v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && v.value1.value1 instanceof Cons)) {
            $tco_var_chunksAcc = new Cons(v, chunksAcc);
            $copy_v = v.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v1) {
            if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
              return new Cons(f(v1.value0), new Cons(f(v1.value1.value0), Nil.value));
            }
            ;
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return new Cons(f(v1.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v1 = $copy_v1;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v1, acc) {
                if (v1 instanceof Cons && (v1.value0 instanceof Cons && (v1.value0.value1 instanceof Cons && v1.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v1.value0.value0), new Cons(f(v1.value0.value1.value0), new Cons(f(v1.value0.value1.value1.value0), acc)));
                  return;
                }
                ;
                $tco_done1 = true;
                return acc;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(chunksAcc)(unrolledMap(v));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_chunksAcc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var map7 = /* @__PURE__ */ map3(functorList);
  var functorNonEmptyList = /* @__PURE__ */ functorNonEmpty(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b) {
        var rev = function() {
          var go = function($copy_acc) {
            return function($copy_v) {
              var $tco_var_acc = $copy_acc;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(acc, v) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons) {
                  $tco_var_acc = new Cons(v.value0, acc);
                  $copy_v = v.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [acc.constructor.name, v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_acc, $copy_v);
              }
              ;
              return $tco_result;
            };
          };
          return go(Nil.value);
        }();
        var $281 = foldl(foldableList)(flip(f))(b);
        return function($282) {
          return $281(rev($282));
        };
      };
    },
    foldl: function(f) {
      var go = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go;
    },
    foldMap: function(dictMonoid) {
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $283 = append23(acc);
          return function($284) {
            return $283(f($284));
          };
        })(mempty2);
      };
    }
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var foldableNonEmptyList = /* @__PURE__ */ foldableNonEmpty(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var monoidList = /* @__PURE__ */ function() {
    return {
      mempty: Nil.value,
      Semigroup0: function() {
        return semigroupList;
      }
    };
  }();
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var traversableList = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      var map118 = map3(Apply0.Functor0());
      var lift26 = lift2(Apply0);
      var pure112 = pure(dictApplicative);
      return function(f) {
        var $298 = map118(foldl2(flip(Cons.create))(Nil.value));
        var $299 = foldl2(function(acc) {
          var $301 = lift26(flip(Cons.create))(acc);
          return function($302) {
            return $301(f($302));
          };
        })(pure112(Nil.value));
        return function($300) {
          return $298($299($300));
        };
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableList)(dictApplicative)(identity10);
    },
    Functor0: function() {
      return functorList;
    },
    Foldable1: function() {
      return foldableList;
    }
  };
  var traversableNonEmptyList = /* @__PURE__ */ traversableNonEmpty(traversableList);
  var unfoldable1List = {
    unfoldr1: function(f) {
      return function(b) {
        var go = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source2, memo) {
              var v = f(source2);
              if (v.value1 instanceof Just) {
                $tco_var_source = v.value1.value0;
                $copy_memo = new Cons(v.value0, memo);
                return;
              }
              ;
              if (v.value1 instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go(b)(Nil.value);
      };
    }
  };
  var unfoldableList = {
    unfoldr: function(f) {
      return function(b) {
        var go = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source2, memo) {
              var v = f(source2);
              if (v instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(memo);
              }
              ;
              if (v instanceof Just) {
                $tco_var_source = v.value0.value1;
                $copy_memo = new Cons(v.value0.value0, memo);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go(b)(Nil.value);
      };
    },
    Unfoldable10: function() {
      return unfoldable1List;
    }
  };
  var applyList = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(map7(v.value0)(v1))(apply(applyList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 157, column 1 - line 159, column 48): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorList;
    }
  };
  var apply2 = /* @__PURE__ */ apply(applyList);
  var applyNonEmptyList = {
    apply: function(v) {
      return function(v1) {
        return new NonEmpty(v.value0(v1.value0), append1(apply2(v.value1)(new Cons(v1.value0, Nil.value)))(apply2(new Cons(v.value0, v.value1))(v1.value1)));
      };
    },
    Functor0: function() {
      return functorNonEmptyList;
    }
  };
  var bindList = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v instanceof Cons) {
          return append1(v1(v.value0))(bind(bindList)(v.value1)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.List.Types (line 164, column 1 - line 166, column 37): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyList;
    }
  };
  var applicativeList = {
    pure: function(a) {
      return new Cons(a, Nil.value);
    },
    Apply0: function() {
      return applyList;
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();
  var applicativeNonEmptyList = {
    pure: /* @__PURE__ */ function() {
      var $312 = singleton2(plusList);
      return function($313) {
        return NonEmptyList($312($313));
      };
    }(),
    Apply0: function() {
      return applyNonEmptyList;
    }
  };

  // output/Data.List/index.js
  var map8 = /* @__PURE__ */ map3(functorMaybe);
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var eq3 = /* @__PURE__ */ eq(eqOrdering);
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var bimap3 = /* @__PURE__ */ bimap2(bifunctorStep);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var bind3 = /* @__PURE__ */ bind(bindList);
  var identity11 = /* @__PURE__ */ identity4(categoryFn);
  var updateAt2 = function(v) {
    return function(v1) {
      return function(v2) {
        if (v === 0 && v2 instanceof Cons) {
          return new Just(new Cons(v1, v2.value1));
        }
        ;
        if (v2 instanceof Cons) {
          return map8(function(v3) {
            return new Cons(v2.value0, v3);
          })(updateAt2(v - 1 | 0)(v1)(v2.value1));
        }
        ;
        return Nothing.value;
      };
    };
  };
  var unzip = /* @__PURE__ */ function() {
    return foldr3(function(v) {
      return function(v1) {
        return new Tuple(new Cons(v.value0, v1.value0), new Cons(v.value1, v1.value1));
      };
    })(new Tuple(Nil.value, Nil.value));
  }();
  var uncons2 = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map8(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons2(xs));
    });
  };
  var span = function(v) {
    return function(v1) {
      if (v1 instanceof Cons && v(v1.value0)) {
        var v2 = span(v)(v1.value1);
        return {
          init: new Cons(v1.value0, v2.init),
          rest: v2.rest
        };
      }
      ;
      return {
        init: Nil.value,
        rest: v1
      };
    };
  };
  var snoc2 = function(xs) {
    return function(x2) {
      return foldr3(Cons.create)(new Cons(x2, Nil.value))(xs);
    };
  };
  var singleton3 = function(a) {
    return new Cons(a, Nil.value);
  };
  var sortBy2 = function(cmp) {
    var merge = function(v) {
      return function(v1) {
        if (v instanceof Cons && v1 instanceof Cons) {
          if (eq3(cmp(v.value0)(v1.value0))(GT.value)) {
            return new Cons(v1.value0, merge(v)(v1.value1));
          }
          ;
          if (otherwise) {
            return new Cons(v.value0, merge(v.value1)(v1));
          }
          ;
        }
        ;
        if (v instanceof Nil) {
          return v1;
        }
        ;
        if (v1 instanceof Nil) {
          return v;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 466, column 3 - line 466, column 38): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    var mergePairs = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        return new Cons(merge(v.value0)(v.value1.value0), mergePairs(v.value1.value1));
      }
      ;
      return v;
    };
    var mergeAll = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Cons && v.value1 instanceof Nil) {
          $tco_done = true;
          return v.value0;
        }
        ;
        $copy_v = mergePairs(v);
        return;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    var sequences = function(v) {
      if (v instanceof Cons && v.value1 instanceof Cons) {
        if (eq3(cmp(v.value0)(v.value1.value0))(GT.value)) {
          return descending(v.value1.value0)(singleton3(v.value0))(v.value1.value1);
        }
        ;
        if (otherwise) {
          return ascending3(v.value1.value0)(function(v1) {
            return new Cons(v.value0, v1);
          })(v.value1.value1);
        }
        ;
      }
      ;
      return singleton3(v);
    };
    var descending = function($copy_a) {
      return function($copy_as) {
        return function($copy_v) {
          var $tco_var_a = $copy_a;
          var $tco_var_as = $copy_as;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(a, as3, v) {
            if (v instanceof Cons && eq3(cmp(a)(v.value0))(GT.value)) {
              $tco_var_a = v.value0;
              $tco_var_as = new Cons(a, as3);
              $copy_v = v.value1;
              return;
            }
            ;
            $tco_done1 = true;
            return new Cons(new Cons(a, as3), sequences(v));
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
    };
    var ascending3 = function($copy_a) {
      return function($copy_as) {
        return function($copy_v) {
          var $tco_var_a = $copy_a;
          var $tco_var_as = $copy_as;
          var $tco_done2 = false;
          var $tco_result;
          function $tco_loop(a, as3, v) {
            if (v instanceof Cons && notEq2(cmp(a)(v.value0))(GT.value)) {
              $tco_var_a = v.value0;
              $tco_var_as = function(ys) {
                return as3(new Cons(a, ys));
              };
              $copy_v = v.value1;
              return;
            }
            ;
            $tco_done2 = true;
            return new Cons(as3(singleton3(a)), sequences(v));
          }
          ;
          while (!$tco_done2) {
            $tco_result = $tco_loop($tco_var_a, $tco_var_as, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
    };
    return function($431) {
      return mergeAll(sequences($431));
    };
  };
  var reverse2 = /* @__PURE__ */ function() {
    var go = function($copy_acc) {
      return function($copy_v) {
        var $tco_var_acc = $copy_acc;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(acc, v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return acc;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_acc = new Cons(v.value0, acc);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [acc.constructor.name, v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_acc, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go(Nil.value);
  }();
  var unsnoc = function(lst) {
    var go = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons && v.value1 instanceof Nil) {
            $tco_done = true;
            return new Just({
              revInit: v1,
              last: v.value0
            });
          }
          ;
          if (v instanceof Cons) {
            $tco_var_v = v.value1;
            $copy_v1 = new Cons(v.value0, v1);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 270, column 3 - line 270, column 21): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return map8(function(h) {
      return {
        init: reverse2(h.revInit),
        last: h.last
      };
    })(go(lst)(Nil.value));
  };
  var zipWith2 = function(f) {
    return function(xs) {
      return function(ys) {
        var go = function($copy_v) {
          return function($copy_v1) {
            return function($copy_acc) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, acc) {
                if (v instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return acc;
                }
                ;
                if (v instanceof Cons && v1 instanceof Cons) {
                  $tco_var_v = v.value1;
                  $tco_var_v1 = v1.value1;
                  $copy_acc = new Cons(f(v.value0)(v1.value0), acc);
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List (line 779, column 3 - line 779, column 21): " + [v.constructor.name, v1.constructor.name, acc.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_acc);
              }
              ;
              return $tco_result;
            };
          };
        };
        return reverse2(go(xs)(ys)(Nil.value));
      };
    };
  };
  var zip2 = /* @__PURE__ */ function() {
    return zipWith2(Tuple.create);
  }();
  var range3 = function(start2) {
    return function(end) {
      if (start2 === end) {
        return singleton3(start2);
      }
      ;
      if (otherwise) {
        var go = function($copy_s) {
          return function($copy_e) {
            return function($copy_step) {
              return function($copy_rest) {
                var $tco_var_s = $copy_s;
                var $tco_var_e = $copy_e;
                var $tco_var_step = $copy_step;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(s, e, step2, rest) {
                  if (s === e) {
                    $tco_done = true;
                    return new Cons(s, rest);
                  }
                  ;
                  if (otherwise) {
                    $tco_var_s = s + step2 | 0;
                    $tco_var_e = e;
                    $tco_var_step = step2;
                    $copy_rest = new Cons(s, rest);
                    return;
                  }
                  ;
                  throw new Error("Failed pattern match at Data.List (line 148, column 3 - line 149, column 65): " + [s.constructor.name, e.constructor.name, step2.constructor.name, rest.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_s, $tco_var_e, $tco_var_step, $copy_rest);
                }
                ;
                return $tco_result;
              };
            };
          };
        };
        return go(end)(start2)(function() {
          var $312 = start2 > end;
          if ($312) {
            return 1;
          }
          ;
          return -1 | 0;
        }())(Nil.value);
      }
      ;
      throw new Error("Failed pattern match at Data.List (line 144, column 1 - line 144, column 32): " + [start2.constructor.name, end.constructor.name]);
    };
  };
  var manyRec = function(dictMonadRec) {
    var bind110 = bind(dictMonadRec.Monad0().Bind1());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(dictAlternative) {
      var Alt0 = dictAlternative.Plus1().Alt0();
      var alt9 = alt(Alt0);
      var map118 = map3(Alt0.Functor0());
      var pure24 = pure(dictAlternative.Applicative0());
      return function(p) {
        var go = function(acc) {
          return bind110(alt9(map118(Loop.create)(p))(pure24(new Done(unit2))))(function(aa) {
            return pure24(bimap3(function(v) {
              return new Cons(v, acc);
            })(function(v) {
              return reverse2(acc);
            })(aa));
          });
        };
        return tailRecM4(go)(Nil.value);
      };
    };
  };
  var some2 = function(dictAlternative) {
    var apply10 = apply(dictAlternative.Applicative0().Apply0());
    var map118 = map3(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer5 = defer(dictLazy);
      return function(v) {
        return apply10(map118(Cons.create)(v))(defer5(function(v1) {
          return many2(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many2 = function(dictAlternative) {
    var alt9 = alt(dictAlternative.Plus1().Alt0());
    var pure24 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt9(some2(dictAlternative)(dictLazy)(v))(pure24(Nil.value));
      };
    };
  };
  var length3 = /* @__PURE__ */ foldl3(function(acc) {
    return function(v) {
      return acc + 1 | 0;
    };
  })(0);
  var index2 = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v instanceof Nil) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Cons && v1 === 0) {
          $tco_done = true;
          return new Just(v.value0);
        }
        ;
        if (v instanceof Cons) {
          $tco_var_v = v.value1;
          $copy_v1 = v1 - 1 | 0;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 281, column 1 - line 281, column 44): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  var head = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [v.constructor.name]);
  };
  var groupBy = function(v) {
    return function(v1) {
      if (v1 instanceof Nil) {
        return Nil.value;
      }
      ;
      if (v1 instanceof Cons) {
        var v2 = span(v(v1.value0))(v1.value1);
        return new Cons(new NonEmpty(v1.value0, v2.init), groupBy(v)(v2.rest));
      }
      ;
      throw new Error("Failed pattern match at Data.List (line 609, column 1 - line 609, column 80): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var fromFoldable2 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nil) {
          return Nil.value;
        }
        ;
        if (v2 instanceof Cons && v(v1)(v2.value0)) {
          return v2.value1;
        }
        ;
        if (v2 instanceof Cons) {
          return new Cons(v2.value0, deleteBy(v)(v1)(v2.value1));
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 732, column 1 - line 732, column 67): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var $$delete = function(dictEq) {
    return deleteBy(eq(dictEq));
  };
  var difference = function(dictEq) {
    return foldl3(flip($$delete(dictEq)));
  };
  var concat2 = function(v) {
    return bind3(v)(identity11);
  };

  // output/Data.Lazy/foreign.js
  var defer2 = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l) {
    return l();
  };

  // output/Data.Lazy/index.js
  var functorLazy = {
    map: function(f) {
      return function(l) {
        return defer2(function(v) {
          return f(force(l));
        });
      };
    }
  };

  // output/Data.List.Lazy.Types/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var List = function(x2) {
    return x2;
  };
  var Nil2 = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons2 = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var nil = /* @__PURE__ */ defer2(function(v) {
    return Nil2.value;
  });
  var step = function($319) {
    return force(unwrap2($319));
  };
  var lazyList = {
    defer: function(f) {
      return defer2(function($320) {
        return step(f($320));
      });
    }
  };
  var defer3 = /* @__PURE__ */ defer(lazyList);
  var cons3 = function(x2) {
    return function(xs) {
      return defer2(function(v) {
        return new Cons2(x2, xs);
      });
    };
  };
  var foldableList2 = {
    foldr: function(op) {
      return function(z) {
        return function(xs) {
          var rev = foldl(foldableList2)(flip(cons3))(nil);
          return foldl(foldableList2)(flip(op))(z)(rev(xs));
        };
      };
    },
    foldl: function(op) {
      var go = function($copy_b) {
        return function($copy_xs) {
          var $tco_var_b = $copy_b;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(b, xs) {
            var v = step(xs);
            if (v instanceof Nil2) {
              $tco_done = true;
              return b;
            }
            ;
            if (v instanceof Cons2) {
              $tco_var_b = op(b)(v.value0);
              $copy_xs = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Lazy.Types (line 127, column 7 - line 129, column 40): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_b, $copy_xs);
          }
          ;
          return $tco_result;
        };
      };
      return go;
    },
    foldMap: function(dictMonoid) {
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList2)(function(b) {
          return function(a) {
            return append23(b)(f(a));
          };
        })(mempty2);
      };
    }
  };
  var unfoldable1List2 = {
    unfoldr1: /* @__PURE__ */ function() {
      var go = function(f) {
        return function(b) {
          return defer3(function(v) {
            var v1 = f(b);
            if (v1.value1 instanceof Just) {
              return cons3(v1.value0)(go(f)(v1.value1.value0));
            }
            ;
            if (v1.value1 instanceof Nothing) {
              return cons3(v1.value0)(nil);
            }
            ;
            throw new Error("Failed pattern match at Data.List.Lazy.Types (line 151, column 28 - line 153, column 33): " + [v1.constructor.name]);
          });
        };
      };
      return go;
    }()
  };
  var unfoldableList2 = {
    unfoldr: /* @__PURE__ */ function() {
      var go = function(f) {
        return function(b) {
          return defer3(function(v) {
            var v1 = f(b);
            if (v1 instanceof Nothing) {
              return nil;
            }
            ;
            if (v1 instanceof Just) {
              return cons3(v1.value0.value0)(go(f)(v1.value0.value1));
            }
            ;
            throw new Error("Failed pattern match at Data.List.Lazy.Types (line 157, column 28 - line 159, column 39): " + [v1.constructor.name]);
          });
        };
      };
      return go;
    }(),
    Unfoldable10: function() {
      return unfoldable1List2;
    }
  };

  // output/Data.List.Lazy/index.js
  var map9 = /* @__PURE__ */ map3(functorLazy);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var filter3 = function(p) {
    var go = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Nil2) {
          $tco_done = true;
          return Nil2.value;
        }
        ;
        if (v instanceof Cons2) {
          if (p(v.value0)) {
            $tco_done = true;
            return new Cons2(v.value0, filter3(p)(v.value1));
          }
          ;
          if (otherwise) {
            $copy_v = step(v.value1);
            return;
          }
          ;
        }
        ;
        throw new Error("Failed pattern match at Data.List.Lazy (line 416, column 3 - line 416, column 15): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    var $342 = map9(go);
    return function($343) {
      return List($342(unwrap3($343)));
    };
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.Map.Internal/index.js
  var identity12 = /* @__PURE__ */ identity4(categoryFn);
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Two2(value0, value1, value2, value3);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value2, value3, value4, value5, value6) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
      this.value6 = value6;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return function(value6) {
                  return new Three2(value0, value1, value2, value3, value4, value5, value6);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoLeft2(value0, value1, value2);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new TwoRight2(value0, value1, value2);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeLeft2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeMiddle2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value2, value3, value4, value5) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
      this.value4 = value4;
      this.value5 = value5;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return function(value4) {
              return function(value5) {
                return new ThreeRight2(value0, value1, value2, value3, value4, value5);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new KickUp2(value0, value1, value2, value3);
          };
        };
      };
    };
    return KickUp2;
  }();
  var size = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Two) {
      return (1 + size(v.value0) | 0) + size(v.value3) | 0;
    }
    ;
    if (v instanceof Three) {
      return ((2 + size(v.value0) | 0) + size(v.value3) | 0) + size(v.value6) | 0;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 705, column 1 - line 705, column 35): " + [v.constructor.name]);
  };
  var singleton4 = function(k) {
    return function(v) {
      return new Two(Leaf.value, k, v, Leaf.value);
    };
  };
  var toUnfoldable2 = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return function(m) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof Leaf) {
              $copy_v = v.value1;
              return;
            }
            ;
            if (v.value0 instanceof Two && (v.value0.value0 instanceof Leaf && v.value0.value3 instanceof Leaf)) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), v.value1));
            }
            ;
            if (v.value0 instanceof Two && v.value0.value0 instanceof Leaf) {
              $tco_done = true;
              return new Just(new Tuple(new Tuple(v.value0.value1, v.value0.value2), new Cons(v.value0.value3, v.value1)));
            }
            ;
            if (v.value0 instanceof Two) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton4(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, v.value1)));
              return;
            }
            ;
            if (v.value0 instanceof Three) {
              $copy_v = new Cons(v.value0.value0, new Cons(singleton4(v.value0.value1)(v.value0.value2), new Cons(v.value0.value3, new Cons(singleton4(v.value0.value4)(v.value0.value5), new Cons(v.value0.value6, v.value1)))));
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 624, column 18 - line 633, column 71): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 623, column 3 - line 623, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return unfoldr3(go)(new Cons(m, Nil.value));
    };
  };
  var toUnfoldable1 = /* @__PURE__ */ toUnfoldable2(unfoldableList2);
  var toUnfoldable22 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var toAscArray = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var lookup = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare6(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare6(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare6(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go;
    };
  };
  var member = function(dictOrd) {
    var lookup1 = lookup(dictOrd);
    return function(k) {
      return function(m) {
        return isJust(lookup1(k)(m));
      };
    };
  };
  var isEmpty = function(v) {
    if (v instanceof Leaf) {
      return true;
    }
    ;
    return false;
  };
  var functorMap = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v1 instanceof Two) {
          return new Two(map3(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map3(functorMap)(v)(v1.value3));
        }
        ;
        if (v1 instanceof Three) {
          return new Three(map3(functorMap)(v)(v1.value0), v1.value1, v(v1.value2), map3(functorMap)(v)(v1.value3), v1.value4, v(v1.value5), map3(functorMap)(v)(v1.value6));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 116, column 1 - line 119, column 110): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_tree) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, tree) {
          if (v instanceof Nil) {
            $tco_done = true;
            return tree;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(tree, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Two(v.value0.value0, v.value0.value1, v.value0.value2, tree);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(tree, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, tree, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_tree = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, tree);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, tree.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_tree);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare6 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_ctx) {
          return function($copy_v1) {
            var $tco_var_ctx = $copy_ctx;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(ctx, v1) {
              if (v1 instanceof Leaf) {
                $tco_done1 = true;
                return up(ctx)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v1 instanceof Two) {
                var v2 = compare6(k)(v1.value1);
                if (v2 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Two(v1.value0, k, v, v1.value3));
                }
                ;
                if (v2 instanceof LT) {
                  $tco_var_ctx = new Cons(new TwoLeft(v1.value1, v1.value2, v1.value3), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new TwoRight(v1.value0, v1.value1, v1.value2), ctx);
                $copy_v1 = v1.value3;
                return;
              }
              ;
              if (v1 instanceof Three) {
                var v3 = compare6(k)(v1.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, k, v, v1.value3, v1.value4, v1.value5, v1.value6));
                }
                ;
                var v4 = compare6(k)(v1.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(ctx)(new Three(v1.value0, v1.value1, v1.value2, v1.value3, k, v, v1.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeLeft(v1.value1, v1.value2, v1.value3, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_ctx = new Cons(new ThreeMiddle(v1.value0, v1.value1, v1.value2, v1.value4, v1.value5, v1.value6), ctx);
                  $copy_v1 = v1.value3;
                  return;
                }
                ;
                $tco_var_ctx = new Cons(new ThreeRight(v1.value0, v1.value1, v1.value2, v1.value3, v1.value4, v1.value5), ctx);
                $copy_v1 = v1.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [ctx.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_ctx, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare6 = compare(dictOrd);
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare6(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max5 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max5.key, max5.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = compare6(k)(m.value4);
              var v3 = compare6(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max5 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max5.key, max5.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max5 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max5.key, max5.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append23 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append23(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append23(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append23(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append23(f(m.value2))(append23(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append23(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldrWithIndex(foldableWithIndexMap)(f)(f(m.value1)(m.value2)(foldrWithIndex(foldableWithIndexMap)(f)(f(m.value4)(m.value5)(foldrWithIndex(foldableWithIndexMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 147, column 26 - line 150, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldlWithIndex(foldableWithIndexMap)(f)(f(m.value4)(foldlWithIndex(foldableWithIndexMap)(f)(f(m.value1)(foldlWithIndex(foldableWithIndexMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 151, column 26 - line 154, column 120): " + [m.constructor.name]);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append23 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty2;
          }
          ;
          if (m instanceof Two) {
            return append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append23(f(m.value1)(m.value2))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value0))(append23(f(m.value1)(m.value2))(append23(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value3))(append23(f(m.value4)(m.value5))(foldMapWithIndex(foldableWithIndexMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 155, column 26 - line 158, column 128): " + [m.constructor.name]);
        };
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var foldrWithIndex2 = /* @__PURE__ */ foldrWithIndex(foldableWithIndexMap);
  var foldlWithIndex2 = /* @__PURE__ */ foldlWithIndex(foldableWithIndexMap);
  var keys = /* @__PURE__ */ function() {
    return foldrWithIndex2(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  }();
  var traversableMap = {
    traverse: function(dictApplicative) {
      var pure112 = pure(dictApplicative);
      var Apply0 = dictApplicative.Apply0();
      var apply10 = apply(Apply0);
      var map118 = map3(Apply0.Functor0());
      return function(v) {
        return function(v1) {
          if (v1 instanceof Leaf) {
            return pure112(Leaf.value);
          }
          ;
          if (v1 instanceof Two) {
            return apply10(apply10(apply10(map118(Two.create)(traverse(traversableMap)(dictApplicative)(v)(v1.value0)))(pure112(v1.value1)))(v(v1.value2)))(traverse(traversableMap)(dictApplicative)(v)(v1.value3));
          }
          ;
          if (v1 instanceof Three) {
            return apply10(apply10(apply10(apply10(apply10(apply10(map118(Three.create)(traverse(traversableMap)(dictApplicative)(v)(v1.value0)))(pure112(v1.value1)))(v(v1.value2)))(traverse(traversableMap)(dictApplicative)(v)(v1.value3)))(pure112(v1.value4)))(v(v1.value5)))(traverse(traversableMap)(dictApplicative)(v)(v1.value6));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 160, column 1 - line 175, column 31): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    },
    sequence: function(dictApplicative) {
      return traverse(traversableMap)(dictApplicative)(identity12);
    },
    Functor0: function() {
      return functorMap;
    },
    Foldable1: function() {
      return foldableMap;
    }
  };
  var values = /* @__PURE__ */ function() {
    return foldr(foldableMap)(Cons.create)(Nil.value);
  }();
  var eqMap = function(dictEq) {
    var eqTuple2 = eqTuple(dictEq);
    return function(dictEq1) {
      var eq17 = eq(eqArray(eqTuple2(dictEq1)));
      return {
        eq: function(m1) {
          return function(m2) {
            return eq17(toAscArray(m1))(toAscArray(m2));
          };
        }
      };
    };
  };
  var empty4 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable3 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert1(v.value0)(v.value1)(m);
        };
      })(empty4);
    };
  };
  var filterWithKey = function(dictOrd) {
    var fromFoldable17 = fromFoldable3(dictOrd)(foldableList2);
    return function(predicate) {
      var $923 = filter3(uncurry(predicate));
      return function($924) {
        return fromFoldable17($923(toUnfoldable1($924)));
      };
    };
  };
  var filterKeys = function(dictOrd) {
    var filterWithKey1 = filterWithKey(dictOrd);
    return function(predicate) {
      return filterWithKey1(function($925) {
        return $$const(predicate($925));
      });
    };
  };
  var intersectionWith = function(dictOrd) {
    var compare6 = compare(dictOrd);
    var insert1 = insert(dictOrd);
    return function(f) {
      return function(m1) {
        return function(m2) {
          var go = function($copy_v) {
            return function($copy_v1) {
              return function($copy_m) {
                var $tco_var_v = $copy_v;
                var $tco_var_v1 = $copy_v1;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v, v1, m) {
                  if (v instanceof Nil) {
                    $tco_done = true;
                    return m;
                  }
                  ;
                  if (v1 instanceof Nil) {
                    $tco_done = true;
                    return m;
                  }
                  ;
                  if (v instanceof Cons && v1 instanceof Cons) {
                    var v2 = compare6(v.value0.value0)(v1.value0.value0);
                    if (v2 instanceof LT) {
                      $tco_var_v = v.value1;
                      $tco_var_v1 = v1;
                      $copy_m = m;
                      return;
                    }
                    ;
                    if (v2 instanceof EQ) {
                      $tco_var_v = v.value1;
                      $tco_var_v1 = v1.value1;
                      $copy_m = insert1(v.value0.value0)(f(v.value0.value1)(v1.value0.value1))(m);
                      return;
                    }
                    ;
                    if (v2 instanceof GT) {
                      $tco_var_v = v;
                      $tco_var_v1 = v1.value1;
                      $copy_m = m;
                      return;
                    }
                    ;
                    throw new Error("Failed pattern match at Data.Map.Internal (line 684, column 5 - line 687, column 27): " + [v2.constructor.name]);
                  }
                  ;
                  throw new Error("Failed pattern match at Data.Map.Internal (line 681, column 3 - line 681, column 17): " + [v.constructor.name, v1.constructor.name, m.constructor.name]);
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_m);
                }
                ;
                return $tco_result;
              };
            };
          };
          return go(toUnfoldable22(m1))(toUnfoldable22(m2))(empty4);
        };
      };
    };
  };
  var $$delete2 = function(dictOrd) {
    var pop1 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop1(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup1 = lookup(dictOrd);
    var delete1 = $$delete2(dictOrd);
    var insert1 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup1(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert1(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };
  var unionWith = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(m1) {
        return function(m2) {
          var go = function(k) {
            return function(m) {
              return function(v) {
                return alter1(function() {
                  var $932 = maybe(v)(f(v));
                  return function($933) {
                    return Just.create($932($933));
                  };
                }())(k)(m);
              };
            };
          };
          return foldlWithIndex2(go)(m2)(m1);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var update = function(dictOrd) {
    var alter1 = alter(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          return alter1(maybe(Nothing.value)(f))(k)(m);
        };
      };
    };
  };

  // output/Data.Profunctor/index.js
  var profunctorFn = {
    dimap: function(a2b) {
      return function(c2d) {
        return function(b2c) {
          return function($18) {
            return c2d(b2c(a2b($18)));
          };
        };
      };
    }
  };
  var dimap = function(dict) {
    return dict.dimap;
  };

  // output/Data.Profunctor.Strong/index.js
  var strongFn = {
    first: function(a2b) {
      return function(v) {
        return new Tuple(a2b(v.value0), v.value1);
      };
    },
    second: /* @__PURE__ */ map3(functorTuple),
    Profunctor0: function() {
      return profunctorFn;
    }
  };
  var second = function(dict) {
    return dict.second;
  };
  var first = function(dict) {
    return dict.first;
  };
  var splitStrong = function(dictCategory) {
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    return function(dictStrong) {
      var first1 = first(dictStrong);
      var second1 = second(dictStrong);
      return function(l) {
        return function(r) {
          return composeFlipped2(first1(l))(second1(r));
        };
      };
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var top3 = /* @__PURE__ */ top2(boundedInt);
  var bottom3 = /* @__PURE__ */ bottom2(boundedInt);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom1 = bottom2(dictBoundedEnum.Bounded0());
    return function(low) {
      return function(high) {
        return function(x2) {
          var v = toEnum1(x2);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x2 < fromEnum1(bottom1);
            if ($140) {
              return low;
            }
            ;
            return high;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a) {
        return toEnum$prime(fromEnum$prime(a) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= bottom3 && v <= top3) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top2(boundedChar)) - toCharCode(bottom2(boundedChar)) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var fromCharCode2 = /* @__PURE__ */ toEnum(boundedEnumChar);

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };
  var quot = function(x2) {
    return function(y2) {
      return x2 / y2 | 0;
    };
  };
  var rem = function(x2) {
    return function(y2) {
      return x2 % y2;
    };
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var ceil = Math.ceil;
  var floor = Math.floor;
  var log = Math.log;
  var pow = function(n) {
    return function(p) {
      return Math.pow(n, p);
    };
  };

  // output/Data.Int/index.js
  var top4 = /* @__PURE__ */ top2(boundedInt);
  var bottom4 = /* @__PURE__ */ bottom2(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x2) {
    if (!isFiniteImpl(x2)) {
      return 0;
    }
    ;
    if (x2 >= toNumber(top4)) {
      return top4;
    }
    ;
    if (x2 <= toNumber(bottom4)) {
      return bottom4;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x2));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x2.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };
  var ceil2 = function($40) {
    return unsafeClamp(ceil($40));
  };

  // output/Data.CodePoint.Unicode.Internal/index.js
  var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqInt);
  var NUMCAT_LU = /* @__PURE__ */ function() {
    function NUMCAT_LU2() {
    }
    ;
    NUMCAT_LU2.value = new NUMCAT_LU2();
    return NUMCAT_LU2;
  }();
  var NUMCAT_LL = /* @__PURE__ */ function() {
    function NUMCAT_LL2() {
    }
    ;
    NUMCAT_LL2.value = new NUMCAT_LL2();
    return NUMCAT_LL2;
  }();
  var NUMCAT_LT = /* @__PURE__ */ function() {
    function NUMCAT_LT2() {
    }
    ;
    NUMCAT_LT2.value = new NUMCAT_LT2();
    return NUMCAT_LT2;
  }();
  var NUMCAT_LM = /* @__PURE__ */ function() {
    function NUMCAT_LM2() {
    }
    ;
    NUMCAT_LM2.value = new NUMCAT_LM2();
    return NUMCAT_LM2;
  }();
  var NUMCAT_LO = /* @__PURE__ */ function() {
    function NUMCAT_LO2() {
    }
    ;
    NUMCAT_LO2.value = new NUMCAT_LO2();
    return NUMCAT_LO2;
  }();
  var NUMCAT_MN = /* @__PURE__ */ function() {
    function NUMCAT_MN2() {
    }
    ;
    NUMCAT_MN2.value = new NUMCAT_MN2();
    return NUMCAT_MN2;
  }();
  var NUMCAT_MC = /* @__PURE__ */ function() {
    function NUMCAT_MC2() {
    }
    ;
    NUMCAT_MC2.value = new NUMCAT_MC2();
    return NUMCAT_MC2;
  }();
  var NUMCAT_ME = /* @__PURE__ */ function() {
    function NUMCAT_ME2() {
    }
    ;
    NUMCAT_ME2.value = new NUMCAT_ME2();
    return NUMCAT_ME2;
  }();
  var NUMCAT_ND = /* @__PURE__ */ function() {
    function NUMCAT_ND2() {
    }
    ;
    NUMCAT_ND2.value = new NUMCAT_ND2();
    return NUMCAT_ND2;
  }();
  var NUMCAT_NL = /* @__PURE__ */ function() {
    function NUMCAT_NL2() {
    }
    ;
    NUMCAT_NL2.value = new NUMCAT_NL2();
    return NUMCAT_NL2;
  }();
  var NUMCAT_NO = /* @__PURE__ */ function() {
    function NUMCAT_NO2() {
    }
    ;
    NUMCAT_NO2.value = new NUMCAT_NO2();
    return NUMCAT_NO2;
  }();
  var NUMCAT_PC = /* @__PURE__ */ function() {
    function NUMCAT_PC2() {
    }
    ;
    NUMCAT_PC2.value = new NUMCAT_PC2();
    return NUMCAT_PC2;
  }();
  var NUMCAT_PD = /* @__PURE__ */ function() {
    function NUMCAT_PD2() {
    }
    ;
    NUMCAT_PD2.value = new NUMCAT_PD2();
    return NUMCAT_PD2;
  }();
  var NUMCAT_PS = /* @__PURE__ */ function() {
    function NUMCAT_PS2() {
    }
    ;
    NUMCAT_PS2.value = new NUMCAT_PS2();
    return NUMCAT_PS2;
  }();
  var NUMCAT_PE = /* @__PURE__ */ function() {
    function NUMCAT_PE2() {
    }
    ;
    NUMCAT_PE2.value = new NUMCAT_PE2();
    return NUMCAT_PE2;
  }();
  var NUMCAT_PI = /* @__PURE__ */ function() {
    function NUMCAT_PI2() {
    }
    ;
    NUMCAT_PI2.value = new NUMCAT_PI2();
    return NUMCAT_PI2;
  }();
  var NUMCAT_PF = /* @__PURE__ */ function() {
    function NUMCAT_PF2() {
    }
    ;
    NUMCAT_PF2.value = new NUMCAT_PF2();
    return NUMCAT_PF2;
  }();
  var NUMCAT_PO = /* @__PURE__ */ function() {
    function NUMCAT_PO2() {
    }
    ;
    NUMCAT_PO2.value = new NUMCAT_PO2();
    return NUMCAT_PO2;
  }();
  var NUMCAT_SM = /* @__PURE__ */ function() {
    function NUMCAT_SM2() {
    }
    ;
    NUMCAT_SM2.value = new NUMCAT_SM2();
    return NUMCAT_SM2;
  }();
  var NUMCAT_SC = /* @__PURE__ */ function() {
    function NUMCAT_SC2() {
    }
    ;
    NUMCAT_SC2.value = new NUMCAT_SC2();
    return NUMCAT_SC2;
  }();
  var NUMCAT_SK = /* @__PURE__ */ function() {
    function NUMCAT_SK2() {
    }
    ;
    NUMCAT_SK2.value = new NUMCAT_SK2();
    return NUMCAT_SK2;
  }();
  var NUMCAT_SO = /* @__PURE__ */ function() {
    function NUMCAT_SO2() {
    }
    ;
    NUMCAT_SO2.value = new NUMCAT_SO2();
    return NUMCAT_SO2;
  }();
  var NUMCAT_ZS = /* @__PURE__ */ function() {
    function NUMCAT_ZS2() {
    }
    ;
    NUMCAT_ZS2.value = new NUMCAT_ZS2();
    return NUMCAT_ZS2;
  }();
  var NUMCAT_ZL = /* @__PURE__ */ function() {
    function NUMCAT_ZL2() {
    }
    ;
    NUMCAT_ZL2.value = new NUMCAT_ZL2();
    return NUMCAT_ZL2;
  }();
  var NUMCAT_ZP = /* @__PURE__ */ function() {
    function NUMCAT_ZP2() {
    }
    ;
    NUMCAT_ZP2.value = new NUMCAT_ZP2();
    return NUMCAT_ZP2;
  }();
  var NUMCAT_CC = /* @__PURE__ */ function() {
    function NUMCAT_CC2() {
    }
    ;
    NUMCAT_CC2.value = new NUMCAT_CC2();
    return NUMCAT_CC2;
  }();
  var NUMCAT_CF = /* @__PURE__ */ function() {
    function NUMCAT_CF2() {
    }
    ;
    NUMCAT_CF2.value = new NUMCAT_CF2();
    return NUMCAT_CF2;
  }();
  var NUMCAT_CS = /* @__PURE__ */ function() {
    function NUMCAT_CS2() {
    }
    ;
    NUMCAT_CS2.value = new NUMCAT_CS2();
    return NUMCAT_CS2;
  }();
  var NUMCAT_CO = /* @__PURE__ */ function() {
    function NUMCAT_CO2() {
    }
    ;
    NUMCAT_CO2.value = new NUMCAT_CO2();
    return NUMCAT_CO2;
  }();
  var NUMCAT_CN = /* @__PURE__ */ function() {
    function NUMCAT_CN2() {
    }
    ;
    NUMCAT_CN2.value = new NUMCAT_CN2();
    return NUMCAT_CN2;
  }();
  var numSpaceBlocks = 7;
  var numLat1Blocks = 63;
  var numConvBlocks = 1332;
  var numBlocks = 3396;
  var gencatZS = 2;
  var rule1 = /* @__PURE__ */ function() {
    return {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var spacechars = [{
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }];
  var gencatZP = 67108864;
  var rule162 = /* @__PURE__ */ function() {
    return {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZL = 33554432;
  var rule161 = /* @__PURE__ */ function() {
    return {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSO = 8192;
  var rule13 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule170 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
    };
  }();
  var rule171 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
    };
  }();
  var gencatSM = 64;
  var rule6 = /* @__PURE__ */ function() {
    return {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSK = 1024;
  var rule10 = /* @__PURE__ */ function() {
    return {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSC = 8;
  var rule3 = /* @__PURE__ */ function() {
    return {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPS = 16;
  var rule4 = /* @__PURE__ */ function() {
    return {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPO = 4;
  var rule2 = /* @__PURE__ */ function() {
    return {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPI = 32768;
  var rule15 = /* @__PURE__ */ function() {
    return {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPF = 262144;
  var rule19 = /* @__PURE__ */ function() {
    return {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPE = 32;
  var rule5 = /* @__PURE__ */ function() {
    return {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPD = 128;
  var rule7 = /* @__PURE__ */ function() {
    return {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPC = 2048;
  var rule11 = /* @__PURE__ */ function() {
    return {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNO = 131072;
  var rule17 = /* @__PURE__ */ function() {
    return {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNL = 16777216;
  var rule128 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule168 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
    };
  }();
  var rule169 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
    };
  }();
  var gencatND = 256;
  var rule8 = /* @__PURE__ */ function() {
    return {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMN = 2097152;
  var rule92 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule93 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
    };
  }();
  var gencatME = 4194304;
  var rule119 = /* @__PURE__ */ function() {
    return {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMC = 8388608;
  var rule124 = /* @__PURE__ */ function() {
    return {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLU = 512;
  var nullrule = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule104 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
    };
  }();
  var rule107 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule115 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
    };
  }();
  var rule117 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
    };
  }();
  var rule118 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
    };
  }();
  var rule120 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
    };
  }();
  var rule122 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
    };
  }();
  var rule125 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
    };
  }();
  var rule127 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38864,
      titledist: 0
    };
  }();
  var rule137 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3008 | 0,
      titledist: 0
    };
  }();
  var rule142 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
    };
  }();
  var rule144 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule153 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
    };
  }();
  var rule156 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
    };
  }();
  var rule157 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
    };
  }();
  var rule158 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
    };
  }();
  var rule159 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
    };
  }();
  var rule160 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
    };
  }();
  var rule163 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
    };
  }();
  var rule164 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
    };
  }();
  var rule165 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
    };
  }();
  var rule166 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
    };
  }();
  var rule172 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
    };
  }();
  var rule173 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
    };
  }();
  var rule174 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
    };
  }();
  var rule177 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
    };
  }();
  var rule178 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
    };
  }();
  var rule179 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
    };
  }();
  var rule180 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
    };
  }();
  var rule181 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
    };
  }();
  var rule183 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
    };
  }();
  var rule184 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
    };
  }();
  var rule186 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42308 | 0,
      titledist: 0
    };
  }();
  var rule187 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42319 | 0,
      titledist: 0
    };
  }();
  var rule188 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42315 | 0,
      titledist: 0
    };
  }();
  var rule189 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42305 | 0,
      titledist: 0
    };
  }();
  var rule190 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42258 | 0,
      titledist: 0
    };
  }();
  var rule191 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42282 | 0,
      titledist: 0
    };
  }();
  var rule192 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42261 | 0,
      titledist: 0
    };
  }();
  var rule193 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 928,
      titledist: 0
    };
  }();
  var rule194 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -48 | 0,
      titledist: 0
    };
  }();
  var rule195 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42307 | 0,
      titledist: 0
    };
  }();
  var rule196 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35384 | 0,
      titledist: 0
    };
  }();
  var rule201 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
    };
  }();
  var rule203 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 34,
      titledist: 0
    };
  }();
  var rule22 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var rule24 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
    };
  }();
  var rule26 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
    };
  }();
  var rule29 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
    };
  }();
  var rule30 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
    };
  }();
  var rule31 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
    };
  }();
  var rule32 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
    };
  }();
  var rule33 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
    };
  }();
  var rule34 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
    };
  }();
  var rule35 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
    };
  }();
  var rule37 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
    };
  }();
  var rule38 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
    };
  }();
  var rule40 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
    };
  }();
  var rule42 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
    };
  }();
  var rule43 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
    };
  }();
  var rule44 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
    };
  }();
  var rule45 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
    };
  }();
  var rule47 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
    };
  }();
  var rule51 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
    };
  }();
  var rule52 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
    };
  }();
  var rule53 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
    };
  }();
  var rule54 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
    };
  }();
  var rule55 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
    };
  }();
  var rule56 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
    };
  }();
  var rule58 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
    };
  }();
  var rule59 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
    };
  }();
  var rule60 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
    };
  }();
  var rule9 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
    };
  }();
  var rule94 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 116,
      titledist: 0
    };
  }();
  var rule95 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
    };
  }();
  var rule96 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
    };
  }();
  var rule97 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
    };
  }();
  var rule98 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
    };
  }();
  var gencatLT = 524288;
  var rule151 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule154 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
    };
  }();
  var rule48 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var gencatLO = 16384;
  var rule14 = /* @__PURE__ */ function() {
    return {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLM = 1048576;
  var rule91 = /* @__PURE__ */ function() {
    return {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLL = 4096;
  var rule100 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
    };
  }();
  var rule101 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
    };
  }();
  var rule102 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
    };
  }();
  var rule103 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
    };
  }();
  var rule105 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
    };
  }();
  var rule106 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
    };
  }();
  var rule108 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
    };
  }();
  var rule109 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
    };
  }();
  var rule110 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
    };
  }();
  var rule111 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
    };
  }();
  var rule112 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
    };
  }();
  var rule113 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
    };
  }();
  var rule114 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -116 | 0,
      lowdist: 0,
      titledist: -116 | 0
    };
  }();
  var rule116 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
    };
  }();
  var rule12 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
    };
  }();
  var rule121 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
    };
  }();
  var rule123 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
    };
  }();
  var rule126 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3008,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule129 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6254 | 0,
      lowdist: 0,
      titledist: -6254 | 0
    };
  }();
  var rule130 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6253 | 0,
      lowdist: 0,
      titledist: -6253 | 0
    };
  }();
  var rule131 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6244 | 0,
      lowdist: 0,
      titledist: -6244 | 0
    };
  }();
  var rule132 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6242 | 0,
      lowdist: 0,
      titledist: -6242 | 0
    };
  }();
  var rule133 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6243 | 0,
      lowdist: 0,
      titledist: -6243 | 0
    };
  }();
  var rule134 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6236 | 0,
      lowdist: 0,
      titledist: -6236 | 0
    };
  }();
  var rule135 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6181 | 0,
      lowdist: 0,
      titledist: -6181 | 0
    };
  }();
  var rule136 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35266,
      lowdist: 0,
      titledist: 35266
    };
  }();
  var rule138 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
    };
  }();
  var rule139 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
    };
  }();
  var rule140 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35384,
      lowdist: 0,
      titledist: 35384
    };
  }();
  var rule141 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
    };
  }();
  var rule143 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
    };
  }();
  var rule145 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
    };
  }();
  var rule146 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
    };
  }();
  var rule147 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
    };
  }();
  var rule148 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
    };
  }();
  var rule149 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
    };
  }();
  var rule150 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
    };
  }();
  var rule152 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
    };
  }();
  var rule155 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
    };
  }();
  var rule167 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
    };
  }();
  var rule175 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
    };
  }();
  var rule176 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
    };
  }();
  var rule18 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
    };
  }();
  var rule182 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
    };
  }();
  var rule185 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 48,
      lowdist: 0,
      titledist: 48
    };
  }();
  var rule197 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -928 | 0,
      lowdist: 0,
      titledist: -928 | 0
    };
  }();
  var rule198 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38864 | 0,
      lowdist: 0,
      titledist: -38864 | 0
    };
  }();
  var rule20 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule202 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
    };
  }();
  var rule204 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -34 | 0,
      lowdist: 0,
      titledist: -34 | 0
    };
  }();
  var rule21 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
    };
  }();
  var rule23 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule25 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
    };
  }();
  var rule27 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
    };
  }();
  var rule28 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
    };
  }();
  var rule36 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
    };
  }();
  var rule39 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
    };
  }();
  var rule41 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
    };
  }();
  var rule46 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
    };
  }();
  var rule49 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule50 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
    };
  }();
  var rule57 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
    };
  }();
  var rule61 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
    };
  }();
  var rule62 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
    };
  }();
  var rule63 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
    };
  }();
  var rule64 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
    };
  }();
  var rule65 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
    };
  }();
  var rule66 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
    };
  }();
  var rule67 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
    };
  }();
  var rule68 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
    };
  }();
  var rule69 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42319,
      lowdist: 0,
      titledist: 42319
    };
  }();
  var rule70 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42315,
      lowdist: 0,
      titledist: 42315
    };
  }();
  var rule71 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
    };
  }();
  var rule72 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
    };
  }();
  var rule73 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42308,
      lowdist: 0,
      titledist: 42308
    };
  }();
  var rule74 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
    };
  }();
  var rule75 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
    };
  }();
  var rule76 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
    };
  }();
  var rule77 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42305,
      lowdist: 0,
      titledist: 42305
    };
  }();
  var rule78 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
    };
  }();
  var rule79 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
    };
  }();
  var rule80 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
    };
  }();
  var rule81 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
    };
  }();
  var rule82 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
    };
  }();
  var rule83 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42307,
      lowdist: 0,
      titledist: 42307
    };
  }();
  var rule84 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42282,
      lowdist: 0,
      titledist: 42282
    };
  }();
  var rule85 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
    };
  }();
  var rule86 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
    };
  }();
  var rule87 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
    };
  }();
  var rule88 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
    };
  }();
  var rule89 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42261,
      lowdist: 0,
      titledist: 42261
    };
  }();
  var rule90 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42258,
      lowdist: 0,
      titledist: 42258
    };
  }();
  var rule99 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
    };
  }();
  var gencatCS = 134217728;
  var rule199 = /* @__PURE__ */ function() {
    return {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCO = 268435456;
  var rule200 = /* @__PURE__ */ function() {
    return {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCF = 65536;
  var rule16 = /* @__PURE__ */ function() {
    return {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCC = 1;
  var rule0 = /* @__PURE__ */ function() {
    return {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var convchars = [{
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }];
  var bsearch = function(a) {
    return function(array) {
      return function(size3) {
        return function(compare6) {
          var go = function($copy_i) {
            return function($copy_k) {
              var $tco_var_i = $copy_i;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(i, k) {
                if (i > k || i >= length(array)) {
                  $tco_done = true;
                  return Nothing.value;
                }
                ;
                if (otherwise) {
                  var j = floor2(toNumber(i + k | 0) / 2);
                  var b = unsafeIndex2(array)(j);
                  var v = compare6(a)(b);
                  if (v instanceof EQ) {
                    $tco_done = true;
                    return new Just(b);
                  }
                  ;
                  if (v instanceof GT) {
                    $tco_var_i = j + 1 | 0;
                    $copy_k = k;
                    return;
                  }
                  ;
                  $tco_var_i = i;
                  $copy_k = j - 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i.constructor.name, k.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_k);
              }
              ;
              return $tco_result;
            };
          };
          return go(0)(size3);
        };
      };
    };
  };
  var blkCmp = function(v) {
    return function(v1) {
      if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
        return EQ.value;
      }
      ;
      if (v.start > v1.start) {
        return GT.value;
      }
      ;
      if (otherwise) {
        return LT.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getRule = function(blocks) {
    return function(unichar) {
      return function(size3) {
        var key = {
          start: unichar,
          length: 1,
          convRule: nullrule
        };
        var maybeCharBlock = bsearch(key)(blocks)(size3)(blkCmp);
        if (maybeCharBlock instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (maybeCharBlock instanceof Just) {
          return new Just(maybeCharBlock.value0.convRule);
        }
        ;
        throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
      };
    };
  };
  var caseConv = function(f) {
    return function($$char2) {
      var maybeConversionRule = getRule(convchars)($$char2)(numConvBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return $$char2;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return $$char2 + f(maybeConversionRule.value0) | 0;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5727, column 5 - line 5729, column 53): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uTowlower = /* @__PURE__ */ caseConv(function(v) {
    return v.lowdist;
  });
  var uTowupper = /* @__PURE__ */ caseConv(function(v) {
    return v.updist;
  });
  var checkAttrS = function(categories) {
    return function($$char2) {
      var maybeConversionRule = getRule(spacechars)($$char2)(numSpaceBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5654, column 5 - line 5656, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswspace = /* @__PURE__ */ checkAttrS([gencatZS]);
  var allchars = [{
    start: 0,
    length: 32,
    convRule: rule0
  }, {
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 33,
    length: 3,
    convRule: rule2
  }, {
    start: 36,
    length: 1,
    convRule: rule3
  }, {
    start: 37,
    length: 3,
    convRule: rule2
  }, {
    start: 40,
    length: 1,
    convRule: rule4
  }, {
    start: 41,
    length: 1,
    convRule: rule5
  }, {
    start: 42,
    length: 1,
    convRule: rule2
  }, {
    start: 43,
    length: 1,
    convRule: rule6
  }, {
    start: 44,
    length: 1,
    convRule: rule2
  }, {
    start: 45,
    length: 1,
    convRule: rule7
  }, {
    start: 46,
    length: 2,
    convRule: rule2
  }, {
    start: 48,
    length: 10,
    convRule: rule8
  }, {
    start: 58,
    length: 2,
    convRule: rule2
  }, {
    start: 60,
    length: 3,
    convRule: rule6
  }, {
    start: 63,
    length: 2,
    convRule: rule2
  }, {
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 91,
    length: 1,
    convRule: rule4
  }, {
    start: 92,
    length: 1,
    convRule: rule2
  }, {
    start: 93,
    length: 1,
    convRule: rule5
  }, {
    start: 94,
    length: 1,
    convRule: rule10
  }, {
    start: 95,
    length: 1,
    convRule: rule11
  }, {
    start: 96,
    length: 1,
    convRule: rule10
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 123,
    length: 1,
    convRule: rule4
  }, {
    start: 124,
    length: 1,
    convRule: rule6
  }, {
    start: 125,
    length: 1,
    convRule: rule5
  }, {
    start: 126,
    length: 1,
    convRule: rule6
  }, {
    start: 127,
    length: 33,
    convRule: rule0
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 161,
    length: 1,
    convRule: rule2
  }, {
    start: 162,
    length: 4,
    convRule: rule3
  }, {
    start: 166,
    length: 1,
    convRule: rule13
  }, {
    start: 167,
    length: 1,
    convRule: rule2
  }, {
    start: 168,
    length: 1,
    convRule: rule10
  }, {
    start: 169,
    length: 1,
    convRule: rule13
  }, {
    start: 170,
    length: 1,
    convRule: rule14
  }, {
    start: 171,
    length: 1,
    convRule: rule15
  }, {
    start: 172,
    length: 1,
    convRule: rule6
  }, {
    start: 173,
    length: 1,
    convRule: rule16
  }, {
    start: 174,
    length: 1,
    convRule: rule13
  }, {
    start: 175,
    length: 1,
    convRule: rule10
  }, {
    start: 176,
    length: 1,
    convRule: rule13
  }, {
    start: 177,
    length: 1,
    convRule: rule6
  }, {
    start: 178,
    length: 2,
    convRule: rule17
  }, {
    start: 180,
    length: 1,
    convRule: rule10
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 182,
    length: 2,
    convRule: rule2
  }, {
    start: 184,
    length: 1,
    convRule: rule10
  }, {
    start: 185,
    length: 1,
    convRule: rule17
  }, {
    start: 186,
    length: 1,
    convRule: rule14
  }, {
    start: 187,
    length: 1,
    convRule: rule19
  }, {
    start: 188,
    length: 3,
    convRule: rule17
  }, {
    start: 191,
    length: 1,
    convRule: rule2
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 215,
    length: 1,
    convRule: rule6
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 223,
    length: 1,
    convRule: rule20
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 247,
    length: 1,
    convRule: rule6
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 312,
    length: 1,
    convRule: rule20
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 329,
    length: 1,
    convRule: rule20
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 397,
    length: 1,
    convRule: rule20
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 411,
    length: 1,
    convRule: rule20
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 426,
    length: 2,
    convRule: rule20
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 442,
    length: 1,
    convRule: rule20
  }, {
    start: 443,
    length: 1,
    convRule: rule14
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 446,
    length: 1,
    convRule: rule20
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 448,
    length: 4,
    convRule: rule14
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 496,
    length: 1,
    convRule: rule20
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 545,
    length: 1,
    convRule: rule20
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 564,
    length: 6,
    convRule: rule20
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 597,
    length: 1,
    convRule: rule20
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 600,
    length: 1,
    convRule: rule20
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 602,
    length: 1,
    convRule: rule20
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 605,
    length: 3,
    convRule: rule20
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 610,
    length: 1,
    convRule: rule20
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 612,
    length: 1,
    convRule: rule20
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 615,
    length: 1,
    convRule: rule20
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 621,
    length: 2,
    convRule: rule20
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 624,
    length: 1,
    convRule: rule20
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 627,
    length: 2,
    convRule: rule20
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 630,
    length: 7,
    convRule: rule20
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 638,
    length: 2,
    convRule: rule20
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 641,
    length: 1,
    convRule: rule20
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 644,
    length: 3,
    convRule: rule20
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 653,
    length: 5,
    convRule: rule20
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 659,
    length: 1,
    convRule: rule20
  }, {
    start: 660,
    length: 1,
    convRule: rule14
  }, {
    start: 661,
    length: 8,
    convRule: rule20
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 671,
    length: 17,
    convRule: rule20
  }, {
    start: 688,
    length: 18,
    convRule: rule91
  }, {
    start: 706,
    length: 4,
    convRule: rule10
  }, {
    start: 710,
    length: 12,
    convRule: rule91
  }, {
    start: 722,
    length: 14,
    convRule: rule10
  }, {
    start: 736,
    length: 5,
    convRule: rule91
  }, {
    start: 741,
    length: 7,
    convRule: rule10
  }, {
    start: 748,
    length: 1,
    convRule: rule91
  }, {
    start: 749,
    length: 1,
    convRule: rule10
  }, {
    start: 750,
    length: 1,
    convRule: rule91
  }, {
    start: 751,
    length: 17,
    convRule: rule10
  }, {
    start: 768,
    length: 69,
    convRule: rule92
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 838,
    length: 42,
    convRule: rule92
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 884,
    length: 1,
    convRule: rule91
  }, {
    start: 885,
    length: 1,
    convRule: rule10
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 890,
    length: 1,
    convRule: rule91
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 894,
    length: 1,
    convRule: rule2
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 900,
    length: 2,
    convRule: rule10
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 903,
    length: 1,
    convRule: rule2
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 912,
    length: 1,
    convRule: rule20
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 944,
    length: 1,
    convRule: rule20
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 978,
    length: 3,
    convRule: rule107
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1014,
    length: 1,
    convRule: rule6
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1020,
    length: 1,
    convRule: rule20
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1154,
    length: 1,
    convRule: rule13
  }, {
    start: 1155,
    length: 5,
    convRule: rule92
  }, {
    start: 1160,
    length: 2,
    convRule: rule119
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1369,
    length: 1,
    convRule: rule91
  }, {
    start: 1370,
    length: 6,
    convRule: rule2
  }, {
    start: 1376,
    length: 1,
    convRule: rule20
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 1415,
    length: 2,
    convRule: rule20
  }, {
    start: 1417,
    length: 1,
    convRule: rule2
  }, {
    start: 1418,
    length: 1,
    convRule: rule7
  }, {
    start: 1421,
    length: 2,
    convRule: rule13
  }, {
    start: 1423,
    length: 1,
    convRule: rule3
  }, {
    start: 1425,
    length: 45,
    convRule: rule92
  }, {
    start: 1470,
    length: 1,
    convRule: rule7
  }, {
    start: 1471,
    length: 1,
    convRule: rule92
  }, {
    start: 1472,
    length: 1,
    convRule: rule2
  }, {
    start: 1473,
    length: 2,
    convRule: rule92
  }, {
    start: 1475,
    length: 1,
    convRule: rule2
  }, {
    start: 1476,
    length: 2,
    convRule: rule92
  }, {
    start: 1478,
    length: 1,
    convRule: rule2
  }, {
    start: 1479,
    length: 1,
    convRule: rule92
  }, {
    start: 1488,
    length: 27,
    convRule: rule14
  }, {
    start: 1519,
    length: 4,
    convRule: rule14
  }, {
    start: 1523,
    length: 2,
    convRule: rule2
  }, {
    start: 1536,
    length: 6,
    convRule: rule16
  }, {
    start: 1542,
    length: 3,
    convRule: rule6
  }, {
    start: 1545,
    length: 2,
    convRule: rule2
  }, {
    start: 1547,
    length: 1,
    convRule: rule3
  }, {
    start: 1548,
    length: 2,
    convRule: rule2
  }, {
    start: 1550,
    length: 2,
    convRule: rule13
  }, {
    start: 1552,
    length: 11,
    convRule: rule92
  }, {
    start: 1563,
    length: 1,
    convRule: rule2
  }, {
    start: 1564,
    length: 1,
    convRule: rule16
  }, {
    start: 1566,
    length: 2,
    convRule: rule2
  }, {
    start: 1568,
    length: 32,
    convRule: rule14
  }, {
    start: 1600,
    length: 1,
    convRule: rule91
  }, {
    start: 1601,
    length: 10,
    convRule: rule14
  }, {
    start: 1611,
    length: 21,
    convRule: rule92
  }, {
    start: 1632,
    length: 10,
    convRule: rule8
  }, {
    start: 1642,
    length: 4,
    convRule: rule2
  }, {
    start: 1646,
    length: 2,
    convRule: rule14
  }, {
    start: 1648,
    length: 1,
    convRule: rule92
  }, {
    start: 1649,
    length: 99,
    convRule: rule14
  }, {
    start: 1748,
    length: 1,
    convRule: rule2
  }, {
    start: 1749,
    length: 1,
    convRule: rule14
  }, {
    start: 1750,
    length: 7,
    convRule: rule92
  }, {
    start: 1757,
    length: 1,
    convRule: rule16
  }, {
    start: 1758,
    length: 1,
    convRule: rule13
  }, {
    start: 1759,
    length: 6,
    convRule: rule92
  }, {
    start: 1765,
    length: 2,
    convRule: rule91
  }, {
    start: 1767,
    length: 2,
    convRule: rule92
  }, {
    start: 1769,
    length: 1,
    convRule: rule13
  }, {
    start: 1770,
    length: 4,
    convRule: rule92
  }, {
    start: 1774,
    length: 2,
    convRule: rule14
  }, {
    start: 1776,
    length: 10,
    convRule: rule8
  }, {
    start: 1786,
    length: 3,
    convRule: rule14
  }, {
    start: 1789,
    length: 2,
    convRule: rule13
  }, {
    start: 1791,
    length: 1,
    convRule: rule14
  }, {
    start: 1792,
    length: 14,
    convRule: rule2
  }, {
    start: 1807,
    length: 1,
    convRule: rule16
  }, {
    start: 1808,
    length: 1,
    convRule: rule14
  }, {
    start: 1809,
    length: 1,
    convRule: rule92
  }, {
    start: 1810,
    length: 30,
    convRule: rule14
  }, {
    start: 1840,
    length: 27,
    convRule: rule92
  }, {
    start: 1869,
    length: 89,
    convRule: rule14
  }, {
    start: 1958,
    length: 11,
    convRule: rule92
  }, {
    start: 1969,
    length: 1,
    convRule: rule14
  }, {
    start: 1984,
    length: 10,
    convRule: rule8
  }, {
    start: 1994,
    length: 33,
    convRule: rule14
  }, {
    start: 2027,
    length: 9,
    convRule: rule92
  }, {
    start: 2036,
    length: 2,
    convRule: rule91
  }, {
    start: 2038,
    length: 1,
    convRule: rule13
  }, {
    start: 2039,
    length: 3,
    convRule: rule2
  }, {
    start: 2042,
    length: 1,
    convRule: rule91
  }, {
    start: 2045,
    length: 1,
    convRule: rule92
  }, {
    start: 2046,
    length: 2,
    convRule: rule3
  }, {
    start: 2048,
    length: 22,
    convRule: rule14
  }, {
    start: 2070,
    length: 4,
    convRule: rule92
  }, {
    start: 2074,
    length: 1,
    convRule: rule91
  }, {
    start: 2075,
    length: 9,
    convRule: rule92
  }, {
    start: 2084,
    length: 1,
    convRule: rule91
  }, {
    start: 2085,
    length: 3,
    convRule: rule92
  }, {
    start: 2088,
    length: 1,
    convRule: rule91
  }, {
    start: 2089,
    length: 5,
    convRule: rule92
  }, {
    start: 2096,
    length: 15,
    convRule: rule2
  }, {
    start: 2112,
    length: 25,
    convRule: rule14
  }, {
    start: 2137,
    length: 3,
    convRule: rule92
  }, {
    start: 2142,
    length: 1,
    convRule: rule2
  }, {
    start: 2144,
    length: 11,
    convRule: rule14
  }, {
    start: 2208,
    length: 21,
    convRule: rule14
  }, {
    start: 2230,
    length: 18,
    convRule: rule14
  }, {
    start: 2259,
    length: 15,
    convRule: rule92
  }, {
    start: 2274,
    length: 1,
    convRule: rule16
  }, {
    start: 2275,
    length: 32,
    convRule: rule92
  }, {
    start: 2307,
    length: 1,
    convRule: rule124
  }, {
    start: 2308,
    length: 54,
    convRule: rule14
  }, {
    start: 2362,
    length: 1,
    convRule: rule92
  }, {
    start: 2363,
    length: 1,
    convRule: rule124
  }, {
    start: 2364,
    length: 1,
    convRule: rule92
  }, {
    start: 2365,
    length: 1,
    convRule: rule14
  }, {
    start: 2366,
    length: 3,
    convRule: rule124
  }, {
    start: 2369,
    length: 8,
    convRule: rule92
  }, {
    start: 2377,
    length: 4,
    convRule: rule124
  }, {
    start: 2381,
    length: 1,
    convRule: rule92
  }, {
    start: 2382,
    length: 2,
    convRule: rule124
  }, {
    start: 2384,
    length: 1,
    convRule: rule14
  }, {
    start: 2385,
    length: 7,
    convRule: rule92
  }, {
    start: 2392,
    length: 10,
    convRule: rule14
  }, {
    start: 2402,
    length: 2,
    convRule: rule92
  }, {
    start: 2404,
    length: 2,
    convRule: rule2
  }, {
    start: 2406,
    length: 10,
    convRule: rule8
  }, {
    start: 2416,
    length: 1,
    convRule: rule2
  }, {
    start: 2417,
    length: 1,
    convRule: rule91
  }, {
    start: 2418,
    length: 15,
    convRule: rule14
  }, {
    start: 2433,
    length: 1,
    convRule: rule92
  }, {
    start: 2434,
    length: 2,
    convRule: rule124
  }, {
    start: 2437,
    length: 8,
    convRule: rule14
  }, {
    start: 2447,
    length: 2,
    convRule: rule14
  }, {
    start: 2451,
    length: 22,
    convRule: rule14
  }, {
    start: 2474,
    length: 7,
    convRule: rule14
  }, {
    start: 2482,
    length: 1,
    convRule: rule14
  }, {
    start: 2486,
    length: 4,
    convRule: rule14
  }, {
    start: 2492,
    length: 1,
    convRule: rule92
  }, {
    start: 2493,
    length: 1,
    convRule: rule14
  }, {
    start: 2494,
    length: 3,
    convRule: rule124
  }, {
    start: 2497,
    length: 4,
    convRule: rule92
  }, {
    start: 2503,
    length: 2,
    convRule: rule124
  }, {
    start: 2507,
    length: 2,
    convRule: rule124
  }, {
    start: 2509,
    length: 1,
    convRule: rule92
  }, {
    start: 2510,
    length: 1,
    convRule: rule14
  }, {
    start: 2519,
    length: 1,
    convRule: rule124
  }, {
    start: 2524,
    length: 2,
    convRule: rule14
  }, {
    start: 2527,
    length: 3,
    convRule: rule14
  }, {
    start: 2530,
    length: 2,
    convRule: rule92
  }, {
    start: 2534,
    length: 10,
    convRule: rule8
  }, {
    start: 2544,
    length: 2,
    convRule: rule14
  }, {
    start: 2546,
    length: 2,
    convRule: rule3
  }, {
    start: 2548,
    length: 6,
    convRule: rule17
  }, {
    start: 2554,
    length: 1,
    convRule: rule13
  }, {
    start: 2555,
    length: 1,
    convRule: rule3
  }, {
    start: 2556,
    length: 1,
    convRule: rule14
  }, {
    start: 2557,
    length: 1,
    convRule: rule2
  }, {
    start: 2558,
    length: 1,
    convRule: rule92
  }, {
    start: 2561,
    length: 2,
    convRule: rule92
  }, {
    start: 2563,
    length: 1,
    convRule: rule124
  }, {
    start: 2565,
    length: 6,
    convRule: rule14
  }, {
    start: 2575,
    length: 2,
    convRule: rule14
  }, {
    start: 2579,
    length: 22,
    convRule: rule14
  }, {
    start: 2602,
    length: 7,
    convRule: rule14
  }, {
    start: 2610,
    length: 2,
    convRule: rule14
  }, {
    start: 2613,
    length: 2,
    convRule: rule14
  }, {
    start: 2616,
    length: 2,
    convRule: rule14
  }, {
    start: 2620,
    length: 1,
    convRule: rule92
  }, {
    start: 2622,
    length: 3,
    convRule: rule124
  }, {
    start: 2625,
    length: 2,
    convRule: rule92
  }, {
    start: 2631,
    length: 2,
    convRule: rule92
  }, {
    start: 2635,
    length: 3,
    convRule: rule92
  }, {
    start: 2641,
    length: 1,
    convRule: rule92
  }, {
    start: 2649,
    length: 4,
    convRule: rule14
  }, {
    start: 2654,
    length: 1,
    convRule: rule14
  }, {
    start: 2662,
    length: 10,
    convRule: rule8
  }, {
    start: 2672,
    length: 2,
    convRule: rule92
  }, {
    start: 2674,
    length: 3,
    convRule: rule14
  }, {
    start: 2677,
    length: 1,
    convRule: rule92
  }, {
    start: 2678,
    length: 1,
    convRule: rule2
  }, {
    start: 2689,
    length: 2,
    convRule: rule92
  }, {
    start: 2691,
    length: 1,
    convRule: rule124
  }, {
    start: 2693,
    length: 9,
    convRule: rule14
  }, {
    start: 2703,
    length: 3,
    convRule: rule14
  }, {
    start: 2707,
    length: 22,
    convRule: rule14
  }, {
    start: 2730,
    length: 7,
    convRule: rule14
  }, {
    start: 2738,
    length: 2,
    convRule: rule14
  }, {
    start: 2741,
    length: 5,
    convRule: rule14
  }, {
    start: 2748,
    length: 1,
    convRule: rule92
  }, {
    start: 2749,
    length: 1,
    convRule: rule14
  }, {
    start: 2750,
    length: 3,
    convRule: rule124
  }, {
    start: 2753,
    length: 5,
    convRule: rule92
  }, {
    start: 2759,
    length: 2,
    convRule: rule92
  }, {
    start: 2761,
    length: 1,
    convRule: rule124
  }, {
    start: 2763,
    length: 2,
    convRule: rule124
  }, {
    start: 2765,
    length: 1,
    convRule: rule92
  }, {
    start: 2768,
    length: 1,
    convRule: rule14
  }, {
    start: 2784,
    length: 2,
    convRule: rule14
  }, {
    start: 2786,
    length: 2,
    convRule: rule92
  }, {
    start: 2790,
    length: 10,
    convRule: rule8
  }, {
    start: 2800,
    length: 1,
    convRule: rule2
  }, {
    start: 2801,
    length: 1,
    convRule: rule3
  }, {
    start: 2809,
    length: 1,
    convRule: rule14
  }, {
    start: 2810,
    length: 6,
    convRule: rule92
  }, {
    start: 2817,
    length: 1,
    convRule: rule92
  }, {
    start: 2818,
    length: 2,
    convRule: rule124
  }, {
    start: 2821,
    length: 8,
    convRule: rule14
  }, {
    start: 2831,
    length: 2,
    convRule: rule14
  }, {
    start: 2835,
    length: 22,
    convRule: rule14
  }, {
    start: 2858,
    length: 7,
    convRule: rule14
  }, {
    start: 2866,
    length: 2,
    convRule: rule14
  }, {
    start: 2869,
    length: 5,
    convRule: rule14
  }, {
    start: 2876,
    length: 1,
    convRule: rule92
  }, {
    start: 2877,
    length: 1,
    convRule: rule14
  }, {
    start: 2878,
    length: 1,
    convRule: rule124
  }, {
    start: 2879,
    length: 1,
    convRule: rule92
  }, {
    start: 2880,
    length: 1,
    convRule: rule124
  }, {
    start: 2881,
    length: 4,
    convRule: rule92
  }, {
    start: 2887,
    length: 2,
    convRule: rule124
  }, {
    start: 2891,
    length: 2,
    convRule: rule124
  }, {
    start: 2893,
    length: 1,
    convRule: rule92
  }, {
    start: 2901,
    length: 2,
    convRule: rule92
  }, {
    start: 2903,
    length: 1,
    convRule: rule124
  }, {
    start: 2908,
    length: 2,
    convRule: rule14
  }, {
    start: 2911,
    length: 3,
    convRule: rule14
  }, {
    start: 2914,
    length: 2,
    convRule: rule92
  }, {
    start: 2918,
    length: 10,
    convRule: rule8
  }, {
    start: 2928,
    length: 1,
    convRule: rule13
  }, {
    start: 2929,
    length: 1,
    convRule: rule14
  }, {
    start: 2930,
    length: 6,
    convRule: rule17
  }, {
    start: 2946,
    length: 1,
    convRule: rule92
  }, {
    start: 2947,
    length: 1,
    convRule: rule14
  }, {
    start: 2949,
    length: 6,
    convRule: rule14
  }, {
    start: 2958,
    length: 3,
    convRule: rule14
  }, {
    start: 2962,
    length: 4,
    convRule: rule14
  }, {
    start: 2969,
    length: 2,
    convRule: rule14
  }, {
    start: 2972,
    length: 1,
    convRule: rule14
  }, {
    start: 2974,
    length: 2,
    convRule: rule14
  }, {
    start: 2979,
    length: 2,
    convRule: rule14
  }, {
    start: 2984,
    length: 3,
    convRule: rule14
  }, {
    start: 2990,
    length: 12,
    convRule: rule14
  }, {
    start: 3006,
    length: 2,
    convRule: rule124
  }, {
    start: 3008,
    length: 1,
    convRule: rule92
  }, {
    start: 3009,
    length: 2,
    convRule: rule124
  }, {
    start: 3014,
    length: 3,
    convRule: rule124
  }, {
    start: 3018,
    length: 3,
    convRule: rule124
  }, {
    start: 3021,
    length: 1,
    convRule: rule92
  }, {
    start: 3024,
    length: 1,
    convRule: rule14
  }, {
    start: 3031,
    length: 1,
    convRule: rule124
  }, {
    start: 3046,
    length: 10,
    convRule: rule8
  }, {
    start: 3056,
    length: 3,
    convRule: rule17
  }, {
    start: 3059,
    length: 6,
    convRule: rule13
  }, {
    start: 3065,
    length: 1,
    convRule: rule3
  }, {
    start: 3066,
    length: 1,
    convRule: rule13
  }, {
    start: 3072,
    length: 1,
    convRule: rule92
  }, {
    start: 3073,
    length: 3,
    convRule: rule124
  }, {
    start: 3076,
    length: 1,
    convRule: rule92
  }, {
    start: 3077,
    length: 8,
    convRule: rule14
  }, {
    start: 3086,
    length: 3,
    convRule: rule14
  }, {
    start: 3090,
    length: 23,
    convRule: rule14
  }, {
    start: 3114,
    length: 16,
    convRule: rule14
  }, {
    start: 3133,
    length: 1,
    convRule: rule14
  }, {
    start: 3134,
    length: 3,
    convRule: rule92
  }, {
    start: 3137,
    length: 4,
    convRule: rule124
  }, {
    start: 3142,
    length: 3,
    convRule: rule92
  }, {
    start: 3146,
    length: 4,
    convRule: rule92
  }, {
    start: 3157,
    length: 2,
    convRule: rule92
  }, {
    start: 3160,
    length: 3,
    convRule: rule14
  }, {
    start: 3168,
    length: 2,
    convRule: rule14
  }, {
    start: 3170,
    length: 2,
    convRule: rule92
  }, {
    start: 3174,
    length: 10,
    convRule: rule8
  }, {
    start: 3191,
    length: 1,
    convRule: rule2
  }, {
    start: 3192,
    length: 7,
    convRule: rule17
  }, {
    start: 3199,
    length: 1,
    convRule: rule13
  }, {
    start: 3200,
    length: 1,
    convRule: rule14
  }, {
    start: 3201,
    length: 1,
    convRule: rule92
  }, {
    start: 3202,
    length: 2,
    convRule: rule124
  }, {
    start: 3204,
    length: 1,
    convRule: rule2
  }, {
    start: 3205,
    length: 8,
    convRule: rule14
  }, {
    start: 3214,
    length: 3,
    convRule: rule14
  }, {
    start: 3218,
    length: 23,
    convRule: rule14
  }, {
    start: 3242,
    length: 10,
    convRule: rule14
  }, {
    start: 3253,
    length: 5,
    convRule: rule14
  }, {
    start: 3260,
    length: 1,
    convRule: rule92
  }, {
    start: 3261,
    length: 1,
    convRule: rule14
  }, {
    start: 3262,
    length: 1,
    convRule: rule124
  }, {
    start: 3263,
    length: 1,
    convRule: rule92
  }, {
    start: 3264,
    length: 5,
    convRule: rule124
  }, {
    start: 3270,
    length: 1,
    convRule: rule92
  }, {
    start: 3271,
    length: 2,
    convRule: rule124
  }, {
    start: 3274,
    length: 2,
    convRule: rule124
  }, {
    start: 3276,
    length: 2,
    convRule: rule92
  }, {
    start: 3285,
    length: 2,
    convRule: rule124
  }, {
    start: 3294,
    length: 1,
    convRule: rule14
  }, {
    start: 3296,
    length: 2,
    convRule: rule14
  }, {
    start: 3298,
    length: 2,
    convRule: rule92
  }, {
    start: 3302,
    length: 10,
    convRule: rule8
  }, {
    start: 3313,
    length: 2,
    convRule: rule14
  }, {
    start: 3328,
    length: 2,
    convRule: rule92
  }, {
    start: 3330,
    length: 2,
    convRule: rule124
  }, {
    start: 3332,
    length: 9,
    convRule: rule14
  }, {
    start: 3342,
    length: 3,
    convRule: rule14
  }, {
    start: 3346,
    length: 41,
    convRule: rule14
  }, {
    start: 3387,
    length: 2,
    convRule: rule92
  }, {
    start: 3389,
    length: 1,
    convRule: rule14
  }, {
    start: 3390,
    length: 3,
    convRule: rule124
  }, {
    start: 3393,
    length: 4,
    convRule: rule92
  }, {
    start: 3398,
    length: 3,
    convRule: rule124
  }, {
    start: 3402,
    length: 3,
    convRule: rule124
  }, {
    start: 3405,
    length: 1,
    convRule: rule92
  }, {
    start: 3406,
    length: 1,
    convRule: rule14
  }, {
    start: 3407,
    length: 1,
    convRule: rule13
  }, {
    start: 3412,
    length: 3,
    convRule: rule14
  }, {
    start: 3415,
    length: 1,
    convRule: rule124
  }, {
    start: 3416,
    length: 7,
    convRule: rule17
  }, {
    start: 3423,
    length: 3,
    convRule: rule14
  }, {
    start: 3426,
    length: 2,
    convRule: rule92
  }, {
    start: 3430,
    length: 10,
    convRule: rule8
  }, {
    start: 3440,
    length: 9,
    convRule: rule17
  }, {
    start: 3449,
    length: 1,
    convRule: rule13
  }, {
    start: 3450,
    length: 6,
    convRule: rule14
  }, {
    start: 3457,
    length: 1,
    convRule: rule92
  }, {
    start: 3458,
    length: 2,
    convRule: rule124
  }, {
    start: 3461,
    length: 18,
    convRule: rule14
  }, {
    start: 3482,
    length: 24,
    convRule: rule14
  }, {
    start: 3507,
    length: 9,
    convRule: rule14
  }, {
    start: 3517,
    length: 1,
    convRule: rule14
  }, {
    start: 3520,
    length: 7,
    convRule: rule14
  }, {
    start: 3530,
    length: 1,
    convRule: rule92
  }, {
    start: 3535,
    length: 3,
    convRule: rule124
  }, {
    start: 3538,
    length: 3,
    convRule: rule92
  }, {
    start: 3542,
    length: 1,
    convRule: rule92
  }, {
    start: 3544,
    length: 8,
    convRule: rule124
  }, {
    start: 3558,
    length: 10,
    convRule: rule8
  }, {
    start: 3570,
    length: 2,
    convRule: rule124
  }, {
    start: 3572,
    length: 1,
    convRule: rule2
  }, {
    start: 3585,
    length: 48,
    convRule: rule14
  }, {
    start: 3633,
    length: 1,
    convRule: rule92
  }, {
    start: 3634,
    length: 2,
    convRule: rule14
  }, {
    start: 3636,
    length: 7,
    convRule: rule92
  }, {
    start: 3647,
    length: 1,
    convRule: rule3
  }, {
    start: 3648,
    length: 6,
    convRule: rule14
  }, {
    start: 3654,
    length: 1,
    convRule: rule91
  }, {
    start: 3655,
    length: 8,
    convRule: rule92
  }, {
    start: 3663,
    length: 1,
    convRule: rule2
  }, {
    start: 3664,
    length: 10,
    convRule: rule8
  }, {
    start: 3674,
    length: 2,
    convRule: rule2
  }, {
    start: 3713,
    length: 2,
    convRule: rule14
  }, {
    start: 3716,
    length: 1,
    convRule: rule14
  }, {
    start: 3718,
    length: 5,
    convRule: rule14
  }, {
    start: 3724,
    length: 24,
    convRule: rule14
  }, {
    start: 3749,
    length: 1,
    convRule: rule14
  }, {
    start: 3751,
    length: 10,
    convRule: rule14
  }, {
    start: 3761,
    length: 1,
    convRule: rule92
  }, {
    start: 3762,
    length: 2,
    convRule: rule14
  }, {
    start: 3764,
    length: 9,
    convRule: rule92
  }, {
    start: 3773,
    length: 1,
    convRule: rule14
  }, {
    start: 3776,
    length: 5,
    convRule: rule14
  }, {
    start: 3782,
    length: 1,
    convRule: rule91
  }, {
    start: 3784,
    length: 6,
    convRule: rule92
  }, {
    start: 3792,
    length: 10,
    convRule: rule8
  }, {
    start: 3804,
    length: 4,
    convRule: rule14
  }, {
    start: 3840,
    length: 1,
    convRule: rule14
  }, {
    start: 3841,
    length: 3,
    convRule: rule13
  }, {
    start: 3844,
    length: 15,
    convRule: rule2
  }, {
    start: 3859,
    length: 1,
    convRule: rule13
  }, {
    start: 3860,
    length: 1,
    convRule: rule2
  }, {
    start: 3861,
    length: 3,
    convRule: rule13
  }, {
    start: 3864,
    length: 2,
    convRule: rule92
  }, {
    start: 3866,
    length: 6,
    convRule: rule13
  }, {
    start: 3872,
    length: 10,
    convRule: rule8
  }, {
    start: 3882,
    length: 10,
    convRule: rule17
  }, {
    start: 3892,
    length: 1,
    convRule: rule13
  }, {
    start: 3893,
    length: 1,
    convRule: rule92
  }, {
    start: 3894,
    length: 1,
    convRule: rule13
  }, {
    start: 3895,
    length: 1,
    convRule: rule92
  }, {
    start: 3896,
    length: 1,
    convRule: rule13
  }, {
    start: 3897,
    length: 1,
    convRule: rule92
  }, {
    start: 3898,
    length: 1,
    convRule: rule4
  }, {
    start: 3899,
    length: 1,
    convRule: rule5
  }, {
    start: 3900,
    length: 1,
    convRule: rule4
  }, {
    start: 3901,
    length: 1,
    convRule: rule5
  }, {
    start: 3902,
    length: 2,
    convRule: rule124
  }, {
    start: 3904,
    length: 8,
    convRule: rule14
  }, {
    start: 3913,
    length: 36,
    convRule: rule14
  }, {
    start: 3953,
    length: 14,
    convRule: rule92
  }, {
    start: 3967,
    length: 1,
    convRule: rule124
  }, {
    start: 3968,
    length: 5,
    convRule: rule92
  }, {
    start: 3973,
    length: 1,
    convRule: rule2
  }, {
    start: 3974,
    length: 2,
    convRule: rule92
  }, {
    start: 3976,
    length: 5,
    convRule: rule14
  }, {
    start: 3981,
    length: 11,
    convRule: rule92
  }, {
    start: 3993,
    length: 36,
    convRule: rule92
  }, {
    start: 4030,
    length: 8,
    convRule: rule13
  }, {
    start: 4038,
    length: 1,
    convRule: rule92
  }, {
    start: 4039,
    length: 6,
    convRule: rule13
  }, {
    start: 4046,
    length: 2,
    convRule: rule13
  }, {
    start: 4048,
    length: 5,
    convRule: rule2
  }, {
    start: 4053,
    length: 4,
    convRule: rule13
  }, {
    start: 4057,
    length: 2,
    convRule: rule2
  }, {
    start: 4096,
    length: 43,
    convRule: rule14
  }, {
    start: 4139,
    length: 2,
    convRule: rule124
  }, {
    start: 4141,
    length: 4,
    convRule: rule92
  }, {
    start: 4145,
    length: 1,
    convRule: rule124
  }, {
    start: 4146,
    length: 6,
    convRule: rule92
  }, {
    start: 4152,
    length: 1,
    convRule: rule124
  }, {
    start: 4153,
    length: 2,
    convRule: rule92
  }, {
    start: 4155,
    length: 2,
    convRule: rule124
  }, {
    start: 4157,
    length: 2,
    convRule: rule92
  }, {
    start: 4159,
    length: 1,
    convRule: rule14
  }, {
    start: 4160,
    length: 10,
    convRule: rule8
  }, {
    start: 4170,
    length: 6,
    convRule: rule2
  }, {
    start: 4176,
    length: 6,
    convRule: rule14
  }, {
    start: 4182,
    length: 2,
    convRule: rule124
  }, {
    start: 4184,
    length: 2,
    convRule: rule92
  }, {
    start: 4186,
    length: 4,
    convRule: rule14
  }, {
    start: 4190,
    length: 3,
    convRule: rule92
  }, {
    start: 4193,
    length: 1,
    convRule: rule14
  }, {
    start: 4194,
    length: 3,
    convRule: rule124
  }, {
    start: 4197,
    length: 2,
    convRule: rule14
  }, {
    start: 4199,
    length: 7,
    convRule: rule124
  }, {
    start: 4206,
    length: 3,
    convRule: rule14
  }, {
    start: 4209,
    length: 4,
    convRule: rule92
  }, {
    start: 4213,
    length: 13,
    convRule: rule14
  }, {
    start: 4226,
    length: 1,
    convRule: rule92
  }, {
    start: 4227,
    length: 2,
    convRule: rule124
  }, {
    start: 4229,
    length: 2,
    convRule: rule92
  }, {
    start: 4231,
    length: 6,
    convRule: rule124
  }, {
    start: 4237,
    length: 1,
    convRule: rule92
  }, {
    start: 4238,
    length: 1,
    convRule: rule14
  }, {
    start: 4239,
    length: 1,
    convRule: rule124
  }, {
    start: 4240,
    length: 10,
    convRule: rule8
  }, {
    start: 4250,
    length: 3,
    convRule: rule124
  }, {
    start: 4253,
    length: 1,
    convRule: rule92
  }, {
    start: 4254,
    length: 2,
    convRule: rule13
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4347,
    length: 1,
    convRule: rule2
  }, {
    start: 4348,
    length: 1,
    convRule: rule91
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 4352,
    length: 329,
    convRule: rule14
  }, {
    start: 4682,
    length: 4,
    convRule: rule14
  }, {
    start: 4688,
    length: 7,
    convRule: rule14
  }, {
    start: 4696,
    length: 1,
    convRule: rule14
  }, {
    start: 4698,
    length: 4,
    convRule: rule14
  }, {
    start: 4704,
    length: 41,
    convRule: rule14
  }, {
    start: 4746,
    length: 4,
    convRule: rule14
  }, {
    start: 4752,
    length: 33,
    convRule: rule14
  }, {
    start: 4786,
    length: 4,
    convRule: rule14
  }, {
    start: 4792,
    length: 7,
    convRule: rule14
  }, {
    start: 4800,
    length: 1,
    convRule: rule14
  }, {
    start: 4802,
    length: 4,
    convRule: rule14
  }, {
    start: 4808,
    length: 15,
    convRule: rule14
  }, {
    start: 4824,
    length: 57,
    convRule: rule14
  }, {
    start: 4882,
    length: 4,
    convRule: rule14
  }, {
    start: 4888,
    length: 67,
    convRule: rule14
  }, {
    start: 4957,
    length: 3,
    convRule: rule92
  }, {
    start: 4960,
    length: 9,
    convRule: rule2
  }, {
    start: 4969,
    length: 20,
    convRule: rule17
  }, {
    start: 4992,
    length: 16,
    convRule: rule14
  }, {
    start: 5008,
    length: 10,
    convRule: rule13
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 5120,
    length: 1,
    convRule: rule7
  }, {
    start: 5121,
    length: 620,
    convRule: rule14
  }, {
    start: 5741,
    length: 1,
    convRule: rule13
  }, {
    start: 5742,
    length: 1,
    convRule: rule2
  }, {
    start: 5743,
    length: 17,
    convRule: rule14
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 5761,
    length: 26,
    convRule: rule14
  }, {
    start: 5787,
    length: 1,
    convRule: rule4
  }, {
    start: 5788,
    length: 1,
    convRule: rule5
  }, {
    start: 5792,
    length: 75,
    convRule: rule14
  }, {
    start: 5867,
    length: 3,
    convRule: rule2
  }, {
    start: 5870,
    length: 3,
    convRule: rule128
  }, {
    start: 5873,
    length: 8,
    convRule: rule14
  }, {
    start: 5888,
    length: 13,
    convRule: rule14
  }, {
    start: 5902,
    length: 4,
    convRule: rule14
  }, {
    start: 5906,
    length: 3,
    convRule: rule92
  }, {
    start: 5920,
    length: 18,
    convRule: rule14
  }, {
    start: 5938,
    length: 3,
    convRule: rule92
  }, {
    start: 5941,
    length: 2,
    convRule: rule2
  }, {
    start: 5952,
    length: 18,
    convRule: rule14
  }, {
    start: 5970,
    length: 2,
    convRule: rule92
  }, {
    start: 5984,
    length: 13,
    convRule: rule14
  }, {
    start: 5998,
    length: 3,
    convRule: rule14
  }, {
    start: 6002,
    length: 2,
    convRule: rule92
  }, {
    start: 6016,
    length: 52,
    convRule: rule14
  }, {
    start: 6068,
    length: 2,
    convRule: rule92
  }, {
    start: 6070,
    length: 1,
    convRule: rule124
  }, {
    start: 6071,
    length: 7,
    convRule: rule92
  }, {
    start: 6078,
    length: 8,
    convRule: rule124
  }, {
    start: 6086,
    length: 1,
    convRule: rule92
  }, {
    start: 6087,
    length: 2,
    convRule: rule124
  }, {
    start: 6089,
    length: 11,
    convRule: rule92
  }, {
    start: 6100,
    length: 3,
    convRule: rule2
  }, {
    start: 6103,
    length: 1,
    convRule: rule91
  }, {
    start: 6104,
    length: 3,
    convRule: rule2
  }, {
    start: 6107,
    length: 1,
    convRule: rule3
  }, {
    start: 6108,
    length: 1,
    convRule: rule14
  }, {
    start: 6109,
    length: 1,
    convRule: rule92
  }, {
    start: 6112,
    length: 10,
    convRule: rule8
  }, {
    start: 6128,
    length: 10,
    convRule: rule17
  }, {
    start: 6144,
    length: 6,
    convRule: rule2
  }, {
    start: 6150,
    length: 1,
    convRule: rule7
  }, {
    start: 6151,
    length: 4,
    convRule: rule2
  }, {
    start: 6155,
    length: 3,
    convRule: rule92
  }, {
    start: 6158,
    length: 1,
    convRule: rule16
  }, {
    start: 6160,
    length: 10,
    convRule: rule8
  }, {
    start: 6176,
    length: 35,
    convRule: rule14
  }, {
    start: 6211,
    length: 1,
    convRule: rule91
  }, {
    start: 6212,
    length: 53,
    convRule: rule14
  }, {
    start: 6272,
    length: 5,
    convRule: rule14
  }, {
    start: 6277,
    length: 2,
    convRule: rule92
  }, {
    start: 6279,
    length: 34,
    convRule: rule14
  }, {
    start: 6313,
    length: 1,
    convRule: rule92
  }, {
    start: 6314,
    length: 1,
    convRule: rule14
  }, {
    start: 6320,
    length: 70,
    convRule: rule14
  }, {
    start: 6400,
    length: 31,
    convRule: rule14
  }, {
    start: 6432,
    length: 3,
    convRule: rule92
  }, {
    start: 6435,
    length: 4,
    convRule: rule124
  }, {
    start: 6439,
    length: 2,
    convRule: rule92
  }, {
    start: 6441,
    length: 3,
    convRule: rule124
  }, {
    start: 6448,
    length: 2,
    convRule: rule124
  }, {
    start: 6450,
    length: 1,
    convRule: rule92
  }, {
    start: 6451,
    length: 6,
    convRule: rule124
  }, {
    start: 6457,
    length: 3,
    convRule: rule92
  }, {
    start: 6464,
    length: 1,
    convRule: rule13
  }, {
    start: 6468,
    length: 2,
    convRule: rule2
  }, {
    start: 6470,
    length: 10,
    convRule: rule8
  }, {
    start: 6480,
    length: 30,
    convRule: rule14
  }, {
    start: 6512,
    length: 5,
    convRule: rule14
  }, {
    start: 6528,
    length: 44,
    convRule: rule14
  }, {
    start: 6576,
    length: 26,
    convRule: rule14
  }, {
    start: 6608,
    length: 10,
    convRule: rule8
  }, {
    start: 6618,
    length: 1,
    convRule: rule17
  }, {
    start: 6622,
    length: 34,
    convRule: rule13
  }, {
    start: 6656,
    length: 23,
    convRule: rule14
  }, {
    start: 6679,
    length: 2,
    convRule: rule92
  }, {
    start: 6681,
    length: 2,
    convRule: rule124
  }, {
    start: 6683,
    length: 1,
    convRule: rule92
  }, {
    start: 6686,
    length: 2,
    convRule: rule2
  }, {
    start: 6688,
    length: 53,
    convRule: rule14
  }, {
    start: 6741,
    length: 1,
    convRule: rule124
  }, {
    start: 6742,
    length: 1,
    convRule: rule92
  }, {
    start: 6743,
    length: 1,
    convRule: rule124
  }, {
    start: 6744,
    length: 7,
    convRule: rule92
  }, {
    start: 6752,
    length: 1,
    convRule: rule92
  }, {
    start: 6753,
    length: 1,
    convRule: rule124
  }, {
    start: 6754,
    length: 1,
    convRule: rule92
  }, {
    start: 6755,
    length: 2,
    convRule: rule124
  }, {
    start: 6757,
    length: 8,
    convRule: rule92
  }, {
    start: 6765,
    length: 6,
    convRule: rule124
  }, {
    start: 6771,
    length: 10,
    convRule: rule92
  }, {
    start: 6783,
    length: 1,
    convRule: rule92
  }, {
    start: 6784,
    length: 10,
    convRule: rule8
  }, {
    start: 6800,
    length: 10,
    convRule: rule8
  }, {
    start: 6816,
    length: 7,
    convRule: rule2
  }, {
    start: 6823,
    length: 1,
    convRule: rule91
  }, {
    start: 6824,
    length: 6,
    convRule: rule2
  }, {
    start: 6832,
    length: 14,
    convRule: rule92
  }, {
    start: 6846,
    length: 1,
    convRule: rule119
  }, {
    start: 6847,
    length: 2,
    convRule: rule92
  }, {
    start: 6912,
    length: 4,
    convRule: rule92
  }, {
    start: 6916,
    length: 1,
    convRule: rule124
  }, {
    start: 6917,
    length: 47,
    convRule: rule14
  }, {
    start: 6964,
    length: 1,
    convRule: rule92
  }, {
    start: 6965,
    length: 1,
    convRule: rule124
  }, {
    start: 6966,
    length: 5,
    convRule: rule92
  }, {
    start: 6971,
    length: 1,
    convRule: rule124
  }, {
    start: 6972,
    length: 1,
    convRule: rule92
  }, {
    start: 6973,
    length: 5,
    convRule: rule124
  }, {
    start: 6978,
    length: 1,
    convRule: rule92
  }, {
    start: 6979,
    length: 2,
    convRule: rule124
  }, {
    start: 6981,
    length: 7,
    convRule: rule14
  }, {
    start: 6992,
    length: 10,
    convRule: rule8
  }, {
    start: 7002,
    length: 7,
    convRule: rule2
  }, {
    start: 7009,
    length: 10,
    convRule: rule13
  }, {
    start: 7019,
    length: 9,
    convRule: rule92
  }, {
    start: 7028,
    length: 9,
    convRule: rule13
  }, {
    start: 7040,
    length: 2,
    convRule: rule92
  }, {
    start: 7042,
    length: 1,
    convRule: rule124
  }, {
    start: 7043,
    length: 30,
    convRule: rule14
  }, {
    start: 7073,
    length: 1,
    convRule: rule124
  }, {
    start: 7074,
    length: 4,
    convRule: rule92
  }, {
    start: 7078,
    length: 2,
    convRule: rule124
  }, {
    start: 7080,
    length: 2,
    convRule: rule92
  }, {
    start: 7082,
    length: 1,
    convRule: rule124
  }, {
    start: 7083,
    length: 3,
    convRule: rule92
  }, {
    start: 7086,
    length: 2,
    convRule: rule14
  }, {
    start: 7088,
    length: 10,
    convRule: rule8
  }, {
    start: 7098,
    length: 44,
    convRule: rule14
  }, {
    start: 7142,
    length: 1,
    convRule: rule92
  }, {
    start: 7143,
    length: 1,
    convRule: rule124
  }, {
    start: 7144,
    length: 2,
    convRule: rule92
  }, {
    start: 7146,
    length: 3,
    convRule: rule124
  }, {
    start: 7149,
    length: 1,
    convRule: rule92
  }, {
    start: 7150,
    length: 1,
    convRule: rule124
  }, {
    start: 7151,
    length: 3,
    convRule: rule92
  }, {
    start: 7154,
    length: 2,
    convRule: rule124
  }, {
    start: 7164,
    length: 4,
    convRule: rule2
  }, {
    start: 7168,
    length: 36,
    convRule: rule14
  }, {
    start: 7204,
    length: 8,
    convRule: rule124
  }, {
    start: 7212,
    length: 8,
    convRule: rule92
  }, {
    start: 7220,
    length: 2,
    convRule: rule124
  }, {
    start: 7222,
    length: 2,
    convRule: rule92
  }, {
    start: 7227,
    length: 5,
    convRule: rule2
  }, {
    start: 7232,
    length: 10,
    convRule: rule8
  }, {
    start: 7245,
    length: 3,
    convRule: rule14
  }, {
    start: 7248,
    length: 10,
    convRule: rule8
  }, {
    start: 7258,
    length: 30,
    convRule: rule14
  }, {
    start: 7288,
    length: 6,
    convRule: rule91
  }, {
    start: 7294,
    length: 2,
    convRule: rule2
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7360,
    length: 8,
    convRule: rule2
  }, {
    start: 7376,
    length: 3,
    convRule: rule92
  }, {
    start: 7379,
    length: 1,
    convRule: rule2
  }, {
    start: 7380,
    length: 13,
    convRule: rule92
  }, {
    start: 7393,
    length: 1,
    convRule: rule124
  }, {
    start: 7394,
    length: 7,
    convRule: rule92
  }, {
    start: 7401,
    length: 4,
    convRule: rule14
  }, {
    start: 7405,
    length: 1,
    convRule: rule92
  }, {
    start: 7406,
    length: 6,
    convRule: rule14
  }, {
    start: 7412,
    length: 1,
    convRule: rule92
  }, {
    start: 7413,
    length: 2,
    convRule: rule14
  }, {
    start: 7415,
    length: 1,
    convRule: rule124
  }, {
    start: 7416,
    length: 2,
    convRule: rule92
  }, {
    start: 7418,
    length: 1,
    convRule: rule14
  }, {
    start: 7424,
    length: 44,
    convRule: rule20
  }, {
    start: 7468,
    length: 63,
    convRule: rule91
  }, {
    start: 7531,
    length: 13,
    convRule: rule20
  }, {
    start: 7544,
    length: 1,
    convRule: rule91
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7546,
    length: 3,
    convRule: rule20
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7550,
    length: 16,
    convRule: rule20
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7567,
    length: 12,
    convRule: rule20
  }, {
    start: 7579,
    length: 37,
    convRule: rule91
  }, {
    start: 7616,
    length: 58,
    convRule: rule92
  }, {
    start: 7675,
    length: 5,
    convRule: rule92
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7830,
    length: 5,
    convRule: rule20
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7836,
    length: 2,
    convRule: rule20
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7839,
    length: 1,
    convRule: rule20
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8016,
    length: 1,
    convRule: rule20
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8018,
    length: 1,
    convRule: rule20
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8020,
    length: 1,
    convRule: rule20
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8022,
    length: 1,
    convRule: rule20
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8114,
    length: 1,
    convRule: rule20
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8116,
    length: 1,
    convRule: rule20
  }, {
    start: 8118,
    length: 2,
    convRule: rule20
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8125,
    length: 1,
    convRule: rule10
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8127,
    length: 3,
    convRule: rule10
  }, {
    start: 8130,
    length: 1,
    convRule: rule20
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8132,
    length: 1,
    convRule: rule20
  }, {
    start: 8134,
    length: 2,
    convRule: rule20
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8141,
    length: 3,
    convRule: rule10
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8146,
    length: 2,
    convRule: rule20
  }, {
    start: 8150,
    length: 2,
    convRule: rule20
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8157,
    length: 3,
    convRule: rule10
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8162,
    length: 3,
    convRule: rule20
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8166,
    length: 2,
    convRule: rule20
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8173,
    length: 3,
    convRule: rule10
  }, {
    start: 8178,
    length: 1,
    convRule: rule20
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8180,
    length: 1,
    convRule: rule20
  }, {
    start: 8182,
    length: 2,
    convRule: rule20
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8189,
    length: 2,
    convRule: rule10
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8203,
    length: 5,
    convRule: rule16
  }, {
    start: 8208,
    length: 6,
    convRule: rule7
  }, {
    start: 8214,
    length: 2,
    convRule: rule2
  }, {
    start: 8216,
    length: 1,
    convRule: rule15
  }, {
    start: 8217,
    length: 1,
    convRule: rule19
  }, {
    start: 8218,
    length: 1,
    convRule: rule4
  }, {
    start: 8219,
    length: 2,
    convRule: rule15
  }, {
    start: 8221,
    length: 1,
    convRule: rule19
  }, {
    start: 8222,
    length: 1,
    convRule: rule4
  }, {
    start: 8223,
    length: 1,
    convRule: rule15
  }, {
    start: 8224,
    length: 8,
    convRule: rule2
  }, {
    start: 8232,
    length: 1,
    convRule: rule161
  }, {
    start: 8233,
    length: 1,
    convRule: rule162
  }, {
    start: 8234,
    length: 5,
    convRule: rule16
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8240,
    length: 9,
    convRule: rule2
  }, {
    start: 8249,
    length: 1,
    convRule: rule15
  }, {
    start: 8250,
    length: 1,
    convRule: rule19
  }, {
    start: 8251,
    length: 4,
    convRule: rule2
  }, {
    start: 8255,
    length: 2,
    convRule: rule11
  }, {
    start: 8257,
    length: 3,
    convRule: rule2
  }, {
    start: 8260,
    length: 1,
    convRule: rule6
  }, {
    start: 8261,
    length: 1,
    convRule: rule4
  }, {
    start: 8262,
    length: 1,
    convRule: rule5
  }, {
    start: 8263,
    length: 11,
    convRule: rule2
  }, {
    start: 8274,
    length: 1,
    convRule: rule6
  }, {
    start: 8275,
    length: 1,
    convRule: rule2
  }, {
    start: 8276,
    length: 1,
    convRule: rule11
  }, {
    start: 8277,
    length: 10,
    convRule: rule2
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 8288,
    length: 5,
    convRule: rule16
  }, {
    start: 8294,
    length: 10,
    convRule: rule16
  }, {
    start: 8304,
    length: 1,
    convRule: rule17
  }, {
    start: 8305,
    length: 1,
    convRule: rule91
  }, {
    start: 8308,
    length: 6,
    convRule: rule17
  }, {
    start: 8314,
    length: 3,
    convRule: rule6
  }, {
    start: 8317,
    length: 1,
    convRule: rule4
  }, {
    start: 8318,
    length: 1,
    convRule: rule5
  }, {
    start: 8319,
    length: 1,
    convRule: rule91
  }, {
    start: 8320,
    length: 10,
    convRule: rule17
  }, {
    start: 8330,
    length: 3,
    convRule: rule6
  }, {
    start: 8333,
    length: 1,
    convRule: rule4
  }, {
    start: 8334,
    length: 1,
    convRule: rule5
  }, {
    start: 8336,
    length: 13,
    convRule: rule91
  }, {
    start: 8352,
    length: 32,
    convRule: rule3
  }, {
    start: 8400,
    length: 13,
    convRule: rule92
  }, {
    start: 8413,
    length: 4,
    convRule: rule119
  }, {
    start: 8417,
    length: 1,
    convRule: rule92
  }, {
    start: 8418,
    length: 3,
    convRule: rule119
  }, {
    start: 8421,
    length: 12,
    convRule: rule92
  }, {
    start: 8448,
    length: 2,
    convRule: rule13
  }, {
    start: 8450,
    length: 1,
    convRule: rule107
  }, {
    start: 8451,
    length: 4,
    convRule: rule13
  }, {
    start: 8455,
    length: 1,
    convRule: rule107
  }, {
    start: 8456,
    length: 2,
    convRule: rule13
  }, {
    start: 8458,
    length: 1,
    convRule: rule20
  }, {
    start: 8459,
    length: 3,
    convRule: rule107
  }, {
    start: 8462,
    length: 2,
    convRule: rule20
  }, {
    start: 8464,
    length: 3,
    convRule: rule107
  }, {
    start: 8467,
    length: 1,
    convRule: rule20
  }, {
    start: 8468,
    length: 1,
    convRule: rule13
  }, {
    start: 8469,
    length: 1,
    convRule: rule107
  }, {
    start: 8470,
    length: 2,
    convRule: rule13
  }, {
    start: 8472,
    length: 1,
    convRule: rule6
  }, {
    start: 8473,
    length: 5,
    convRule: rule107
  }, {
    start: 8478,
    length: 6,
    convRule: rule13
  }, {
    start: 8484,
    length: 1,
    convRule: rule107
  }, {
    start: 8485,
    length: 1,
    convRule: rule13
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8487,
    length: 1,
    convRule: rule13
  }, {
    start: 8488,
    length: 1,
    convRule: rule107
  }, {
    start: 8489,
    length: 1,
    convRule: rule13
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8492,
    length: 2,
    convRule: rule107
  }, {
    start: 8494,
    length: 1,
    convRule: rule13
  }, {
    start: 8495,
    length: 1,
    convRule: rule20
  }, {
    start: 8496,
    length: 2,
    convRule: rule107
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8499,
    length: 1,
    convRule: rule107
  }, {
    start: 8500,
    length: 1,
    convRule: rule20
  }, {
    start: 8501,
    length: 4,
    convRule: rule14
  }, {
    start: 8505,
    length: 1,
    convRule: rule20
  }, {
    start: 8506,
    length: 2,
    convRule: rule13
  }, {
    start: 8508,
    length: 2,
    convRule: rule20
  }, {
    start: 8510,
    length: 2,
    convRule: rule107
  }, {
    start: 8512,
    length: 5,
    convRule: rule6
  }, {
    start: 8517,
    length: 1,
    convRule: rule107
  }, {
    start: 8518,
    length: 4,
    convRule: rule20
  }, {
    start: 8522,
    length: 1,
    convRule: rule13
  }, {
    start: 8523,
    length: 1,
    convRule: rule6
  }, {
    start: 8524,
    length: 2,
    convRule: rule13
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8527,
    length: 1,
    convRule: rule13
  }, {
    start: 8528,
    length: 16,
    convRule: rule17
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8576,
    length: 3,
    convRule: rule128
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 8581,
    length: 4,
    convRule: rule128
  }, {
    start: 8585,
    length: 1,
    convRule: rule17
  }, {
    start: 8586,
    length: 2,
    convRule: rule13
  }, {
    start: 8592,
    length: 5,
    convRule: rule6
  }, {
    start: 8597,
    length: 5,
    convRule: rule13
  }, {
    start: 8602,
    length: 2,
    convRule: rule6
  }, {
    start: 8604,
    length: 4,
    convRule: rule13
  }, {
    start: 8608,
    length: 1,
    convRule: rule6
  }, {
    start: 8609,
    length: 2,
    convRule: rule13
  }, {
    start: 8611,
    length: 1,
    convRule: rule6
  }, {
    start: 8612,
    length: 2,
    convRule: rule13
  }, {
    start: 8614,
    length: 1,
    convRule: rule6
  }, {
    start: 8615,
    length: 7,
    convRule: rule13
  }, {
    start: 8622,
    length: 1,
    convRule: rule6
  }, {
    start: 8623,
    length: 31,
    convRule: rule13
  }, {
    start: 8654,
    length: 2,
    convRule: rule6
  }, {
    start: 8656,
    length: 2,
    convRule: rule13
  }, {
    start: 8658,
    length: 1,
    convRule: rule6
  }, {
    start: 8659,
    length: 1,
    convRule: rule13
  }, {
    start: 8660,
    length: 1,
    convRule: rule6
  }, {
    start: 8661,
    length: 31,
    convRule: rule13
  }, {
    start: 8692,
    length: 268,
    convRule: rule6
  }, {
    start: 8960,
    length: 8,
    convRule: rule13
  }, {
    start: 8968,
    length: 1,
    convRule: rule4
  }, {
    start: 8969,
    length: 1,
    convRule: rule5
  }, {
    start: 8970,
    length: 1,
    convRule: rule4
  }, {
    start: 8971,
    length: 1,
    convRule: rule5
  }, {
    start: 8972,
    length: 20,
    convRule: rule13
  }, {
    start: 8992,
    length: 2,
    convRule: rule6
  }, {
    start: 8994,
    length: 7,
    convRule: rule13
  }, {
    start: 9001,
    length: 1,
    convRule: rule4
  }, {
    start: 9002,
    length: 1,
    convRule: rule5
  }, {
    start: 9003,
    length: 81,
    convRule: rule13
  }, {
    start: 9084,
    length: 1,
    convRule: rule6
  }, {
    start: 9085,
    length: 30,
    convRule: rule13
  }, {
    start: 9115,
    length: 25,
    convRule: rule6
  }, {
    start: 9140,
    length: 40,
    convRule: rule13
  }, {
    start: 9180,
    length: 6,
    convRule: rule6
  }, {
    start: 9186,
    length: 69,
    convRule: rule13
  }, {
    start: 9280,
    length: 11,
    convRule: rule13
  }, {
    start: 9312,
    length: 60,
    convRule: rule17
  }, {
    start: 9372,
    length: 26,
    convRule: rule13
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 9450,
    length: 22,
    convRule: rule17
  }, {
    start: 9472,
    length: 183,
    convRule: rule13
  }, {
    start: 9655,
    length: 1,
    convRule: rule6
  }, {
    start: 9656,
    length: 9,
    convRule: rule13
  }, {
    start: 9665,
    length: 1,
    convRule: rule6
  }, {
    start: 9666,
    length: 54,
    convRule: rule13
  }, {
    start: 9720,
    length: 8,
    convRule: rule6
  }, {
    start: 9728,
    length: 111,
    convRule: rule13
  }, {
    start: 9839,
    length: 1,
    convRule: rule6
  }, {
    start: 9840,
    length: 248,
    convRule: rule13
  }, {
    start: 10088,
    length: 1,
    convRule: rule4
  }, {
    start: 10089,
    length: 1,
    convRule: rule5
  }, {
    start: 10090,
    length: 1,
    convRule: rule4
  }, {
    start: 10091,
    length: 1,
    convRule: rule5
  }, {
    start: 10092,
    length: 1,
    convRule: rule4
  }, {
    start: 10093,
    length: 1,
    convRule: rule5
  }, {
    start: 10094,
    length: 1,
    convRule: rule4
  }, {
    start: 10095,
    length: 1,
    convRule: rule5
  }, {
    start: 10096,
    length: 1,
    convRule: rule4
  }, {
    start: 10097,
    length: 1,
    convRule: rule5
  }, {
    start: 10098,
    length: 1,
    convRule: rule4
  }, {
    start: 10099,
    length: 1,
    convRule: rule5
  }, {
    start: 10100,
    length: 1,
    convRule: rule4
  }, {
    start: 10101,
    length: 1,
    convRule: rule5
  }, {
    start: 10102,
    length: 30,
    convRule: rule17
  }, {
    start: 10132,
    length: 44,
    convRule: rule13
  }, {
    start: 10176,
    length: 5,
    convRule: rule6
  }, {
    start: 10181,
    length: 1,
    convRule: rule4
  }, {
    start: 10182,
    length: 1,
    convRule: rule5
  }, {
    start: 10183,
    length: 31,
    convRule: rule6
  }, {
    start: 10214,
    length: 1,
    convRule: rule4
  }, {
    start: 10215,
    length: 1,
    convRule: rule5
  }, {
    start: 10216,
    length: 1,
    convRule: rule4
  }, {
    start: 10217,
    length: 1,
    convRule: rule5
  }, {
    start: 10218,
    length: 1,
    convRule: rule4
  }, {
    start: 10219,
    length: 1,
    convRule: rule5
  }, {
    start: 10220,
    length: 1,
    convRule: rule4
  }, {
    start: 10221,
    length: 1,
    convRule: rule5
  }, {
    start: 10222,
    length: 1,
    convRule: rule4
  }, {
    start: 10223,
    length: 1,
    convRule: rule5
  }, {
    start: 10224,
    length: 16,
    convRule: rule6
  }, {
    start: 10240,
    length: 256,
    convRule: rule13
  }, {
    start: 10496,
    length: 131,
    convRule: rule6
  }, {
    start: 10627,
    length: 1,
    convRule: rule4
  }, {
    start: 10628,
    length: 1,
    convRule: rule5
  }, {
    start: 10629,
    length: 1,
    convRule: rule4
  }, {
    start: 10630,
    length: 1,
    convRule: rule5
  }, {
    start: 10631,
    length: 1,
    convRule: rule4
  }, {
    start: 10632,
    length: 1,
    convRule: rule5
  }, {
    start: 10633,
    length: 1,
    convRule: rule4
  }, {
    start: 10634,
    length: 1,
    convRule: rule5
  }, {
    start: 10635,
    length: 1,
    convRule: rule4
  }, {
    start: 10636,
    length: 1,
    convRule: rule5
  }, {
    start: 10637,
    length: 1,
    convRule: rule4
  }, {
    start: 10638,
    length: 1,
    convRule: rule5
  }, {
    start: 10639,
    length: 1,
    convRule: rule4
  }, {
    start: 10640,
    length: 1,
    convRule: rule5
  }, {
    start: 10641,
    length: 1,
    convRule: rule4
  }, {
    start: 10642,
    length: 1,
    convRule: rule5
  }, {
    start: 10643,
    length: 1,
    convRule: rule4
  }, {
    start: 10644,
    length: 1,
    convRule: rule5
  }, {
    start: 10645,
    length: 1,
    convRule: rule4
  }, {
    start: 10646,
    length: 1,
    convRule: rule5
  }, {
    start: 10647,
    length: 1,
    convRule: rule4
  }, {
    start: 10648,
    length: 1,
    convRule: rule5
  }, {
    start: 10649,
    length: 63,
    convRule: rule6
  }, {
    start: 10712,
    length: 1,
    convRule: rule4
  }, {
    start: 10713,
    length: 1,
    convRule: rule5
  }, {
    start: 10714,
    length: 1,
    convRule: rule4
  }, {
    start: 10715,
    length: 1,
    convRule: rule5
  }, {
    start: 10716,
    length: 32,
    convRule: rule6
  }, {
    start: 10748,
    length: 1,
    convRule: rule4
  }, {
    start: 10749,
    length: 1,
    convRule: rule5
  }, {
    start: 10750,
    length: 258,
    convRule: rule6
  }, {
    start: 11008,
    length: 48,
    convRule: rule13
  }, {
    start: 11056,
    length: 21,
    convRule: rule6
  }, {
    start: 11077,
    length: 2,
    convRule: rule13
  }, {
    start: 11079,
    length: 6,
    convRule: rule6
  }, {
    start: 11085,
    length: 39,
    convRule: rule13
  }, {
    start: 11126,
    length: 32,
    convRule: rule13
  }, {
    start: 11159,
    length: 105,
    convRule: rule13
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11377,
    length: 1,
    convRule: rule20
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11380,
    length: 1,
    convRule: rule20
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11383,
    length: 5,
    convRule: rule20
  }, {
    start: 11388,
    length: 2,
    convRule: rule91
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11492,
    length: 1,
    convRule: rule20
  }, {
    start: 11493,
    length: 6,
    convRule: rule13
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11503,
    length: 3,
    convRule: rule92
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11513,
    length: 4,
    convRule: rule2
  }, {
    start: 11517,
    length: 1,
    convRule: rule17
  }, {
    start: 11518,
    length: 2,
    convRule: rule2
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 11568,
    length: 56,
    convRule: rule14
  }, {
    start: 11631,
    length: 1,
    convRule: rule91
  }, {
    start: 11632,
    length: 1,
    convRule: rule2
  }, {
    start: 11647,
    length: 1,
    convRule: rule92
  }, {
    start: 11648,
    length: 23,
    convRule: rule14
  }, {
    start: 11680,
    length: 7,
    convRule: rule14
  }, {
    start: 11688,
    length: 7,
    convRule: rule14
  }, {
    start: 11696,
    length: 7,
    convRule: rule14
  }, {
    start: 11704,
    length: 7,
    convRule: rule14
  }, {
    start: 11712,
    length: 7,
    convRule: rule14
  }, {
    start: 11720,
    length: 7,
    convRule: rule14
  }, {
    start: 11728,
    length: 7,
    convRule: rule14
  }, {
    start: 11736,
    length: 7,
    convRule: rule14
  }, {
    start: 11744,
    length: 32,
    convRule: rule92
  }, {
    start: 11776,
    length: 2,
    convRule: rule2
  }, {
    start: 11778,
    length: 1,
    convRule: rule15
  }, {
    start: 11779,
    length: 1,
    convRule: rule19
  }, {
    start: 11780,
    length: 1,
    convRule: rule15
  }, {
    start: 11781,
    length: 1,
    convRule: rule19
  }, {
    start: 11782,
    length: 3,
    convRule: rule2
  }, {
    start: 11785,
    length: 1,
    convRule: rule15
  }, {
    start: 11786,
    length: 1,
    convRule: rule19
  }, {
    start: 11787,
    length: 1,
    convRule: rule2
  }, {
    start: 11788,
    length: 1,
    convRule: rule15
  }, {
    start: 11789,
    length: 1,
    convRule: rule19
  }, {
    start: 11790,
    length: 9,
    convRule: rule2
  }, {
    start: 11799,
    length: 1,
    convRule: rule7
  }, {
    start: 11800,
    length: 2,
    convRule: rule2
  }, {
    start: 11802,
    length: 1,
    convRule: rule7
  }, {
    start: 11803,
    length: 1,
    convRule: rule2
  }, {
    start: 11804,
    length: 1,
    convRule: rule15
  }, {
    start: 11805,
    length: 1,
    convRule: rule19
  }, {
    start: 11806,
    length: 2,
    convRule: rule2
  }, {
    start: 11808,
    length: 1,
    convRule: rule15
  }, {
    start: 11809,
    length: 1,
    convRule: rule19
  }, {
    start: 11810,
    length: 1,
    convRule: rule4
  }, {
    start: 11811,
    length: 1,
    convRule: rule5
  }, {
    start: 11812,
    length: 1,
    convRule: rule4
  }, {
    start: 11813,
    length: 1,
    convRule: rule5
  }, {
    start: 11814,
    length: 1,
    convRule: rule4
  }, {
    start: 11815,
    length: 1,
    convRule: rule5
  }, {
    start: 11816,
    length: 1,
    convRule: rule4
  }, {
    start: 11817,
    length: 1,
    convRule: rule5
  }, {
    start: 11818,
    length: 5,
    convRule: rule2
  }, {
    start: 11823,
    length: 1,
    convRule: rule91
  }, {
    start: 11824,
    length: 10,
    convRule: rule2
  }, {
    start: 11834,
    length: 2,
    convRule: rule7
  }, {
    start: 11836,
    length: 4,
    convRule: rule2
  }, {
    start: 11840,
    length: 1,
    convRule: rule7
  }, {
    start: 11841,
    length: 1,
    convRule: rule2
  }, {
    start: 11842,
    length: 1,
    convRule: rule4
  }, {
    start: 11843,
    length: 13,
    convRule: rule2
  }, {
    start: 11856,
    length: 2,
    convRule: rule13
  }, {
    start: 11858,
    length: 1,
    convRule: rule2
  }, {
    start: 11904,
    length: 26,
    convRule: rule13
  }, {
    start: 11931,
    length: 89,
    convRule: rule13
  }, {
    start: 12032,
    length: 214,
    convRule: rule13
  }, {
    start: 12272,
    length: 12,
    convRule: rule13
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }, {
    start: 12289,
    length: 3,
    convRule: rule2
  }, {
    start: 12292,
    length: 1,
    convRule: rule13
  }, {
    start: 12293,
    length: 1,
    convRule: rule91
  }, {
    start: 12294,
    length: 1,
    convRule: rule14
  }, {
    start: 12295,
    length: 1,
    convRule: rule128
  }, {
    start: 12296,
    length: 1,
    convRule: rule4
  }, {
    start: 12297,
    length: 1,
    convRule: rule5
  }, {
    start: 12298,
    length: 1,
    convRule: rule4
  }, {
    start: 12299,
    length: 1,
    convRule: rule5
  }, {
    start: 12300,
    length: 1,
    convRule: rule4
  }, {
    start: 12301,
    length: 1,
    convRule: rule5
  }, {
    start: 12302,
    length: 1,
    convRule: rule4
  }, {
    start: 12303,
    length: 1,
    convRule: rule5
  }, {
    start: 12304,
    length: 1,
    convRule: rule4
  }, {
    start: 12305,
    length: 1,
    convRule: rule5
  }, {
    start: 12306,
    length: 2,
    convRule: rule13
  }, {
    start: 12308,
    length: 1,
    convRule: rule4
  }, {
    start: 12309,
    length: 1,
    convRule: rule5
  }, {
    start: 12310,
    length: 1,
    convRule: rule4
  }, {
    start: 12311,
    length: 1,
    convRule: rule5
  }, {
    start: 12312,
    length: 1,
    convRule: rule4
  }, {
    start: 12313,
    length: 1,
    convRule: rule5
  }, {
    start: 12314,
    length: 1,
    convRule: rule4
  }, {
    start: 12315,
    length: 1,
    convRule: rule5
  }, {
    start: 12316,
    length: 1,
    convRule: rule7
  }, {
    start: 12317,
    length: 1,
    convRule: rule4
  }, {
    start: 12318,
    length: 2,
    convRule: rule5
  }, {
    start: 12320,
    length: 1,
    convRule: rule13
  }, {
    start: 12321,
    length: 9,
    convRule: rule128
  }, {
    start: 12330,
    length: 4,
    convRule: rule92
  }, {
    start: 12334,
    length: 2,
    convRule: rule124
  }, {
    start: 12336,
    length: 1,
    convRule: rule7
  }, {
    start: 12337,
    length: 5,
    convRule: rule91
  }, {
    start: 12342,
    length: 2,
    convRule: rule13
  }, {
    start: 12344,
    length: 3,
    convRule: rule128
  }, {
    start: 12347,
    length: 1,
    convRule: rule91
  }, {
    start: 12348,
    length: 1,
    convRule: rule14
  }, {
    start: 12349,
    length: 1,
    convRule: rule2
  }, {
    start: 12350,
    length: 2,
    convRule: rule13
  }, {
    start: 12353,
    length: 86,
    convRule: rule14
  }, {
    start: 12441,
    length: 2,
    convRule: rule92
  }, {
    start: 12443,
    length: 2,
    convRule: rule10
  }, {
    start: 12445,
    length: 2,
    convRule: rule91
  }, {
    start: 12447,
    length: 1,
    convRule: rule14
  }, {
    start: 12448,
    length: 1,
    convRule: rule7
  }, {
    start: 12449,
    length: 90,
    convRule: rule14
  }, {
    start: 12539,
    length: 1,
    convRule: rule2
  }, {
    start: 12540,
    length: 3,
    convRule: rule91
  }, {
    start: 12543,
    length: 1,
    convRule: rule14
  }, {
    start: 12549,
    length: 43,
    convRule: rule14
  }, {
    start: 12593,
    length: 94,
    convRule: rule14
  }, {
    start: 12688,
    length: 2,
    convRule: rule13
  }, {
    start: 12690,
    length: 4,
    convRule: rule17
  }, {
    start: 12694,
    length: 10,
    convRule: rule13
  }, {
    start: 12704,
    length: 32,
    convRule: rule14
  }, {
    start: 12736,
    length: 36,
    convRule: rule13
  }, {
    start: 12784,
    length: 16,
    convRule: rule14
  }, {
    start: 12800,
    length: 31,
    convRule: rule13
  }, {
    start: 12832,
    length: 10,
    convRule: rule17
  }, {
    start: 12842,
    length: 30,
    convRule: rule13
  }, {
    start: 12872,
    length: 8,
    convRule: rule17
  }, {
    start: 12880,
    length: 1,
    convRule: rule13
  }, {
    start: 12881,
    length: 15,
    convRule: rule17
  }, {
    start: 12896,
    length: 32,
    convRule: rule13
  }, {
    start: 12928,
    length: 10,
    convRule: rule17
  }, {
    start: 12938,
    length: 39,
    convRule: rule13
  }, {
    start: 12977,
    length: 15,
    convRule: rule17
  }, {
    start: 12992,
    length: 320,
    convRule: rule13
  }, {
    start: 13312,
    length: 6592,
    convRule: rule14
  }, {
    start: 19904,
    length: 64,
    convRule: rule13
  }, {
    start: 19968,
    length: 20989,
    convRule: rule14
  }, {
    start: 40960,
    length: 21,
    convRule: rule14
  }, {
    start: 40981,
    length: 1,
    convRule: rule91
  }, {
    start: 40982,
    length: 1143,
    convRule: rule14
  }, {
    start: 42128,
    length: 55,
    convRule: rule13
  }, {
    start: 42192,
    length: 40,
    convRule: rule14
  }, {
    start: 42232,
    length: 6,
    convRule: rule91
  }, {
    start: 42238,
    length: 2,
    convRule: rule2
  }, {
    start: 42240,
    length: 268,
    convRule: rule14
  }, {
    start: 42508,
    length: 1,
    convRule: rule91
  }, {
    start: 42509,
    length: 3,
    convRule: rule2
  }, {
    start: 42512,
    length: 16,
    convRule: rule14
  }, {
    start: 42528,
    length: 10,
    convRule: rule8
  }, {
    start: 42538,
    length: 2,
    convRule: rule14
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42606,
    length: 1,
    convRule: rule14
  }, {
    start: 42607,
    length: 1,
    convRule: rule92
  }, {
    start: 42608,
    length: 3,
    convRule: rule119
  }, {
    start: 42611,
    length: 1,
    convRule: rule2
  }, {
    start: 42612,
    length: 10,
    convRule: rule92
  }, {
    start: 42622,
    length: 1,
    convRule: rule2
  }, {
    start: 42623,
    length: 1,
    convRule: rule91
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42652,
    length: 2,
    convRule: rule91
  }, {
    start: 42654,
    length: 2,
    convRule: rule92
  }, {
    start: 42656,
    length: 70,
    convRule: rule14
  }, {
    start: 42726,
    length: 10,
    convRule: rule128
  }, {
    start: 42736,
    length: 2,
    convRule: rule92
  }, {
    start: 42738,
    length: 6,
    convRule: rule2
  }, {
    start: 42752,
    length: 23,
    convRule: rule10
  }, {
    start: 42775,
    length: 9,
    convRule: rule91
  }, {
    start: 42784,
    length: 2,
    convRule: rule10
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42800,
    length: 2,
    convRule: rule20
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42864,
    length: 1,
    convRule: rule91
  }, {
    start: 42865,
    length: 8,
    convRule: rule20
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42888,
    length: 1,
    convRule: rule91
  }, {
    start: 42889,
    length: 2,
    convRule: rule10
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42894,
    length: 1,
    convRule: rule20
  }, {
    start: 42895,
    length: 1,
    convRule: rule14
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42901,
    length: 1,
    convRule: rule20
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42927,
    length: 1,
    convRule: rule20
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 42999,
    length: 1,
    convRule: rule14
  }, {
    start: 43e3,
    length: 2,
    convRule: rule91
  }, {
    start: 43002,
    length: 1,
    convRule: rule20
  }, {
    start: 43003,
    length: 7,
    convRule: rule14
  }, {
    start: 43010,
    length: 1,
    convRule: rule92
  }, {
    start: 43011,
    length: 3,
    convRule: rule14
  }, {
    start: 43014,
    length: 1,
    convRule: rule92
  }, {
    start: 43015,
    length: 4,
    convRule: rule14
  }, {
    start: 43019,
    length: 1,
    convRule: rule92
  }, {
    start: 43020,
    length: 23,
    convRule: rule14
  }, {
    start: 43043,
    length: 2,
    convRule: rule124
  }, {
    start: 43045,
    length: 2,
    convRule: rule92
  }, {
    start: 43047,
    length: 1,
    convRule: rule124
  }, {
    start: 43048,
    length: 4,
    convRule: rule13
  }, {
    start: 43052,
    length: 1,
    convRule: rule92
  }, {
    start: 43056,
    length: 6,
    convRule: rule17
  }, {
    start: 43062,
    length: 2,
    convRule: rule13
  }, {
    start: 43064,
    length: 1,
    convRule: rule3
  }, {
    start: 43065,
    length: 1,
    convRule: rule13
  }, {
    start: 43072,
    length: 52,
    convRule: rule14
  }, {
    start: 43124,
    length: 4,
    convRule: rule2
  }, {
    start: 43136,
    length: 2,
    convRule: rule124
  }, {
    start: 43138,
    length: 50,
    convRule: rule14
  }, {
    start: 43188,
    length: 16,
    convRule: rule124
  }, {
    start: 43204,
    length: 2,
    convRule: rule92
  }, {
    start: 43214,
    length: 2,
    convRule: rule2
  }, {
    start: 43216,
    length: 10,
    convRule: rule8
  }, {
    start: 43232,
    length: 18,
    convRule: rule92
  }, {
    start: 43250,
    length: 6,
    convRule: rule14
  }, {
    start: 43256,
    length: 3,
    convRule: rule2
  }, {
    start: 43259,
    length: 1,
    convRule: rule14
  }, {
    start: 43260,
    length: 1,
    convRule: rule2
  }, {
    start: 43261,
    length: 2,
    convRule: rule14
  }, {
    start: 43263,
    length: 1,
    convRule: rule92
  }, {
    start: 43264,
    length: 10,
    convRule: rule8
  }, {
    start: 43274,
    length: 28,
    convRule: rule14
  }, {
    start: 43302,
    length: 8,
    convRule: rule92
  }, {
    start: 43310,
    length: 2,
    convRule: rule2
  }, {
    start: 43312,
    length: 23,
    convRule: rule14
  }, {
    start: 43335,
    length: 11,
    convRule: rule92
  }, {
    start: 43346,
    length: 2,
    convRule: rule124
  }, {
    start: 43359,
    length: 1,
    convRule: rule2
  }, {
    start: 43360,
    length: 29,
    convRule: rule14
  }, {
    start: 43392,
    length: 3,
    convRule: rule92
  }, {
    start: 43395,
    length: 1,
    convRule: rule124
  }, {
    start: 43396,
    length: 47,
    convRule: rule14
  }, {
    start: 43443,
    length: 1,
    convRule: rule92
  }, {
    start: 43444,
    length: 2,
    convRule: rule124
  }, {
    start: 43446,
    length: 4,
    convRule: rule92
  }, {
    start: 43450,
    length: 2,
    convRule: rule124
  }, {
    start: 43452,
    length: 2,
    convRule: rule92
  }, {
    start: 43454,
    length: 3,
    convRule: rule124
  }, {
    start: 43457,
    length: 13,
    convRule: rule2
  }, {
    start: 43471,
    length: 1,
    convRule: rule91
  }, {
    start: 43472,
    length: 10,
    convRule: rule8
  }, {
    start: 43486,
    length: 2,
    convRule: rule2
  }, {
    start: 43488,
    length: 5,
    convRule: rule14
  }, {
    start: 43493,
    length: 1,
    convRule: rule92
  }, {
    start: 43494,
    length: 1,
    convRule: rule91
  }, {
    start: 43495,
    length: 9,
    convRule: rule14
  }, {
    start: 43504,
    length: 10,
    convRule: rule8
  }, {
    start: 43514,
    length: 5,
    convRule: rule14
  }, {
    start: 43520,
    length: 41,
    convRule: rule14
  }, {
    start: 43561,
    length: 6,
    convRule: rule92
  }, {
    start: 43567,
    length: 2,
    convRule: rule124
  }, {
    start: 43569,
    length: 2,
    convRule: rule92
  }, {
    start: 43571,
    length: 2,
    convRule: rule124
  }, {
    start: 43573,
    length: 2,
    convRule: rule92
  }, {
    start: 43584,
    length: 3,
    convRule: rule14
  }, {
    start: 43587,
    length: 1,
    convRule: rule92
  }, {
    start: 43588,
    length: 8,
    convRule: rule14
  }, {
    start: 43596,
    length: 1,
    convRule: rule92
  }, {
    start: 43597,
    length: 1,
    convRule: rule124
  }, {
    start: 43600,
    length: 10,
    convRule: rule8
  }, {
    start: 43612,
    length: 4,
    convRule: rule2
  }, {
    start: 43616,
    length: 16,
    convRule: rule14
  }, {
    start: 43632,
    length: 1,
    convRule: rule91
  }, {
    start: 43633,
    length: 6,
    convRule: rule14
  }, {
    start: 43639,
    length: 3,
    convRule: rule13
  }, {
    start: 43642,
    length: 1,
    convRule: rule14
  }, {
    start: 43643,
    length: 1,
    convRule: rule124
  }, {
    start: 43644,
    length: 1,
    convRule: rule92
  }, {
    start: 43645,
    length: 1,
    convRule: rule124
  }, {
    start: 43646,
    length: 50,
    convRule: rule14
  }, {
    start: 43696,
    length: 1,
    convRule: rule92
  }, {
    start: 43697,
    length: 1,
    convRule: rule14
  }, {
    start: 43698,
    length: 3,
    convRule: rule92
  }, {
    start: 43701,
    length: 2,
    convRule: rule14
  }, {
    start: 43703,
    length: 2,
    convRule: rule92
  }, {
    start: 43705,
    length: 5,
    convRule: rule14
  }, {
    start: 43710,
    length: 2,
    convRule: rule92
  }, {
    start: 43712,
    length: 1,
    convRule: rule14
  }, {
    start: 43713,
    length: 1,
    convRule: rule92
  }, {
    start: 43714,
    length: 1,
    convRule: rule14
  }, {
    start: 43739,
    length: 2,
    convRule: rule14
  }, {
    start: 43741,
    length: 1,
    convRule: rule91
  }, {
    start: 43742,
    length: 2,
    convRule: rule2
  }, {
    start: 43744,
    length: 11,
    convRule: rule14
  }, {
    start: 43755,
    length: 1,
    convRule: rule124
  }, {
    start: 43756,
    length: 2,
    convRule: rule92
  }, {
    start: 43758,
    length: 2,
    convRule: rule124
  }, {
    start: 43760,
    length: 2,
    convRule: rule2
  }, {
    start: 43762,
    length: 1,
    convRule: rule14
  }, {
    start: 43763,
    length: 2,
    convRule: rule91
  }, {
    start: 43765,
    length: 1,
    convRule: rule124
  }, {
    start: 43766,
    length: 1,
    convRule: rule92
  }, {
    start: 43777,
    length: 6,
    convRule: rule14
  }, {
    start: 43785,
    length: 6,
    convRule: rule14
  }, {
    start: 43793,
    length: 6,
    convRule: rule14
  }, {
    start: 43808,
    length: 7,
    convRule: rule14
  }, {
    start: 43816,
    length: 7,
    convRule: rule14
  }, {
    start: 43824,
    length: 35,
    convRule: rule20
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43860,
    length: 7,
    convRule: rule20
  }, {
    start: 43867,
    length: 1,
    convRule: rule10
  }, {
    start: 43868,
    length: 4,
    convRule: rule91
  }, {
    start: 43872,
    length: 9,
    convRule: rule20
  }, {
    start: 43881,
    length: 1,
    convRule: rule91
  }, {
    start: 43882,
    length: 2,
    convRule: rule10
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 43968,
    length: 35,
    convRule: rule14
  }, {
    start: 44003,
    length: 2,
    convRule: rule124
  }, {
    start: 44005,
    length: 1,
    convRule: rule92
  }, {
    start: 44006,
    length: 2,
    convRule: rule124
  }, {
    start: 44008,
    length: 1,
    convRule: rule92
  }, {
    start: 44009,
    length: 2,
    convRule: rule124
  }, {
    start: 44011,
    length: 1,
    convRule: rule2
  }, {
    start: 44012,
    length: 1,
    convRule: rule124
  }, {
    start: 44013,
    length: 1,
    convRule: rule92
  }, {
    start: 44016,
    length: 10,
    convRule: rule8
  }, {
    start: 44032,
    length: 11172,
    convRule: rule14
  }, {
    start: 55216,
    length: 23,
    convRule: rule14
  }, {
    start: 55243,
    length: 49,
    convRule: rule14
  }, {
    start: 55296,
    length: 896,
    convRule: rule199
  }, {
    start: 56192,
    length: 128,
    convRule: rule199
  }, {
    start: 56320,
    length: 1024,
    convRule: rule199
  }, {
    start: 57344,
    length: 6400,
    convRule: rule200
  }, {
    start: 63744,
    length: 366,
    convRule: rule14
  }, {
    start: 64112,
    length: 106,
    convRule: rule14
  }, {
    start: 64256,
    length: 7,
    convRule: rule20
  }, {
    start: 64275,
    length: 5,
    convRule: rule20
  }, {
    start: 64285,
    length: 1,
    convRule: rule14
  }, {
    start: 64286,
    length: 1,
    convRule: rule92
  }, {
    start: 64287,
    length: 10,
    convRule: rule14
  }, {
    start: 64297,
    length: 1,
    convRule: rule6
  }, {
    start: 64298,
    length: 13,
    convRule: rule14
  }, {
    start: 64312,
    length: 5,
    convRule: rule14
  }, {
    start: 64318,
    length: 1,
    convRule: rule14
  }, {
    start: 64320,
    length: 2,
    convRule: rule14
  }, {
    start: 64323,
    length: 2,
    convRule: rule14
  }, {
    start: 64326,
    length: 108,
    convRule: rule14
  }, {
    start: 64434,
    length: 16,
    convRule: rule10
  }, {
    start: 64467,
    length: 363,
    convRule: rule14
  }, {
    start: 64830,
    length: 1,
    convRule: rule5
  }, {
    start: 64831,
    length: 1,
    convRule: rule4
  }, {
    start: 64848,
    length: 64,
    convRule: rule14
  }, {
    start: 64914,
    length: 54,
    convRule: rule14
  }, {
    start: 65008,
    length: 12,
    convRule: rule14
  }, {
    start: 65020,
    length: 1,
    convRule: rule3
  }, {
    start: 65021,
    length: 1,
    convRule: rule13
  }, {
    start: 65024,
    length: 16,
    convRule: rule92
  }, {
    start: 65040,
    length: 7,
    convRule: rule2
  }, {
    start: 65047,
    length: 1,
    convRule: rule4
  }, {
    start: 65048,
    length: 1,
    convRule: rule5
  }, {
    start: 65049,
    length: 1,
    convRule: rule2
  }, {
    start: 65056,
    length: 16,
    convRule: rule92
  }, {
    start: 65072,
    length: 1,
    convRule: rule2
  }, {
    start: 65073,
    length: 2,
    convRule: rule7
  }, {
    start: 65075,
    length: 2,
    convRule: rule11
  }, {
    start: 65077,
    length: 1,
    convRule: rule4
  }, {
    start: 65078,
    length: 1,
    convRule: rule5
  }, {
    start: 65079,
    length: 1,
    convRule: rule4
  }, {
    start: 65080,
    length: 1,
    convRule: rule5
  }, {
    start: 65081,
    length: 1,
    convRule: rule4
  }, {
    start: 65082,
    length: 1,
    convRule: rule5
  }, {
    start: 65083,
    length: 1,
    convRule: rule4
  }, {
    start: 65084,
    length: 1,
    convRule: rule5
  }, {
    start: 65085,
    length: 1,
    convRule: rule4
  }, {
    start: 65086,
    length: 1,
    convRule: rule5
  }, {
    start: 65087,
    length: 1,
    convRule: rule4
  }, {
    start: 65088,
    length: 1,
    convRule: rule5
  }, {
    start: 65089,
    length: 1,
    convRule: rule4
  }, {
    start: 65090,
    length: 1,
    convRule: rule5
  }, {
    start: 65091,
    length: 1,
    convRule: rule4
  }, {
    start: 65092,
    length: 1,
    convRule: rule5
  }, {
    start: 65093,
    length: 2,
    convRule: rule2
  }, {
    start: 65095,
    length: 1,
    convRule: rule4
  }, {
    start: 65096,
    length: 1,
    convRule: rule5
  }, {
    start: 65097,
    length: 4,
    convRule: rule2
  }, {
    start: 65101,
    length: 3,
    convRule: rule11
  }, {
    start: 65104,
    length: 3,
    convRule: rule2
  }, {
    start: 65108,
    length: 4,
    convRule: rule2
  }, {
    start: 65112,
    length: 1,
    convRule: rule7
  }, {
    start: 65113,
    length: 1,
    convRule: rule4
  }, {
    start: 65114,
    length: 1,
    convRule: rule5
  }, {
    start: 65115,
    length: 1,
    convRule: rule4
  }, {
    start: 65116,
    length: 1,
    convRule: rule5
  }, {
    start: 65117,
    length: 1,
    convRule: rule4
  }, {
    start: 65118,
    length: 1,
    convRule: rule5
  }, {
    start: 65119,
    length: 3,
    convRule: rule2
  }, {
    start: 65122,
    length: 1,
    convRule: rule6
  }, {
    start: 65123,
    length: 1,
    convRule: rule7
  }, {
    start: 65124,
    length: 3,
    convRule: rule6
  }, {
    start: 65128,
    length: 1,
    convRule: rule2
  }, {
    start: 65129,
    length: 1,
    convRule: rule3
  }, {
    start: 65130,
    length: 2,
    convRule: rule2
  }, {
    start: 65136,
    length: 5,
    convRule: rule14
  }, {
    start: 65142,
    length: 135,
    convRule: rule14
  }, {
    start: 65279,
    length: 1,
    convRule: rule16
  }, {
    start: 65281,
    length: 3,
    convRule: rule2
  }, {
    start: 65284,
    length: 1,
    convRule: rule3
  }, {
    start: 65285,
    length: 3,
    convRule: rule2
  }, {
    start: 65288,
    length: 1,
    convRule: rule4
  }, {
    start: 65289,
    length: 1,
    convRule: rule5
  }, {
    start: 65290,
    length: 1,
    convRule: rule2
  }, {
    start: 65291,
    length: 1,
    convRule: rule6
  }, {
    start: 65292,
    length: 1,
    convRule: rule2
  }, {
    start: 65293,
    length: 1,
    convRule: rule7
  }, {
    start: 65294,
    length: 2,
    convRule: rule2
  }, {
    start: 65296,
    length: 10,
    convRule: rule8
  }, {
    start: 65306,
    length: 2,
    convRule: rule2
  }, {
    start: 65308,
    length: 3,
    convRule: rule6
  }, {
    start: 65311,
    length: 2,
    convRule: rule2
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65339,
    length: 1,
    convRule: rule4
  }, {
    start: 65340,
    length: 1,
    convRule: rule2
  }, {
    start: 65341,
    length: 1,
    convRule: rule5
  }, {
    start: 65342,
    length: 1,
    convRule: rule10
  }, {
    start: 65343,
    length: 1,
    convRule: rule11
  }, {
    start: 65344,
    length: 1,
    convRule: rule10
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 65371,
    length: 1,
    convRule: rule4
  }, {
    start: 65372,
    length: 1,
    convRule: rule6
  }, {
    start: 65373,
    length: 1,
    convRule: rule5
  }, {
    start: 65374,
    length: 1,
    convRule: rule6
  }, {
    start: 65375,
    length: 1,
    convRule: rule4
  }, {
    start: 65376,
    length: 1,
    convRule: rule5
  }, {
    start: 65377,
    length: 1,
    convRule: rule2
  }, {
    start: 65378,
    length: 1,
    convRule: rule4
  }, {
    start: 65379,
    length: 1,
    convRule: rule5
  }, {
    start: 65380,
    length: 2,
    convRule: rule2
  }, {
    start: 65382,
    length: 10,
    convRule: rule14
  }, {
    start: 65392,
    length: 1,
    convRule: rule91
  }, {
    start: 65393,
    length: 45,
    convRule: rule14
  }, {
    start: 65438,
    length: 2,
    convRule: rule91
  }, {
    start: 65440,
    length: 31,
    convRule: rule14
  }, {
    start: 65474,
    length: 6,
    convRule: rule14
  }, {
    start: 65482,
    length: 6,
    convRule: rule14
  }, {
    start: 65490,
    length: 6,
    convRule: rule14
  }, {
    start: 65498,
    length: 3,
    convRule: rule14
  }, {
    start: 65504,
    length: 2,
    convRule: rule3
  }, {
    start: 65506,
    length: 1,
    convRule: rule6
  }, {
    start: 65507,
    length: 1,
    convRule: rule10
  }, {
    start: 65508,
    length: 1,
    convRule: rule13
  }, {
    start: 65509,
    length: 2,
    convRule: rule3
  }, {
    start: 65512,
    length: 1,
    convRule: rule13
  }, {
    start: 65513,
    length: 4,
    convRule: rule6
  }, {
    start: 65517,
    length: 2,
    convRule: rule13
  }, {
    start: 65529,
    length: 3,
    convRule: rule16
  }, {
    start: 65532,
    length: 2,
    convRule: rule13
  }, {
    start: 65536,
    length: 12,
    convRule: rule14
  }, {
    start: 65549,
    length: 26,
    convRule: rule14
  }, {
    start: 65576,
    length: 19,
    convRule: rule14
  }, {
    start: 65596,
    length: 2,
    convRule: rule14
  }, {
    start: 65599,
    length: 15,
    convRule: rule14
  }, {
    start: 65616,
    length: 14,
    convRule: rule14
  }, {
    start: 65664,
    length: 123,
    convRule: rule14
  }, {
    start: 65792,
    length: 3,
    convRule: rule2
  }, {
    start: 65799,
    length: 45,
    convRule: rule17
  }, {
    start: 65847,
    length: 9,
    convRule: rule13
  }, {
    start: 65856,
    length: 53,
    convRule: rule128
  }, {
    start: 65909,
    length: 4,
    convRule: rule17
  }, {
    start: 65913,
    length: 17,
    convRule: rule13
  }, {
    start: 65930,
    length: 2,
    convRule: rule17
  }, {
    start: 65932,
    length: 3,
    convRule: rule13
  }, {
    start: 65936,
    length: 13,
    convRule: rule13
  }, {
    start: 65952,
    length: 1,
    convRule: rule13
  }, {
    start: 66e3,
    length: 45,
    convRule: rule13
  }, {
    start: 66045,
    length: 1,
    convRule: rule92
  }, {
    start: 66176,
    length: 29,
    convRule: rule14
  }, {
    start: 66208,
    length: 49,
    convRule: rule14
  }, {
    start: 66272,
    length: 1,
    convRule: rule92
  }, {
    start: 66273,
    length: 27,
    convRule: rule17
  }, {
    start: 66304,
    length: 32,
    convRule: rule14
  }, {
    start: 66336,
    length: 4,
    convRule: rule17
  }, {
    start: 66349,
    length: 20,
    convRule: rule14
  }, {
    start: 66369,
    length: 1,
    convRule: rule128
  }, {
    start: 66370,
    length: 8,
    convRule: rule14
  }, {
    start: 66378,
    length: 1,
    convRule: rule128
  }, {
    start: 66384,
    length: 38,
    convRule: rule14
  }, {
    start: 66422,
    length: 5,
    convRule: rule92
  }, {
    start: 66432,
    length: 30,
    convRule: rule14
  }, {
    start: 66463,
    length: 1,
    convRule: rule2
  }, {
    start: 66464,
    length: 36,
    convRule: rule14
  }, {
    start: 66504,
    length: 8,
    convRule: rule14
  }, {
    start: 66512,
    length: 1,
    convRule: rule2
  }, {
    start: 66513,
    length: 5,
    convRule: rule128
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66640,
    length: 78,
    convRule: rule14
  }, {
    start: 66720,
    length: 10,
    convRule: rule8
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 66816,
    length: 40,
    convRule: rule14
  }, {
    start: 66864,
    length: 52,
    convRule: rule14
  }, {
    start: 66927,
    length: 1,
    convRule: rule2
  }, {
    start: 67072,
    length: 311,
    convRule: rule14
  }, {
    start: 67392,
    length: 22,
    convRule: rule14
  }, {
    start: 67424,
    length: 8,
    convRule: rule14
  }, {
    start: 67584,
    length: 6,
    convRule: rule14
  }, {
    start: 67592,
    length: 1,
    convRule: rule14
  }, {
    start: 67594,
    length: 44,
    convRule: rule14
  }, {
    start: 67639,
    length: 2,
    convRule: rule14
  }, {
    start: 67644,
    length: 1,
    convRule: rule14
  }, {
    start: 67647,
    length: 23,
    convRule: rule14
  }, {
    start: 67671,
    length: 1,
    convRule: rule2
  }, {
    start: 67672,
    length: 8,
    convRule: rule17
  }, {
    start: 67680,
    length: 23,
    convRule: rule14
  }, {
    start: 67703,
    length: 2,
    convRule: rule13
  }, {
    start: 67705,
    length: 7,
    convRule: rule17
  }, {
    start: 67712,
    length: 31,
    convRule: rule14
  }, {
    start: 67751,
    length: 9,
    convRule: rule17
  }, {
    start: 67808,
    length: 19,
    convRule: rule14
  }, {
    start: 67828,
    length: 2,
    convRule: rule14
  }, {
    start: 67835,
    length: 5,
    convRule: rule17
  }, {
    start: 67840,
    length: 22,
    convRule: rule14
  }, {
    start: 67862,
    length: 6,
    convRule: rule17
  }, {
    start: 67871,
    length: 1,
    convRule: rule2
  }, {
    start: 67872,
    length: 26,
    convRule: rule14
  }, {
    start: 67903,
    length: 1,
    convRule: rule2
  }, {
    start: 67968,
    length: 56,
    convRule: rule14
  }, {
    start: 68028,
    length: 2,
    convRule: rule17
  }, {
    start: 68030,
    length: 2,
    convRule: rule14
  }, {
    start: 68032,
    length: 16,
    convRule: rule17
  }, {
    start: 68050,
    length: 46,
    convRule: rule17
  }, {
    start: 68096,
    length: 1,
    convRule: rule14
  }, {
    start: 68097,
    length: 3,
    convRule: rule92
  }, {
    start: 68101,
    length: 2,
    convRule: rule92
  }, {
    start: 68108,
    length: 4,
    convRule: rule92
  }, {
    start: 68112,
    length: 4,
    convRule: rule14
  }, {
    start: 68117,
    length: 3,
    convRule: rule14
  }, {
    start: 68121,
    length: 29,
    convRule: rule14
  }, {
    start: 68152,
    length: 3,
    convRule: rule92
  }, {
    start: 68159,
    length: 1,
    convRule: rule92
  }, {
    start: 68160,
    length: 9,
    convRule: rule17
  }, {
    start: 68176,
    length: 9,
    convRule: rule2
  }, {
    start: 68192,
    length: 29,
    convRule: rule14
  }, {
    start: 68221,
    length: 2,
    convRule: rule17
  }, {
    start: 68223,
    length: 1,
    convRule: rule2
  }, {
    start: 68224,
    length: 29,
    convRule: rule14
  }, {
    start: 68253,
    length: 3,
    convRule: rule17
  }, {
    start: 68288,
    length: 8,
    convRule: rule14
  }, {
    start: 68296,
    length: 1,
    convRule: rule13
  }, {
    start: 68297,
    length: 28,
    convRule: rule14
  }, {
    start: 68325,
    length: 2,
    convRule: rule92
  }, {
    start: 68331,
    length: 5,
    convRule: rule17
  }, {
    start: 68336,
    length: 7,
    convRule: rule2
  }, {
    start: 68352,
    length: 54,
    convRule: rule14
  }, {
    start: 68409,
    length: 7,
    convRule: rule2
  }, {
    start: 68416,
    length: 22,
    convRule: rule14
  }, {
    start: 68440,
    length: 8,
    convRule: rule17
  }, {
    start: 68448,
    length: 19,
    convRule: rule14
  }, {
    start: 68472,
    length: 8,
    convRule: rule17
  }, {
    start: 68480,
    length: 18,
    convRule: rule14
  }, {
    start: 68505,
    length: 4,
    convRule: rule2
  }, {
    start: 68521,
    length: 7,
    convRule: rule17
  }, {
    start: 68608,
    length: 73,
    convRule: rule14
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 68858,
    length: 6,
    convRule: rule17
  }, {
    start: 68864,
    length: 36,
    convRule: rule14
  }, {
    start: 68900,
    length: 4,
    convRule: rule92
  }, {
    start: 68912,
    length: 10,
    convRule: rule8
  }, {
    start: 69216,
    length: 31,
    convRule: rule17
  }, {
    start: 69248,
    length: 42,
    convRule: rule14
  }, {
    start: 69291,
    length: 2,
    convRule: rule92
  }, {
    start: 69293,
    length: 1,
    convRule: rule7
  }, {
    start: 69296,
    length: 2,
    convRule: rule14
  }, {
    start: 69376,
    length: 29,
    convRule: rule14
  }, {
    start: 69405,
    length: 10,
    convRule: rule17
  }, {
    start: 69415,
    length: 1,
    convRule: rule14
  }, {
    start: 69424,
    length: 22,
    convRule: rule14
  }, {
    start: 69446,
    length: 11,
    convRule: rule92
  }, {
    start: 69457,
    length: 4,
    convRule: rule17
  }, {
    start: 69461,
    length: 5,
    convRule: rule2
  }, {
    start: 69552,
    length: 21,
    convRule: rule14
  }, {
    start: 69573,
    length: 7,
    convRule: rule17
  }, {
    start: 69600,
    length: 23,
    convRule: rule14
  }, {
    start: 69632,
    length: 1,
    convRule: rule124
  }, {
    start: 69633,
    length: 1,
    convRule: rule92
  }, {
    start: 69634,
    length: 1,
    convRule: rule124
  }, {
    start: 69635,
    length: 53,
    convRule: rule14
  }, {
    start: 69688,
    length: 15,
    convRule: rule92
  }, {
    start: 69703,
    length: 7,
    convRule: rule2
  }, {
    start: 69714,
    length: 20,
    convRule: rule17
  }, {
    start: 69734,
    length: 10,
    convRule: rule8
  }, {
    start: 69759,
    length: 3,
    convRule: rule92
  }, {
    start: 69762,
    length: 1,
    convRule: rule124
  }, {
    start: 69763,
    length: 45,
    convRule: rule14
  }, {
    start: 69808,
    length: 3,
    convRule: rule124
  }, {
    start: 69811,
    length: 4,
    convRule: rule92
  }, {
    start: 69815,
    length: 2,
    convRule: rule124
  }, {
    start: 69817,
    length: 2,
    convRule: rule92
  }, {
    start: 69819,
    length: 2,
    convRule: rule2
  }, {
    start: 69821,
    length: 1,
    convRule: rule16
  }, {
    start: 69822,
    length: 4,
    convRule: rule2
  }, {
    start: 69837,
    length: 1,
    convRule: rule16
  }, {
    start: 69840,
    length: 25,
    convRule: rule14
  }, {
    start: 69872,
    length: 10,
    convRule: rule8
  }, {
    start: 69888,
    length: 3,
    convRule: rule92
  }, {
    start: 69891,
    length: 36,
    convRule: rule14
  }, {
    start: 69927,
    length: 5,
    convRule: rule92
  }, {
    start: 69932,
    length: 1,
    convRule: rule124
  }, {
    start: 69933,
    length: 8,
    convRule: rule92
  }, {
    start: 69942,
    length: 10,
    convRule: rule8
  }, {
    start: 69952,
    length: 4,
    convRule: rule2
  }, {
    start: 69956,
    length: 1,
    convRule: rule14
  }, {
    start: 69957,
    length: 2,
    convRule: rule124
  }, {
    start: 69959,
    length: 1,
    convRule: rule14
  }, {
    start: 69968,
    length: 35,
    convRule: rule14
  }, {
    start: 70003,
    length: 1,
    convRule: rule92
  }, {
    start: 70004,
    length: 2,
    convRule: rule2
  }, {
    start: 70006,
    length: 1,
    convRule: rule14
  }, {
    start: 70016,
    length: 2,
    convRule: rule92
  }, {
    start: 70018,
    length: 1,
    convRule: rule124
  }, {
    start: 70019,
    length: 48,
    convRule: rule14
  }, {
    start: 70067,
    length: 3,
    convRule: rule124
  }, {
    start: 70070,
    length: 9,
    convRule: rule92
  }, {
    start: 70079,
    length: 2,
    convRule: rule124
  }, {
    start: 70081,
    length: 4,
    convRule: rule14
  }, {
    start: 70085,
    length: 4,
    convRule: rule2
  }, {
    start: 70089,
    length: 4,
    convRule: rule92
  }, {
    start: 70093,
    length: 1,
    convRule: rule2
  }, {
    start: 70094,
    length: 1,
    convRule: rule124
  }, {
    start: 70095,
    length: 1,
    convRule: rule92
  }, {
    start: 70096,
    length: 10,
    convRule: rule8
  }, {
    start: 70106,
    length: 1,
    convRule: rule14
  }, {
    start: 70107,
    length: 1,
    convRule: rule2
  }, {
    start: 70108,
    length: 1,
    convRule: rule14
  }, {
    start: 70109,
    length: 3,
    convRule: rule2
  }, {
    start: 70113,
    length: 20,
    convRule: rule17
  }, {
    start: 70144,
    length: 18,
    convRule: rule14
  }, {
    start: 70163,
    length: 25,
    convRule: rule14
  }, {
    start: 70188,
    length: 3,
    convRule: rule124
  }, {
    start: 70191,
    length: 3,
    convRule: rule92
  }, {
    start: 70194,
    length: 2,
    convRule: rule124
  }, {
    start: 70196,
    length: 1,
    convRule: rule92
  }, {
    start: 70197,
    length: 1,
    convRule: rule124
  }, {
    start: 70198,
    length: 2,
    convRule: rule92
  }, {
    start: 70200,
    length: 6,
    convRule: rule2
  }, {
    start: 70206,
    length: 1,
    convRule: rule92
  }, {
    start: 70272,
    length: 7,
    convRule: rule14
  }, {
    start: 70280,
    length: 1,
    convRule: rule14
  }, {
    start: 70282,
    length: 4,
    convRule: rule14
  }, {
    start: 70287,
    length: 15,
    convRule: rule14
  }, {
    start: 70303,
    length: 10,
    convRule: rule14
  }, {
    start: 70313,
    length: 1,
    convRule: rule2
  }, {
    start: 70320,
    length: 47,
    convRule: rule14
  }, {
    start: 70367,
    length: 1,
    convRule: rule92
  }, {
    start: 70368,
    length: 3,
    convRule: rule124
  }, {
    start: 70371,
    length: 8,
    convRule: rule92
  }, {
    start: 70384,
    length: 10,
    convRule: rule8
  }, {
    start: 70400,
    length: 2,
    convRule: rule92
  }, {
    start: 70402,
    length: 2,
    convRule: rule124
  }, {
    start: 70405,
    length: 8,
    convRule: rule14
  }, {
    start: 70415,
    length: 2,
    convRule: rule14
  }, {
    start: 70419,
    length: 22,
    convRule: rule14
  }, {
    start: 70442,
    length: 7,
    convRule: rule14
  }, {
    start: 70450,
    length: 2,
    convRule: rule14
  }, {
    start: 70453,
    length: 5,
    convRule: rule14
  }, {
    start: 70459,
    length: 2,
    convRule: rule92
  }, {
    start: 70461,
    length: 1,
    convRule: rule14
  }, {
    start: 70462,
    length: 2,
    convRule: rule124
  }, {
    start: 70464,
    length: 1,
    convRule: rule92
  }, {
    start: 70465,
    length: 4,
    convRule: rule124
  }, {
    start: 70471,
    length: 2,
    convRule: rule124
  }, {
    start: 70475,
    length: 3,
    convRule: rule124
  }, {
    start: 70480,
    length: 1,
    convRule: rule14
  }, {
    start: 70487,
    length: 1,
    convRule: rule124
  }, {
    start: 70493,
    length: 5,
    convRule: rule14
  }, {
    start: 70498,
    length: 2,
    convRule: rule124
  }, {
    start: 70502,
    length: 7,
    convRule: rule92
  }, {
    start: 70512,
    length: 5,
    convRule: rule92
  }, {
    start: 70656,
    length: 53,
    convRule: rule14
  }, {
    start: 70709,
    length: 3,
    convRule: rule124
  }, {
    start: 70712,
    length: 8,
    convRule: rule92
  }, {
    start: 70720,
    length: 2,
    convRule: rule124
  }, {
    start: 70722,
    length: 3,
    convRule: rule92
  }, {
    start: 70725,
    length: 1,
    convRule: rule124
  }, {
    start: 70726,
    length: 1,
    convRule: rule92
  }, {
    start: 70727,
    length: 4,
    convRule: rule14
  }, {
    start: 70731,
    length: 5,
    convRule: rule2
  }, {
    start: 70736,
    length: 10,
    convRule: rule8
  }, {
    start: 70746,
    length: 2,
    convRule: rule2
  }, {
    start: 70749,
    length: 1,
    convRule: rule2
  }, {
    start: 70750,
    length: 1,
    convRule: rule92
  }, {
    start: 70751,
    length: 3,
    convRule: rule14
  }, {
    start: 70784,
    length: 48,
    convRule: rule14
  }, {
    start: 70832,
    length: 3,
    convRule: rule124
  }, {
    start: 70835,
    length: 6,
    convRule: rule92
  }, {
    start: 70841,
    length: 1,
    convRule: rule124
  }, {
    start: 70842,
    length: 1,
    convRule: rule92
  }, {
    start: 70843,
    length: 4,
    convRule: rule124
  }, {
    start: 70847,
    length: 2,
    convRule: rule92
  }, {
    start: 70849,
    length: 1,
    convRule: rule124
  }, {
    start: 70850,
    length: 2,
    convRule: rule92
  }, {
    start: 70852,
    length: 2,
    convRule: rule14
  }, {
    start: 70854,
    length: 1,
    convRule: rule2
  }, {
    start: 70855,
    length: 1,
    convRule: rule14
  }, {
    start: 70864,
    length: 10,
    convRule: rule8
  }, {
    start: 71040,
    length: 47,
    convRule: rule14
  }, {
    start: 71087,
    length: 3,
    convRule: rule124
  }, {
    start: 71090,
    length: 4,
    convRule: rule92
  }, {
    start: 71096,
    length: 4,
    convRule: rule124
  }, {
    start: 71100,
    length: 2,
    convRule: rule92
  }, {
    start: 71102,
    length: 1,
    convRule: rule124
  }, {
    start: 71103,
    length: 2,
    convRule: rule92
  }, {
    start: 71105,
    length: 23,
    convRule: rule2
  }, {
    start: 71128,
    length: 4,
    convRule: rule14
  }, {
    start: 71132,
    length: 2,
    convRule: rule92
  }, {
    start: 71168,
    length: 48,
    convRule: rule14
  }, {
    start: 71216,
    length: 3,
    convRule: rule124
  }, {
    start: 71219,
    length: 8,
    convRule: rule92
  }, {
    start: 71227,
    length: 2,
    convRule: rule124
  }, {
    start: 71229,
    length: 1,
    convRule: rule92
  }, {
    start: 71230,
    length: 1,
    convRule: rule124
  }, {
    start: 71231,
    length: 2,
    convRule: rule92
  }, {
    start: 71233,
    length: 3,
    convRule: rule2
  }, {
    start: 71236,
    length: 1,
    convRule: rule14
  }, {
    start: 71248,
    length: 10,
    convRule: rule8
  }, {
    start: 71264,
    length: 13,
    convRule: rule2
  }, {
    start: 71296,
    length: 43,
    convRule: rule14
  }, {
    start: 71339,
    length: 1,
    convRule: rule92
  }, {
    start: 71340,
    length: 1,
    convRule: rule124
  }, {
    start: 71341,
    length: 1,
    convRule: rule92
  }, {
    start: 71342,
    length: 2,
    convRule: rule124
  }, {
    start: 71344,
    length: 6,
    convRule: rule92
  }, {
    start: 71350,
    length: 1,
    convRule: rule124
  }, {
    start: 71351,
    length: 1,
    convRule: rule92
  }, {
    start: 71352,
    length: 1,
    convRule: rule14
  }, {
    start: 71360,
    length: 10,
    convRule: rule8
  }, {
    start: 71424,
    length: 27,
    convRule: rule14
  }, {
    start: 71453,
    length: 3,
    convRule: rule92
  }, {
    start: 71456,
    length: 2,
    convRule: rule124
  }, {
    start: 71458,
    length: 4,
    convRule: rule92
  }, {
    start: 71462,
    length: 1,
    convRule: rule124
  }, {
    start: 71463,
    length: 5,
    convRule: rule92
  }, {
    start: 71472,
    length: 10,
    convRule: rule8
  }, {
    start: 71482,
    length: 2,
    convRule: rule17
  }, {
    start: 71484,
    length: 3,
    convRule: rule2
  }, {
    start: 71487,
    length: 1,
    convRule: rule13
  }, {
    start: 71680,
    length: 44,
    convRule: rule14
  }, {
    start: 71724,
    length: 3,
    convRule: rule124
  }, {
    start: 71727,
    length: 9,
    convRule: rule92
  }, {
    start: 71736,
    length: 1,
    convRule: rule124
  }, {
    start: 71737,
    length: 2,
    convRule: rule92
  }, {
    start: 71739,
    length: 1,
    convRule: rule2
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 71904,
    length: 10,
    convRule: rule8
  }, {
    start: 71914,
    length: 9,
    convRule: rule17
  }, {
    start: 71935,
    length: 8,
    convRule: rule14
  }, {
    start: 71945,
    length: 1,
    convRule: rule14
  }, {
    start: 71948,
    length: 8,
    convRule: rule14
  }, {
    start: 71957,
    length: 2,
    convRule: rule14
  }, {
    start: 71960,
    length: 24,
    convRule: rule14
  }, {
    start: 71984,
    length: 6,
    convRule: rule124
  }, {
    start: 71991,
    length: 2,
    convRule: rule124
  }, {
    start: 71995,
    length: 2,
    convRule: rule92
  }, {
    start: 71997,
    length: 1,
    convRule: rule124
  }, {
    start: 71998,
    length: 1,
    convRule: rule92
  }, {
    start: 71999,
    length: 1,
    convRule: rule14
  }, {
    start: 72e3,
    length: 1,
    convRule: rule124
  }, {
    start: 72001,
    length: 1,
    convRule: rule14
  }, {
    start: 72002,
    length: 1,
    convRule: rule124
  }, {
    start: 72003,
    length: 1,
    convRule: rule92
  }, {
    start: 72004,
    length: 3,
    convRule: rule2
  }, {
    start: 72016,
    length: 10,
    convRule: rule8
  }, {
    start: 72096,
    length: 8,
    convRule: rule14
  }, {
    start: 72106,
    length: 39,
    convRule: rule14
  }, {
    start: 72145,
    length: 3,
    convRule: rule124
  }, {
    start: 72148,
    length: 4,
    convRule: rule92
  }, {
    start: 72154,
    length: 2,
    convRule: rule92
  }, {
    start: 72156,
    length: 4,
    convRule: rule124
  }, {
    start: 72160,
    length: 1,
    convRule: rule92
  }, {
    start: 72161,
    length: 1,
    convRule: rule14
  }, {
    start: 72162,
    length: 1,
    convRule: rule2
  }, {
    start: 72163,
    length: 1,
    convRule: rule14
  }, {
    start: 72164,
    length: 1,
    convRule: rule124
  }, {
    start: 72192,
    length: 1,
    convRule: rule14
  }, {
    start: 72193,
    length: 10,
    convRule: rule92
  }, {
    start: 72203,
    length: 40,
    convRule: rule14
  }, {
    start: 72243,
    length: 6,
    convRule: rule92
  }, {
    start: 72249,
    length: 1,
    convRule: rule124
  }, {
    start: 72250,
    length: 1,
    convRule: rule14
  }, {
    start: 72251,
    length: 4,
    convRule: rule92
  }, {
    start: 72255,
    length: 8,
    convRule: rule2
  }, {
    start: 72263,
    length: 1,
    convRule: rule92
  }, {
    start: 72272,
    length: 1,
    convRule: rule14
  }, {
    start: 72273,
    length: 6,
    convRule: rule92
  }, {
    start: 72279,
    length: 2,
    convRule: rule124
  }, {
    start: 72281,
    length: 3,
    convRule: rule92
  }, {
    start: 72284,
    length: 46,
    convRule: rule14
  }, {
    start: 72330,
    length: 13,
    convRule: rule92
  }, {
    start: 72343,
    length: 1,
    convRule: rule124
  }, {
    start: 72344,
    length: 2,
    convRule: rule92
  }, {
    start: 72346,
    length: 3,
    convRule: rule2
  }, {
    start: 72349,
    length: 1,
    convRule: rule14
  }, {
    start: 72350,
    length: 5,
    convRule: rule2
  }, {
    start: 72384,
    length: 57,
    convRule: rule14
  }, {
    start: 72704,
    length: 9,
    convRule: rule14
  }, {
    start: 72714,
    length: 37,
    convRule: rule14
  }, {
    start: 72751,
    length: 1,
    convRule: rule124
  }, {
    start: 72752,
    length: 7,
    convRule: rule92
  }, {
    start: 72760,
    length: 6,
    convRule: rule92
  }, {
    start: 72766,
    length: 1,
    convRule: rule124
  }, {
    start: 72767,
    length: 1,
    convRule: rule92
  }, {
    start: 72768,
    length: 1,
    convRule: rule14
  }, {
    start: 72769,
    length: 5,
    convRule: rule2
  }, {
    start: 72784,
    length: 10,
    convRule: rule8
  }, {
    start: 72794,
    length: 19,
    convRule: rule17
  }, {
    start: 72816,
    length: 2,
    convRule: rule2
  }, {
    start: 72818,
    length: 30,
    convRule: rule14
  }, {
    start: 72850,
    length: 22,
    convRule: rule92
  }, {
    start: 72873,
    length: 1,
    convRule: rule124
  }, {
    start: 72874,
    length: 7,
    convRule: rule92
  }, {
    start: 72881,
    length: 1,
    convRule: rule124
  }, {
    start: 72882,
    length: 2,
    convRule: rule92
  }, {
    start: 72884,
    length: 1,
    convRule: rule124
  }, {
    start: 72885,
    length: 2,
    convRule: rule92
  }, {
    start: 72960,
    length: 7,
    convRule: rule14
  }, {
    start: 72968,
    length: 2,
    convRule: rule14
  }, {
    start: 72971,
    length: 38,
    convRule: rule14
  }, {
    start: 73009,
    length: 6,
    convRule: rule92
  }, {
    start: 73018,
    length: 1,
    convRule: rule92
  }, {
    start: 73020,
    length: 2,
    convRule: rule92
  }, {
    start: 73023,
    length: 7,
    convRule: rule92
  }, {
    start: 73030,
    length: 1,
    convRule: rule14
  }, {
    start: 73031,
    length: 1,
    convRule: rule92
  }, {
    start: 73040,
    length: 10,
    convRule: rule8
  }, {
    start: 73056,
    length: 6,
    convRule: rule14
  }, {
    start: 73063,
    length: 2,
    convRule: rule14
  }, {
    start: 73066,
    length: 32,
    convRule: rule14
  }, {
    start: 73098,
    length: 5,
    convRule: rule124
  }, {
    start: 73104,
    length: 2,
    convRule: rule92
  }, {
    start: 73107,
    length: 2,
    convRule: rule124
  }, {
    start: 73109,
    length: 1,
    convRule: rule92
  }, {
    start: 73110,
    length: 1,
    convRule: rule124
  }, {
    start: 73111,
    length: 1,
    convRule: rule92
  }, {
    start: 73112,
    length: 1,
    convRule: rule14
  }, {
    start: 73120,
    length: 10,
    convRule: rule8
  }, {
    start: 73440,
    length: 19,
    convRule: rule14
  }, {
    start: 73459,
    length: 2,
    convRule: rule92
  }, {
    start: 73461,
    length: 2,
    convRule: rule124
  }, {
    start: 73463,
    length: 2,
    convRule: rule2
  }, {
    start: 73648,
    length: 1,
    convRule: rule14
  }, {
    start: 73664,
    length: 21,
    convRule: rule17
  }, {
    start: 73685,
    length: 8,
    convRule: rule13
  }, {
    start: 73693,
    length: 4,
    convRule: rule3
  }, {
    start: 73697,
    length: 17,
    convRule: rule13
  }, {
    start: 73727,
    length: 1,
    convRule: rule2
  }, {
    start: 73728,
    length: 922,
    convRule: rule14
  }, {
    start: 74752,
    length: 111,
    convRule: rule128
  }, {
    start: 74864,
    length: 5,
    convRule: rule2
  }, {
    start: 74880,
    length: 196,
    convRule: rule14
  }, {
    start: 77824,
    length: 1071,
    convRule: rule14
  }, {
    start: 78896,
    length: 9,
    convRule: rule16
  }, {
    start: 82944,
    length: 583,
    convRule: rule14
  }, {
    start: 92160,
    length: 569,
    convRule: rule14
  }, {
    start: 92736,
    length: 31,
    convRule: rule14
  }, {
    start: 92768,
    length: 10,
    convRule: rule8
  }, {
    start: 92782,
    length: 2,
    convRule: rule2
  }, {
    start: 92880,
    length: 30,
    convRule: rule14
  }, {
    start: 92912,
    length: 5,
    convRule: rule92
  }, {
    start: 92917,
    length: 1,
    convRule: rule2
  }, {
    start: 92928,
    length: 48,
    convRule: rule14
  }, {
    start: 92976,
    length: 7,
    convRule: rule92
  }, {
    start: 92983,
    length: 5,
    convRule: rule2
  }, {
    start: 92988,
    length: 4,
    convRule: rule13
  }, {
    start: 92992,
    length: 4,
    convRule: rule91
  }, {
    start: 92996,
    length: 1,
    convRule: rule2
  }, {
    start: 92997,
    length: 1,
    convRule: rule13
  }, {
    start: 93008,
    length: 10,
    convRule: rule8
  }, {
    start: 93019,
    length: 7,
    convRule: rule17
  }, {
    start: 93027,
    length: 21,
    convRule: rule14
  }, {
    start: 93053,
    length: 19,
    convRule: rule14
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 93824,
    length: 23,
    convRule: rule17
  }, {
    start: 93847,
    length: 4,
    convRule: rule2
  }, {
    start: 93952,
    length: 75,
    convRule: rule14
  }, {
    start: 94031,
    length: 1,
    convRule: rule92
  }, {
    start: 94032,
    length: 1,
    convRule: rule14
  }, {
    start: 94033,
    length: 55,
    convRule: rule124
  }, {
    start: 94095,
    length: 4,
    convRule: rule92
  }, {
    start: 94099,
    length: 13,
    convRule: rule91
  }, {
    start: 94176,
    length: 2,
    convRule: rule91
  }, {
    start: 94178,
    length: 1,
    convRule: rule2
  }, {
    start: 94179,
    length: 1,
    convRule: rule91
  }, {
    start: 94180,
    length: 1,
    convRule: rule92
  }, {
    start: 94192,
    length: 2,
    convRule: rule124
  }, {
    start: 94208,
    length: 6136,
    convRule: rule14
  }, {
    start: 100352,
    length: 1238,
    convRule: rule14
  }, {
    start: 101632,
    length: 9,
    convRule: rule14
  }, {
    start: 110592,
    length: 287,
    convRule: rule14
  }, {
    start: 110928,
    length: 3,
    convRule: rule14
  }, {
    start: 110948,
    length: 4,
    convRule: rule14
  }, {
    start: 110960,
    length: 396,
    convRule: rule14
  }, {
    start: 113664,
    length: 107,
    convRule: rule14
  }, {
    start: 113776,
    length: 13,
    convRule: rule14
  }, {
    start: 113792,
    length: 9,
    convRule: rule14
  }, {
    start: 113808,
    length: 10,
    convRule: rule14
  }, {
    start: 113820,
    length: 1,
    convRule: rule13
  }, {
    start: 113821,
    length: 2,
    convRule: rule92
  }, {
    start: 113823,
    length: 1,
    convRule: rule2
  }, {
    start: 113824,
    length: 4,
    convRule: rule16
  }, {
    start: 118784,
    length: 246,
    convRule: rule13
  }, {
    start: 119040,
    length: 39,
    convRule: rule13
  }, {
    start: 119081,
    length: 60,
    convRule: rule13
  }, {
    start: 119141,
    length: 2,
    convRule: rule124
  }, {
    start: 119143,
    length: 3,
    convRule: rule92
  }, {
    start: 119146,
    length: 3,
    convRule: rule13
  }, {
    start: 119149,
    length: 6,
    convRule: rule124
  }, {
    start: 119155,
    length: 8,
    convRule: rule16
  }, {
    start: 119163,
    length: 8,
    convRule: rule92
  }, {
    start: 119171,
    length: 2,
    convRule: rule13
  }, {
    start: 119173,
    length: 7,
    convRule: rule92
  }, {
    start: 119180,
    length: 30,
    convRule: rule13
  }, {
    start: 119210,
    length: 4,
    convRule: rule92
  }, {
    start: 119214,
    length: 59,
    convRule: rule13
  }, {
    start: 119296,
    length: 66,
    convRule: rule13
  }, {
    start: 119362,
    length: 3,
    convRule: rule92
  }, {
    start: 119365,
    length: 1,
    convRule: rule13
  }, {
    start: 119520,
    length: 20,
    convRule: rule17
  }, {
    start: 119552,
    length: 87,
    convRule: rule13
  }, {
    start: 119648,
    length: 25,
    convRule: rule17
  }, {
    start: 119808,
    length: 26,
    convRule: rule107
  }, {
    start: 119834,
    length: 26,
    convRule: rule20
  }, {
    start: 119860,
    length: 26,
    convRule: rule107
  }, {
    start: 119886,
    length: 7,
    convRule: rule20
  }, {
    start: 119894,
    length: 18,
    convRule: rule20
  }, {
    start: 119912,
    length: 26,
    convRule: rule107
  }, {
    start: 119938,
    length: 26,
    convRule: rule20
  }, {
    start: 119964,
    length: 1,
    convRule: rule107
  }, {
    start: 119966,
    length: 2,
    convRule: rule107
  }, {
    start: 119970,
    length: 1,
    convRule: rule107
  }, {
    start: 119973,
    length: 2,
    convRule: rule107
  }, {
    start: 119977,
    length: 4,
    convRule: rule107
  }, {
    start: 119982,
    length: 8,
    convRule: rule107
  }, {
    start: 119990,
    length: 4,
    convRule: rule20
  }, {
    start: 119995,
    length: 1,
    convRule: rule20
  }, {
    start: 119997,
    length: 7,
    convRule: rule20
  }, {
    start: 120005,
    length: 11,
    convRule: rule20
  }, {
    start: 120016,
    length: 26,
    convRule: rule107
  }, {
    start: 120042,
    length: 26,
    convRule: rule20
  }, {
    start: 120068,
    length: 2,
    convRule: rule107
  }, {
    start: 120071,
    length: 4,
    convRule: rule107
  }, {
    start: 120077,
    length: 8,
    convRule: rule107
  }, {
    start: 120086,
    length: 7,
    convRule: rule107
  }, {
    start: 120094,
    length: 26,
    convRule: rule20
  }, {
    start: 120120,
    length: 2,
    convRule: rule107
  }, {
    start: 120123,
    length: 4,
    convRule: rule107
  }, {
    start: 120128,
    length: 5,
    convRule: rule107
  }, {
    start: 120134,
    length: 1,
    convRule: rule107
  }, {
    start: 120138,
    length: 7,
    convRule: rule107
  }, {
    start: 120146,
    length: 26,
    convRule: rule20
  }, {
    start: 120172,
    length: 26,
    convRule: rule107
  }, {
    start: 120198,
    length: 26,
    convRule: rule20
  }, {
    start: 120224,
    length: 26,
    convRule: rule107
  }, {
    start: 120250,
    length: 26,
    convRule: rule20
  }, {
    start: 120276,
    length: 26,
    convRule: rule107
  }, {
    start: 120302,
    length: 26,
    convRule: rule20
  }, {
    start: 120328,
    length: 26,
    convRule: rule107
  }, {
    start: 120354,
    length: 26,
    convRule: rule20
  }, {
    start: 120380,
    length: 26,
    convRule: rule107
  }, {
    start: 120406,
    length: 26,
    convRule: rule20
  }, {
    start: 120432,
    length: 26,
    convRule: rule107
  }, {
    start: 120458,
    length: 28,
    convRule: rule20
  }, {
    start: 120488,
    length: 25,
    convRule: rule107
  }, {
    start: 120513,
    length: 1,
    convRule: rule6
  }, {
    start: 120514,
    length: 25,
    convRule: rule20
  }, {
    start: 120539,
    length: 1,
    convRule: rule6
  }, {
    start: 120540,
    length: 6,
    convRule: rule20
  }, {
    start: 120546,
    length: 25,
    convRule: rule107
  }, {
    start: 120571,
    length: 1,
    convRule: rule6
  }, {
    start: 120572,
    length: 25,
    convRule: rule20
  }, {
    start: 120597,
    length: 1,
    convRule: rule6
  }, {
    start: 120598,
    length: 6,
    convRule: rule20
  }, {
    start: 120604,
    length: 25,
    convRule: rule107
  }, {
    start: 120629,
    length: 1,
    convRule: rule6
  }, {
    start: 120630,
    length: 25,
    convRule: rule20
  }, {
    start: 120655,
    length: 1,
    convRule: rule6
  }, {
    start: 120656,
    length: 6,
    convRule: rule20
  }, {
    start: 120662,
    length: 25,
    convRule: rule107
  }, {
    start: 120687,
    length: 1,
    convRule: rule6
  }, {
    start: 120688,
    length: 25,
    convRule: rule20
  }, {
    start: 120713,
    length: 1,
    convRule: rule6
  }, {
    start: 120714,
    length: 6,
    convRule: rule20
  }, {
    start: 120720,
    length: 25,
    convRule: rule107
  }, {
    start: 120745,
    length: 1,
    convRule: rule6
  }, {
    start: 120746,
    length: 25,
    convRule: rule20
  }, {
    start: 120771,
    length: 1,
    convRule: rule6
  }, {
    start: 120772,
    length: 6,
    convRule: rule20
  }, {
    start: 120778,
    length: 1,
    convRule: rule107
  }, {
    start: 120779,
    length: 1,
    convRule: rule20
  }, {
    start: 120782,
    length: 50,
    convRule: rule8
  }, {
    start: 120832,
    length: 512,
    convRule: rule13
  }, {
    start: 121344,
    length: 55,
    convRule: rule92
  }, {
    start: 121399,
    length: 4,
    convRule: rule13
  }, {
    start: 121403,
    length: 50,
    convRule: rule92
  }, {
    start: 121453,
    length: 8,
    convRule: rule13
  }, {
    start: 121461,
    length: 1,
    convRule: rule92
  }, {
    start: 121462,
    length: 14,
    convRule: rule13
  }, {
    start: 121476,
    length: 1,
    convRule: rule92
  }, {
    start: 121477,
    length: 2,
    convRule: rule13
  }, {
    start: 121479,
    length: 5,
    convRule: rule2
  }, {
    start: 121499,
    length: 5,
    convRule: rule92
  }, {
    start: 121505,
    length: 15,
    convRule: rule92
  }, {
    start: 122880,
    length: 7,
    convRule: rule92
  }, {
    start: 122888,
    length: 17,
    convRule: rule92
  }, {
    start: 122907,
    length: 7,
    convRule: rule92
  }, {
    start: 122915,
    length: 2,
    convRule: rule92
  }, {
    start: 122918,
    length: 5,
    convRule: rule92
  }, {
    start: 123136,
    length: 45,
    convRule: rule14
  }, {
    start: 123184,
    length: 7,
    convRule: rule92
  }, {
    start: 123191,
    length: 7,
    convRule: rule91
  }, {
    start: 123200,
    length: 10,
    convRule: rule8
  }, {
    start: 123214,
    length: 1,
    convRule: rule14
  }, {
    start: 123215,
    length: 1,
    convRule: rule13
  }, {
    start: 123584,
    length: 44,
    convRule: rule14
  }, {
    start: 123628,
    length: 4,
    convRule: rule92
  }, {
    start: 123632,
    length: 10,
    convRule: rule8
  }, {
    start: 123647,
    length: 1,
    convRule: rule3
  }, {
    start: 124928,
    length: 197,
    convRule: rule14
  }, {
    start: 125127,
    length: 9,
    convRule: rule17
  }, {
    start: 125136,
    length: 7,
    convRule: rule92
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }, {
    start: 125252,
    length: 7,
    convRule: rule92
  }, {
    start: 125259,
    length: 1,
    convRule: rule91
  }, {
    start: 125264,
    length: 10,
    convRule: rule8
  }, {
    start: 125278,
    length: 2,
    convRule: rule2
  }, {
    start: 126065,
    length: 59,
    convRule: rule17
  }, {
    start: 126124,
    length: 1,
    convRule: rule13
  }, {
    start: 126125,
    length: 3,
    convRule: rule17
  }, {
    start: 126128,
    length: 1,
    convRule: rule3
  }, {
    start: 126129,
    length: 4,
    convRule: rule17
  }, {
    start: 126209,
    length: 45,
    convRule: rule17
  }, {
    start: 126254,
    length: 1,
    convRule: rule13
  }, {
    start: 126255,
    length: 15,
    convRule: rule17
  }, {
    start: 126464,
    length: 4,
    convRule: rule14
  }, {
    start: 126469,
    length: 27,
    convRule: rule14
  }, {
    start: 126497,
    length: 2,
    convRule: rule14
  }, {
    start: 126500,
    length: 1,
    convRule: rule14
  }, {
    start: 126503,
    length: 1,
    convRule: rule14
  }, {
    start: 126505,
    length: 10,
    convRule: rule14
  }, {
    start: 126516,
    length: 4,
    convRule: rule14
  }, {
    start: 126521,
    length: 1,
    convRule: rule14
  }, {
    start: 126523,
    length: 1,
    convRule: rule14
  }, {
    start: 126530,
    length: 1,
    convRule: rule14
  }, {
    start: 126535,
    length: 1,
    convRule: rule14
  }, {
    start: 126537,
    length: 1,
    convRule: rule14
  }, {
    start: 126539,
    length: 1,
    convRule: rule14
  }, {
    start: 126541,
    length: 3,
    convRule: rule14
  }, {
    start: 126545,
    length: 2,
    convRule: rule14
  }, {
    start: 126548,
    length: 1,
    convRule: rule14
  }, {
    start: 126551,
    length: 1,
    convRule: rule14
  }, {
    start: 126553,
    length: 1,
    convRule: rule14
  }, {
    start: 126555,
    length: 1,
    convRule: rule14
  }, {
    start: 126557,
    length: 1,
    convRule: rule14
  }, {
    start: 126559,
    length: 1,
    convRule: rule14
  }, {
    start: 126561,
    length: 2,
    convRule: rule14
  }, {
    start: 126564,
    length: 1,
    convRule: rule14
  }, {
    start: 126567,
    length: 4,
    convRule: rule14
  }, {
    start: 126572,
    length: 7,
    convRule: rule14
  }, {
    start: 126580,
    length: 4,
    convRule: rule14
  }, {
    start: 126585,
    length: 4,
    convRule: rule14
  }, {
    start: 126590,
    length: 1,
    convRule: rule14
  }, {
    start: 126592,
    length: 10,
    convRule: rule14
  }, {
    start: 126603,
    length: 17,
    convRule: rule14
  }, {
    start: 126625,
    length: 3,
    convRule: rule14
  }, {
    start: 126629,
    length: 5,
    convRule: rule14
  }, {
    start: 126635,
    length: 17,
    convRule: rule14
  }, {
    start: 126704,
    length: 2,
    convRule: rule6
  }, {
    start: 126976,
    length: 44,
    convRule: rule13
  }, {
    start: 127024,
    length: 100,
    convRule: rule13
  }, {
    start: 127136,
    length: 15,
    convRule: rule13
  }, {
    start: 127153,
    length: 15,
    convRule: rule13
  }, {
    start: 127169,
    length: 15,
    convRule: rule13
  }, {
    start: 127185,
    length: 37,
    convRule: rule13
  }, {
    start: 127232,
    length: 13,
    convRule: rule17
  }, {
    start: 127245,
    length: 161,
    convRule: rule13
  }, {
    start: 127462,
    length: 29,
    convRule: rule13
  }, {
    start: 127504,
    length: 44,
    convRule: rule13
  }, {
    start: 127552,
    length: 9,
    convRule: rule13
  }, {
    start: 127568,
    length: 2,
    convRule: rule13
  }, {
    start: 127584,
    length: 6,
    convRule: rule13
  }, {
    start: 127744,
    length: 251,
    convRule: rule13
  }, {
    start: 127995,
    length: 5,
    convRule: rule10
  }, {
    start: 128e3,
    length: 728,
    convRule: rule13
  }, {
    start: 128736,
    length: 13,
    convRule: rule13
  }, {
    start: 128752,
    length: 13,
    convRule: rule13
  }, {
    start: 128768,
    length: 116,
    convRule: rule13
  }, {
    start: 128896,
    length: 89,
    convRule: rule13
  }, {
    start: 128992,
    length: 12,
    convRule: rule13
  }, {
    start: 129024,
    length: 12,
    convRule: rule13
  }, {
    start: 129040,
    length: 56,
    convRule: rule13
  }, {
    start: 129104,
    length: 10,
    convRule: rule13
  }, {
    start: 129120,
    length: 40,
    convRule: rule13
  }, {
    start: 129168,
    length: 30,
    convRule: rule13
  }, {
    start: 129200,
    length: 2,
    convRule: rule13
  }, {
    start: 129280,
    length: 121,
    convRule: rule13
  }, {
    start: 129402,
    length: 82,
    convRule: rule13
  }, {
    start: 129485,
    length: 135,
    convRule: rule13
  }, {
    start: 129632,
    length: 14,
    convRule: rule13
  }, {
    start: 129648,
    length: 5,
    convRule: rule13
  }, {
    start: 129656,
    length: 3,
    convRule: rule13
  }, {
    start: 129664,
    length: 7,
    convRule: rule13
  }, {
    start: 129680,
    length: 25,
    convRule: rule13
  }, {
    start: 129712,
    length: 7,
    convRule: rule13
  }, {
    start: 129728,
    length: 3,
    convRule: rule13
  }, {
    start: 129744,
    length: 7,
    convRule: rule13
  }, {
    start: 129792,
    length: 147,
    convRule: rule13
  }, {
    start: 129940,
    length: 55,
    convRule: rule13
  }, {
    start: 130032,
    length: 10,
    convRule: rule8
  }, {
    start: 131072,
    length: 42718,
    convRule: rule14
  }, {
    start: 173824,
    length: 4149,
    convRule: rule14
  }, {
    start: 177984,
    length: 222,
    convRule: rule14
  }, {
    start: 178208,
    length: 5762,
    convRule: rule14
  }, {
    start: 183984,
    length: 7473,
    convRule: rule14
  }, {
    start: 194560,
    length: 542,
    convRule: rule14
  }, {
    start: 196608,
    length: 4939,
    convRule: rule14
  }, {
    start: 917505,
    length: 1,
    convRule: rule16
  }, {
    start: 917536,
    length: 96,
    convRule: rule16
  }, {
    start: 917760,
    length: 240,
    convRule: rule92
  }, {
    start: 983040,
    length: 65534,
    convRule: rule200
  }, {
    start: 1048576,
    length: 65534,
    convRule: rule200
  }];
  var checkAttr = function(categories) {
    return function($$char2) {
      var numOfBlocks = function() {
        var $43 = $$char2 < 256;
        if ($43) {
          return numLat1Blocks;
        }
        ;
        return numBlocks;
      }();
      var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
  var uIswalpha = /* @__PURE__ */ checkAttr([gencatLL, gencatLU, gencatLT, gencatLM, gencatLO]);
  var uIswupper = /* @__PURE__ */ checkAttr([gencatLU, gencatLT]);

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str2) {
      return str2.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index3) {
            return function(str2) {
              var length9 = str2.length;
              if (index3 < 0 || index3 >= length9)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str2[Symbol.iterator]();
                for (var i = index3; ; --i) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index3)(str2);
            };
          };
        };
      };
    };
  };
  var _fromCodePointArray = function(singleton11) {
    return hasFromCodePoint ? function(cps) {
      if (cps.length < 1e4) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton11).join("");
    } : function(cps) {
      return cps.map(singleton11).join("");
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str2) {
          return Array.from(str2, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a) {
    return a.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton5 = function(c) {
    return c;
  };
  var _charAt = function(just) {
    return function(nothing) {
      return function(i) {
        return function(s) {
          return i >= 0 && i < s.length ? just(s.charAt(i)) : nothing;
        };
      };
    };
  };
  var _toChar = function(just) {
    return function(nothing) {
      return function(s) {
        return s.length === 1 ? just(s) : nothing;
      };
    };
  };
  var length4 = function(s) {
    return s.length;
  };
  var _indexOf = function(just) {
    return function(nothing) {
      return function(x2) {
        return function(s) {
          var i = s.indexOf(x2);
          return i === -1 ? nothing : just(i);
        };
      };
    };
  };
  var drop = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i) {
    return function(s) {
      return { before: s.substring(0, i), after: s.substring(i) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var uncons3 = function(v) {
    if (v === "") {
      return Nothing.value;
    }
    ;
    return new Just({
      head: charAt(0)(v),
      tail: drop(1)(v)
    });
  };
  var toChar = /* @__PURE__ */ function() {
    return _toChar(Just.create)(Nothing.value);
  }();
  var stripPrefix = function(v) {
    return function(str2) {
      var v1 = splitAt(length4(v))(str2);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };
  var indexOf = /* @__PURE__ */ function() {
    return _indexOf(Just.create)(Nothing.value);
  }();
  var contains = function(pat) {
    var $23 = indexOf(pat);
    return function($24) {
      return isJust($23($24));
    };
  };
  var charAt2 = /* @__PURE__ */ function() {
    return _charAt(Just.create)(Nothing.value);
  }();

  // output/Data.String.Common/foreign.js
  var split = function(sep) {
    return function(s) {
      return s.split(sep);
    };
  };
  var toLower = function(s) {
    return s.toLowerCase();
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy4 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map10 = /* @__PURE__ */ map3(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x2) {
    return x2;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons4 = function(s) {
    var v = length4(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $42 = isLead(cu0) && isTrail(cu1);
    if ($42) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map10(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons4(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $46 = isLead(cu0) && length4(s) > 1;
    if ($46) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $47 = isTrail(cu1);
      if ($47) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length5 = function($73) {
    return length(toCodePointArray($73));
  };
  var fromCharCode3 = /* @__PURE__ */ function() {
    var $74 = toEnumWithDefaults(boundedEnumChar)(bottom2(boundedChar))(top2(boundedChar));
    return function($75) {
      return singleton5($74($75));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode3(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode3(lead) + fromCharCode3(trail);
  };
  var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
  var eqCodePoint = {
    eq: function(x2) {
      return function(y2) {
        return x2 === y2;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x2) {
      return function(y2) {
        return compare2(x2)(y2);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var codePointFromChar = function($76) {
    return CodePoint(fromEnum2($76));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons4(s);
        if (v instanceof Just) {
          var $65 = n === 0;
          if ($65) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy4("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var modify3 = unsafeCoerce2;
  var toLowerSimple = /* @__PURE__ */ modify3(uTowlower);
  var toUpperSimple = /* @__PURE__ */ modify3(uTowupper);
  var isUpper = function($66) {
    return uIswupper(fromEnum3($66));
  };
  var isSpace = function(c) {
    var uc = fromEnum3(c);
    var $28 = uc <= 823;
    if ($28) {
      return uc === 32 || (uc >= 9 && uc <= 13 || uc === 160);
    }
    ;
    return uIswspace(uc);
  };
  var isOctDigit = function(c) {
    var diff = fromEnum3(c) - toCharCode2("0") | 0;
    return diff <= 7 && diff >= 0;
  };
  var isDecDigit = function(c) {
    var diff = fromEnum3(c) - toCharCode2("0") | 0;
    return diff <= 9 && diff >= 0;
  };
  var isHexDigit = function(c) {
    return isDecDigit(c) || (function() {
      var diff = fromEnum3(c) - toCharCode2("A") | 0;
      return diff <= 5 && diff >= 0;
    }() || function() {
      var diff = fromEnum3(c) - toCharCode2("a") | 0;
      return diff <= 5 && diff >= 0;
    }());
  };
  var isAlphaNum = function($70) {
    return uIswalnum(fromEnum3($70));
  };
  var isAlpha = function($71) {
    return uIswalpha(fromEnum3($71));
  };
  var hexDigitToInt = function(c) {
    var hexUpper = fromEnum3(c) - toCharCode2("A") | 0;
    var hexLower = fromEnum3(c) - toCharCode2("a") | 0;
    var dec = fromEnum3(c) - toCharCode2("0") | 0;
    var result = function() {
      if (dec <= 9 && dec >= 0) {
        return new Just(dec);
      }
      ;
      if (hexLower <= 5 && hexLower >= 0) {
        return new Just(hexLower + 10 | 0);
      }
      ;
      if (hexUpper <= 5 && hexUpper >= 0) {
        return new Just(hexUpper + 10 | 0);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode (line 591, column 3 - line 591, column 22): " + []);
    }();
    return result;
  };

  // output/Data.Set/index.js
  var foldl4 = /* @__PURE__ */ foldl(foldableList);
  var fromFoldable1 = /* @__PURE__ */ fromFoldable(foldableList);
  var unsafeIndex3 = /* @__PURE__ */ unsafeIndex();
  var bind4 = /* @__PURE__ */ bind(bindST);
  var pure3 = /* @__PURE__ */ pure(applicativeST);
  var tailRecM22 = /* @__PURE__ */ tailRecM2(monadRecST);
  var $$Set = function(x2) {
    return x2;
  };
  var union2 = function(dictOrd) {
    var union13 = union(dictOrd);
    return function(v) {
      return function(v1) {
        return union13(v)(v1);
      };
    };
  };
  var toList2 = function(v) {
    return keys(v);
  };
  var toUnfoldable3 = function(dictUnfoldable) {
    var $127 = toUnfoldable(dictUnfoldable);
    return function($128) {
      return $127(toList2($128));
    };
  };
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var singleton6 = function(a) {
    return singleton4(a)(unit2);
  };
  var showSet = function(dictShow) {
    var show11 = show(showArray(dictShow));
    return {
      show: function(s) {
        return "(fromFoldable " + (show11(toUnfoldable12(s)) + ")");
      }
    };
  };
  var member2 = function(dictOrd) {
    var member1 = member(dictOrd);
    return function(a) {
      return function(v) {
        return member1(a)(v);
      };
    };
  };
  var isEmpty2 = function(v) {
    return isEmpty(v);
  };
  var insert2 = function(dictOrd) {
    var insert1 = insert(dictOrd);
    return function(a) {
      return function(v) {
        return insert1(a)(unit2)(v);
      };
    };
  };
  var fromMap = $$Set;
  var eqSet = function(dictEq) {
    var eq17 = eq(eqMap(dictEq)(eqUnit));
    return {
      eq: function(v) {
        return function(v1) {
          return eq17(v)(v1);
        };
      }
    };
  };
  var empty5 = empty4;
  var fromFoldable4 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert1 = insert2(dictOrd);
      return foldl22(function(m) {
        return function(a) {
          return insert1(a)(m);
        };
      })(empty5);
    };
  };
  var fromFoldable22 = /* @__PURE__ */ fromFoldable4(foldableArray);
  var intersection = function(dictOrd) {
    var compare6 = compare(dictOrd);
    var fromFoldable33 = fromFoldable22(dictOrd);
    return function(s1) {
      return function(s2) {
        var toArray2 = function($135) {
          return fromFoldable1(toList2($135));
        };
        var rs = toArray2(s2);
        var rl = length(rs);
        var ls = toArray2(s1);
        var ll = length(ls);
        var intersect2 = function(acc) {
          var go = function(l) {
            return function(r) {
              var $122 = l < ll && r < rl;
              if ($122) {
                var v = compare6(unsafeIndex3(ls)(l))(unsafeIndex3(rs)(r));
                if (v instanceof EQ) {
                  return function __do2() {
                    push(unsafeIndex3(ls)(l))(acc)();
                    return new Loop({
                      a: l + 1 | 0,
                      b: r + 1 | 0
                    });
                  };
                }
                ;
                if (v instanceof LT) {
                  return pure3(new Loop({
                    a: l + 1 | 0,
                    b: r
                  }));
                }
                ;
                if (v instanceof GT) {
                  return pure3(new Loop({
                    a: l,
                    b: r + 1 | 0
                  }));
                }
                ;
                throw new Error("Failed pattern match at Data.Set (line 184, column 12 - line 189, column 43): " + [v.constructor.name]);
              }
              ;
              return pure3(new Done(acc));
            };
          };
          return tailRecM22(go)(0)(0);
        };
        return fromFoldable33(bind4(bind4(newSTArray)(intersect2))(unsafeFreeze)());
      };
    };
  };
  var unions = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      return foldl22(union2(dictOrd))(empty5);
    };
  };
  var $$delete3 = function(dictOrd) {
    var delete1 = $$delete2(dictOrd);
    return function(a) {
      return function(v) {
        return delete1(a)(v);
      };
    };
  };
  var difference2 = function(dictOrd) {
    var delete1 = $$delete3(dictOrd);
    return function(s1) {
      return function(s2) {
        return foldl4(flip(delete1))(s1)(toList2(s2));
      };
    };
  };
  var subset = function(dictOrd) {
    var difference1 = difference2(dictOrd);
    return function(s1) {
      return function(s2) {
        return isEmpty2(difference1(s1)(s2));
      };
    };
  };

  // output/Data.Map/index.js
  var keys2 = /* @__PURE__ */ function() {
    var $38 = $$void(functorMap);
    return function($39) {
      return fromMap($38($39));
    };
  }();

  // output/Effect.Exception/foreign.js
  function showErrorImpl(err) {
    return err.stack || err.toString();
  }
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };
  var showError = {
    show: showErrorImpl
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Util/index.js
  var bimap4 = /* @__PURE__ */ bimap2(bifunctorEither);
  var identity13 = /* @__PURE__ */ identity4(categoryFn);
  var intercalate3 = /* @__PURE__ */ intercalate2(foldableList)(monoidList);
  var pure4 = /* @__PURE__ */ pure(applicativeList);
  var map1 = /* @__PURE__ */ map3(functorList);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean));
  var pure1 = /* @__PURE__ */ pure(applicativeEither);
  var toUnfoldable4 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var $$with = function(msg) {
    return bimap4(function(msg$prime) {
      return msg$prime + function() {
        var $75 = msg === "";
        if ($75) {
          return "";
        }
        ;
        return "\n" + msg;
      }();
    })(identity13);
  };
  var whenever = function(v) {
    if (!v) {
      return $$const(Nothing.value);
    }
    ;
    if (v) {
      return Just.create;
    }
    ;
    throw new Error("Failed pattern match at Util (line 38, column 1 - line 38, column 47): " + [v.constructor.name]);
  };
  var report = /* @__PURE__ */ function() {
    return Left.create;
  }();
  var orElse = note;
  var onlyIf = function(v) {
    return function(dictMonadPlus) {
      var Alternative1 = dictMonadPlus.Alternative1();
      if (v) {
        return pure(Alternative1.Applicative0());
      }
      ;
      if (!v) {
        return $$const(empty3(Alternative1.Plus1()));
      }
      ;
      throw new Error("Failed pattern match at Util (line 61, column 1 - line 61, column 58): " + [v.constructor.name]);
    };
  };
  var mayEq = function(dictEq) {
    var eq22 = eq(dictEq);
    return function(x2) {
      return function(x$prime) {
        return whenever(eq22(x2)(x$prime))(x2);
      };
    };
  };
  var mayFailEq = function(dictShow) {
    var show11 = show(dictShow);
    return function(dictEq) {
      var mayEq1 = mayEq(dictEq);
      return function(x2) {
        return function(x$prime) {
          return orElse(show11(x2) + (" \u2260 " + show11(x$prime)))(mayEq1(x2)(x$prime));
        };
      };
    };
  };
  var intersperse = function(x2) {
    return function(xs) {
      return intercalate3(pure4(x2))(map1(pure4)(xs));
    };
  };
  var error2 = function(msg) {
    return unsafePerformEffect($$throw(msg));
  };
  var successful = function(v) {
    if (v instanceof Left) {
      return error2(v.value0);
    }
    ;
    if (v instanceof Right) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Util (line 77, column 1 - line 77, column 40): " + [v.constructor.name]);
  };
  var successfulWith = function(msg) {
    var $97 = $$with(msg);
    return function($98) {
      return successful($97($98));
    };
  };
  var disjUnion_inv = function(dictOrd) {
    var filterKeys3 = filterKeys(dictOrd);
    var member4 = member2(dictOrd);
    return function(ks) {
      return function(m) {
        return new Tuple(filterKeys3(function(v) {
          return member4(v)(ks);
        })(m), filterKeys3(function(v) {
          return not2(member4(v))(ks);
        })(m));
      };
    };
  };
  var disjUnion = function(dictOrd) {
    return unionWith(dictOrd)(function(v) {
      return function(v1) {
        return error2("not disjoint");
      };
    });
  };
  var definitely = function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return v1.value0;
      }
      ;
      if (v1 instanceof Nothing) {
        return error2(v);
      }
      ;
      throw new Error("Failed pattern match at Util (line 42, column 1 - line 42, column 48): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var unsafeIndex4 = function(xs) {
    return function(i) {
      return definitely("index within bounds")(index(xs)(i));
    };
  };
  var unsafeUpdateAt = function(i) {
    return function(x2) {
      var $99 = definitely("index within bounds");
      var $100 = updateAt(i)(x2);
      return function($101) {
        return $99($100($101));
      };
    };
  };
  var check = function(v) {
    return function(v1) {
      if (v) {
        return pure1(unit2);
      }
      ;
      if (!v) {
        return report(v1);
      }
      ;
      throw new Error("Failed pattern match at Util (line 88, column 1 - line 88, column 43): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var bind2Flipped = function(dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var join12 = join(Bind1);
    var lift21 = lift2(Bind1.Apply0());
    return function(f) {
      return function(x2) {
        return function(y2) {
          return join12(lift21(f)(x2)(y2));
        };
      };
    };
  };
  var assert = function(v) {
    if (v) {
      return identity13;
    }
    ;
    if (!v) {
      return function(v1) {
        return error2("Assertion failure");
      };
    }
    ;
    throw new Error("Failed pattern match at Util (line 28, column 1 - line 28, column 34): " + [v.constructor.name]);
  };
  var asSingletonMap = function(m) {
    return assert(size(m) === 1)(definitely("singleton map")(head(toUnfoldable4(m))));
  };
  var absurd2 = "absurd";
  var definitely$prime = /* @__PURE__ */ definitely(absurd2);
  var mustLookup = function(dictOrd) {
    var lookup6 = lookup(dictOrd);
    return function(k) {
      var $102 = lookup6(k);
      return function($103) {
        return definitely$prime($102($103));
      };
    };
  };
  var nonEmpty = function(v) {
    if (v instanceof Nil) {
      return error2(absurd2);
    }
    ;
    if (v instanceof Cons) {
      return new NonEmpty(v.value0, v.value1);
    }
    ;
    throw new Error("Failed pattern match at Util (line 135, column 1 - line 135, column 48): " + [v.constructor.name]);
  };

  // output/DataType/index.js
  var eq4 = /* @__PURE__ */ eq(eqString);
  var compare3 = /* @__PURE__ */ compare(ordString);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var map11 = /* @__PURE__ */ map3(functorArray);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorList);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var $$void3 = /* @__PURE__ */ $$void(functorEither);
  var bind5 = /* @__PURE__ */ bind(bindEither);
  var bind2Flipped2 = /* @__PURE__ */ bind2Flipped(monadEither);
  var mayFailEq2 = /* @__PURE__ */ mayFailEq(showInt)(eqInt);
  var pure5 = /* @__PURE__ */ pure(applicativeEither);
  var DataType = /* @__PURE__ */ function() {
    function DataType2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DataType2.create = function(value0) {
      return function(value1) {
        return new DataType2(value0, value1);
      };
    };
    return DataType2;
  }();
  var eqCtr = {
    eq: function(x2) {
      return function(y2) {
        return x2 === y2;
      };
    }
  };
  var ordCtr = {
    compare: function(x2) {
      return function(y2) {
        return compare3(x2)(y2);
      };
    },
    Eq0: function() {
      return eqCtr;
    }
  };
  var fromFoldable5 = /* @__PURE__ */ fromFoldable3(ordCtr);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable5(foldableArray);
  var lookup2 = /* @__PURE__ */ lookup(ordCtr);
  var typeName = function(v) {
    return v.value0;
  };
  var eqDataType$primeInt = {
    eq: /* @__PURE__ */ on(eq4)(typeName)
  };
  var showDataType$primeInt = {
    show: typeName
  };
  var show2 = /* @__PURE__ */ show(showDataType$primeInt);
  var mayFailEq1 = /* @__PURE__ */ mayFailEq(showDataType$primeInt)(eqDataType$primeInt);
  var isCtrOp = function(str2) {
    return ":" === definitely$prime(charAt2(0)(str2));
  };
  var isCtrName = function(str2) {
    return isUpper(codePointFromChar(definitely$prime(charAt2(0)(str2))));
  };
  var showCtr = {
    show: function(c) {
      var show$prime = function(str2) {
        if (isCtrName(str2)) {
          return str2;
        }
        ;
        if (isCtrOp(str2)) {
          return "(" + (str2 + ")");
        }
        ;
        if (otherwise) {
          return error2(absurd2);
        }
        ;
        throw new Error("Failed pattern match at DataType (line 39, column 7 - line 41, column 48): " + [str2.constructor.name]);
      };
      return show$prime(unwrap4(c));
    }
  };
  var show1 = /* @__PURE__ */ show(showCtr);
  var f_y = "y";
  var f_x = "x";
  var f_plots = "plots";
  var f_name = "name";
  var f_data = "data";
  var f_caption = "caption";
  var dataTypeFor = function(dict) {
    return dict.dataTypeFor;
  };
  var dataType = function(name3) {
    var $67 = DataType.create(name3);
    var $68 = map11(uncurry(Tuple.create));
    return function($69) {
      return $67(fromFoldable12($68($69)));
    };
  };
  var ctrs = function(v) {
    return keys2(v.value1);
  };
  var cTrue = "True";
  var cSome = "Some";
  var cPair = "Pair";
  var cNone = "None";
  var cNil = "Nil";
  var cLinePlot = "LinePlot";
  var cLineChart = "LineChart";
  var cFalse = "False";
  var cCons = ":";
  var cBarChart = "BarChart";
  var dataTypes = /* @__PURE__ */ function() {
    return fromFoldable2(foldableArray)([dataType("Bool")([new Tuple(cTrue, 0), new Tuple(cFalse, 0)]), dataType("List")([new Tuple(cNil, 0), new Tuple(cCons, 2)]), dataType("Option")([new Tuple(cNone, 0), new Tuple(cSome, 1)]), dataType("Ordering")([new Tuple("GT", 0), new Tuple("LT", 0), new Tuple("EQ", 0)]), dataType("Pair")([new Tuple("Pair", 2)]), dataType("Tree")([new Tuple("Empty", 0), new Tuple("NonEmpty", 3)]), dataType("Point")([new Tuple("Point", 2)]), dataType("Orient")([new Tuple("Horiz", 0), new Tuple("Vert", 0)]), dataType("Plot")([new Tuple(cBarChart, 1), new Tuple(cLineChart, 1), new Tuple(cLinePlot, 1)]), dataType("GraphicsElement")([new Tuple("Circle", 4), new Tuple("Group", 1), new Tuple("Line", 4), new Tuple("Polyline", 3), new Tuple("Polymarkers", 2), new Tuple("Rect", 5), new Tuple("Text", 5), new Tuple("Viewport", 9)]), dataType("Transform")([new Tuple("Scale", 2), new Tuple("Translate", 2)]), dataType("Marker")([new Tuple("Arrowhead", 0)])]);
  }();
  var ctrToDataType = /* @__PURE__ */ fromFoldable5(foldableList)(/* @__PURE__ */ concat2(/* @__PURE__ */ mapFlipped2(dataTypes)(function(d) {
    return mapFlipped2(toUnfoldable5(ctrs(d)))(function(v) {
      return new Tuple(v, d);
    });
  })));
  var dataTypeForCtr = {
    dataTypeFor: function(c) {
      return note("Unknown constructor " + show1(c))(lookup2(c)(ctrToDataType));
    }
  };
  var dataTypeFor1 = /* @__PURE__ */ dataTypeFor(dataTypeForCtr);
  var dataTypeForSetCtr = {
    dataTypeFor: function(cs) {
      var v = toUnfoldable5(cs);
      if (v instanceof Nil) {
        return error2(absurd2);
      }
      ;
      if (v instanceof Cons) {
        return dataTypeFor1(v.value0);
      }
      ;
      throw new Error("Failed pattern match at DataType (line 76, column 21 - line 78, column 29): " + [v.constructor.name]);
    }
  };
  var dataTypeFor2 = /* @__PURE__ */ dataTypeFor(dataTypeForSetCtr);
  var keyCtr = {
    checkConsistent: function(msg) {
      return function(c) {
        return function(cs) {
          return $$void3(bind5(dataTypeFor1(c))(function(d) {
            return bind5(dataTypeFor2(cs))(function(d$prime) {
              return $$with(msg + (show1(c) + (" is not a constructor of " + show2(d$prime))))(mayFailEq1(d)(d$prime));
            });
          }));
        };
      };
    },
    Ord0: function() {
      return ordCtr;
    }
  };
  var arity = function(c) {
    return bind5(dataTypeFor1(c))(function(v) {
      return note(absurd2)(lookup2(c)(v.value1));
    });
  };
  var checkArity = function(c) {
    return function(n) {
      return $$void3($$with("Checking arity of " + show1(c))(bind2Flipped2(mayFailEq2)(arity(c))(pure5(n))));
    };
  };

  // output/Data.List.NonEmpty/index.js
  var wrappedOperation = function(name3) {
    return function(f) {
      return function(v) {
        var v1 = f(new Cons(v.value0, v.value1));
        if (v1 instanceof Cons) {
          return new NonEmpty(v1.value0, v1.value1);
        }
        ;
        if (v1 instanceof Nil) {
          return unsafeCrashWith("Impossible: empty list in NonEmptyList " + name3);
        }
        ;
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 92, column 3 - line 94, column 81): " + [v1.constructor.name]);
      };
    };
  };
  var toList3 = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var singleton7 = /* @__PURE__ */ function() {
    var $199 = singleton2(plusList);
    return function($200) {
      return NonEmptyList($199($200));
    };
  }();
  var head3 = function(v) {
    return v.value0;
  };
  var groupBy2 = /* @__PURE__ */ function() {
    var $214 = wrappedOperation("groupBy");
    return function($215) {
      return $214(groupBy($215));
    };
  }();
  var fromList = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just(new NonEmpty(v.value0, v.value1));
    }
    ;
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 121, column 1 - line 121, column 57): " + [v.constructor.name]);
  };
  var cons$prime = function(x2) {
    return function(xs) {
      return new NonEmpty(x2, xs);
    };
  };

  // output/Lattice/index.js
  var pure6 = /* @__PURE__ */ pure(applicativeEither);
  var length6 = /* @__PURE__ */ length2(foldableArray)(semiringInt);
  var sequence2 = /* @__PURE__ */ sequence(traversableArray)(applicativeEither);
  var map12 = /* @__PURE__ */ map3(functorArray);
  var length1 = /* @__PURE__ */ length2(foldableList)(semiringInt);
  var sequence12 = /* @__PURE__ */ sequence(traversableList)(applicativeEither);
  var map13 = /* @__PURE__ */ map3(functorList);
  var lift22 = /* @__PURE__ */ lift2(applyEither);
  var second2 = /* @__PURE__ */ second(strongFn);
  var discard3 = /* @__PURE__ */ discard(discardUnit)(bindEither);
  var flap2 = /* @__PURE__ */ flap(functorEither);
  var map32 = /* @__PURE__ */ map3(functorEither);
  var map42 = /* @__PURE__ */ map3(functorFn);
  var foldM3 = /* @__PURE__ */ foldM(foldableList)(monadEither);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var map52 = /* @__PURE__ */ map3(functorMap);
  var map62 = /* @__PURE__ */ map3(functorTuple);
  var keyString = {
    checkConsistent: function(v) {
      return function(v1) {
        return function(v2) {
          return pure6(unit2);
        };
      };
    },
    Ord0: function() {
      return ordString;
    }
  };
  var joinSemilatticeBoolean = {
    join: /* @__PURE__ */ disj(heytingAlgebraBoolean),
    neg: /* @__PURE__ */ not(heytingAlgebraBoolean)
  };
  var boundedJoinSemilatticeBoo = {
    bot: false,
    JoinSemilattice0: function() {
      return joinSemilatticeBoolean;
    }
  };
  var neg = function(dict) {
    return dict.neg;
  };
  var meet = /* @__PURE__ */ conj(heytingAlgebraBoolean);
  var maybeJoin = function(dict) {
    return dict.maybeJoin;
  };
  var join2 = function(dict) {
    return dict.join;
  };
  var definedJoin = function(dictSlices) {
    var maybeJoin12 = maybeJoin(dictSlices);
    return function(x2) {
      var $157 = successfulWith("Join undefined");
      var $158 = maybeJoin12(x2);
      return function($159) {
        return $157($158($159));
      };
    };
  };
  var slicesArray = function(dictSlices) {
    var maybeJoin12 = maybeJoin(dictSlices);
    return {
      maybeJoin: function(xs) {
        return function(ys) {
          if (length6(xs) === length6(ys)) {
            return sequence2(zipWith(maybeJoin12)(xs)(ys));
          }
          ;
          if (otherwise) {
            return report("Mismatched lengths");
          }
          ;
          throw new Error("Failed pattern match at Lattice (line 129, column 1 - line 132, column 72): " + [xs.constructor.name, ys.constructor.name]);
        };
      },
      JoinSemilattice0: function() {
        return joinSemilatticeArray(dictSlices);
      }
    };
  };
  var joinSemilatticeArray = function(dictSlices) {
    return {
      join: definedJoin(slicesArray(dictSlices)),
      neg: map12(neg(dictSlices.JoinSemilattice0()))
    };
  };
  var slicesList = function(dictSlices) {
    var maybeJoin12 = maybeJoin(dictSlices);
    return {
      maybeJoin: function(xs) {
        return function(ys) {
          if (length1(xs) === length1(ys)) {
            return sequence12(zipWith2(maybeJoin12)(xs)(ys));
          }
          ;
          if (otherwise) {
            return report("Mismatched lengths");
          }
          ;
          throw new Error("Failed pattern match at Lattice (line 80, column 1 - line 83, column 72): " + [xs.constructor.name, ys.constructor.name]);
        };
      },
      JoinSemilattice0: function() {
        return joinSemilatticeList(dictSlices);
      }
    };
  };
  var joinSemilatticeList = function(dictSlices) {
    return {
      join: definedJoin(slicesList(dictSlices)),
      neg: map13(neg(dictSlices.JoinSemilattice0()))
    };
  };
  var slicesTuple = function(dictEq) {
    return function(dictShow) {
      var mayFailEq6 = mayFailEq(dictShow)(dictEq);
      return function(dictSlices) {
        var maybeJoin12 = maybeJoin(dictSlices);
        return {
          maybeJoin: function(v) {
            return function(v1) {
              return lift22(Tuple.create)(mayFailEq6(v.value0)(v1.value0))(maybeJoin12(v.value1)(v1.value1));
            };
          },
          JoinSemilattice0: function() {
            return joinSemilatticeTuple(dictEq)(dictShow)(dictSlices);
          }
        };
      };
    };
  };
  var joinSemilatticeTuple = function(dictEq) {
    return function(dictShow) {
      return function(dictSlices) {
        return {
          join: definedJoin(slicesTuple(dictEq)(dictShow)(dictSlices)),
          neg: second2(neg(dictSlices.JoinSemilattice0()))
        };
      };
    };
  };
  var checkConsistent = function(dict) {
    return dict.checkConsistent;
  };
  var mayFailUpdate = function(dictKey) {
    var Ord0 = dictKey.Ord0();
    var lookup6 = lookup(Ord0);
    var checkConsistent1 = checkConsistent(dictKey);
    var insert5 = insert(Ord0);
    var update4 = update(Ord0);
    return function(dictSlices) {
      var maybeJoin12 = maybeJoin(dictSlices);
      return function(m) {
        return function(v) {
          var v2 = lookup6(v.value0)(m);
          if (v2 instanceof Nothing) {
            return discard3(checkConsistent1("Inconsistent keys: ")(v.value0)(keys2(m)))(function() {
              return pure6(insert5(v.value0)(v.value1)(m));
            });
          }
          ;
          if (v2 instanceof Just) {
            return flap2(flap2(map32(update4)(map32(map42($$const)(Just.create))(maybeJoin12(v2.value0)(v.value1))))(v.value0))(m);
          }
          ;
          throw new Error("Failed pattern match at Lattice (line 115, column 4 - line 120, column 68): " + [v2.constructor.name]);
        };
      };
    };
  };
  var slicesMap = function(dictKey) {
    var mayFailUpdate1 = mayFailUpdate(dictKey);
    return function(dictSlices) {
      var mayFailUpdate2 = mayFailUpdate1(dictSlices);
      return {
        maybeJoin: function(m) {
          return function(m$prime) {
            return foldM3(mayFailUpdate2)(m)(toUnfoldable6(m$prime));
          };
        },
        JoinSemilattice0: function() {
          return joinSemilatticeMap(dictKey)(dictSlices);
        }
      };
    };
  };
  var joinSemilatticeMap = function(dictKey) {
    return function(dictSlices) {
      return {
        join: definedJoin(slicesMap(dictKey)(dictSlices)),
        neg: map52(neg(dictSlices.JoinSemilattice0()))
      };
    };
  };
  var botOf = function(dict) {
    return dict.botOf;
  };
  var boundedSlicesList = function(dictBoundedSlices) {
    var slicesList1 = slicesList(dictBoundedSlices.Slices0());
    return {
      botOf: map13(botOf(dictBoundedSlices)),
      Slices0: function() {
        return slicesList1;
      }
    };
  };
  var boundedSlicesMap = function(dictKey) {
    var slicesMap12 = slicesMap(dictKey);
    return function(dictBoundedSlices) {
      var slicesMap22 = slicesMap12(dictBoundedSlices.Slices0());
      return {
        botOf: map52(botOf(dictBoundedSlices)),
        Slices0: function() {
          return slicesMap22;
        }
      };
    };
  };
  var boundedSlicesTuple = function(dictEq) {
    var slicesTuple1 = slicesTuple(dictEq);
    return function(dictShow) {
      var slicesTuple22 = slicesTuple1(dictShow);
      return function(dictBoundedSlices) {
        var slicesTuple3 = slicesTuple22(dictBoundedSlices.Slices0());
        return {
          botOf: map62(botOf(dictBoundedSlices)),
          Slices0: function() {
            return slicesTuple3;
          }
        };
      };
    };
  };
  var bot = function(dict) {
    return dict.bot;
  };

  // output/Data.Profunctor.Choice/index.js
  var identity14 = /* @__PURE__ */ identity4(categoryFn);
  var right2 = function(dict) {
    return dict.right;
  };
  var left2 = function(dict) {
    return dict.left;
  };
  var splitChoice = function(dictCategory) {
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    return function(dictChoice) {
      var left1 = left2(dictChoice);
      var right1 = right2(dictChoice);
      return function(l) {
        return function(r) {
          return composeFlipped2(left1(l))(right1(r));
        };
      };
    };
  };
  var fanin = function(dictCategory) {
    var identity1 = identity4(dictCategory);
    var composeFlipped2 = composeFlipped(dictCategory.Semigroupoid0());
    var splitChoice1 = splitChoice(dictCategory);
    return function(dictChoice) {
      var dimap2 = dimap(dictChoice.Profunctor0());
      var splitChoice2 = splitChoice1(dictChoice);
      return function(l) {
        return function(r) {
          var join7 = dimap2(either(identity14)(identity14))(identity14)(identity1);
          return composeFlipped2(splitChoice2(l)(r))(join7);
        };
      };
    };
  };
  var choiceFn = {
    left: function(v) {
      return function(v1) {
        if (v1 instanceof Left) {
          return new Left(v(v1.value0));
        }
        ;
        if (v1 instanceof Right) {
          return new Right(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Profunctor.Choice (line 32, column 1 - line 35, column 16): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    right: /* @__PURE__ */ map3(functorEither),
    Profunctor0: function() {
      return profunctorFn;
    }
  };

  // output/Bindings/index.js
  var pure7 = /* @__PURE__ */ pure(applicativeEither);
  var union4 = /* @__PURE__ */ union2(ordString);
  var varAnon = "_";
  var val = snd;
  var find3 = function($copy_x) {
    return function($copy_v) {
      var $tco_var_x = $copy_x;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(x2, v) {
        if (v instanceof Nil) {
          $tco_done = true;
          return report("variable " + (x2 + " not found"));
        }
        ;
        if (v instanceof Cons) {
          if (x2 === v.value0.value0) {
            $tco_done = true;
            return pure7(v.value0.value1);
          }
          ;
          if (otherwise) {
            $tco_var_x = x2;
            $copy_v = v.value1;
            return;
          }
          ;
        }
        ;
        throw new Error("Failed pattern match at Bindings (line 32, column 1 - line 32, column 53): " + [x2.constructor.name, v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_x, $copy_v);
      }
      ;
      return $tco_result;
    };
  };
  var dom = function(v) {
    if (v instanceof Nil) {
      return empty5;
    }
    ;
    if (v instanceof Cons) {
      return union4(singleton6(v.value0.value0))(dom(v.value1));
    }
    ;
    throw new Error("Failed pattern match at Bindings (line 25, column 1 - line 25, column 43): " + [v.constructor.name]);
  };

  // output/Expr/index.js
  var $runtime_lazy5 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var map14 = /* @__PURE__ */ map3(functorMap);
  var map15 = /* @__PURE__ */ map3(functorList);
  var map22 = /* @__PURE__ */ map3(functorTuple);
  var apply3 = /* @__PURE__ */ apply(applyEither);
  var map33 = /* @__PURE__ */ map3(functorEither);
  var mayFailEq3 = /* @__PURE__ */ mayFailEq(showString)(eqString);
  var join3 = /* @__PURE__ */ join2(joinSemilatticeBoolean);
  var mayFailEq12 = /* @__PURE__ */ mayFailEq(showInt)(eqInt);
  var mayFailEq22 = /* @__PURE__ */ mayFailEq(showNumber)(eqNumber);
  var slicesMap2 = /* @__PURE__ */ slicesMap(keyString);
  var mayFailEq32 = /* @__PURE__ */ mayFailEq(showCtr)(eqCtr);
  var lift23 = /* @__PURE__ */ lift2(applyEither);
  var slicesTuple2 = /* @__PURE__ */ slicesTuple(eqString)(showString);
  var slicesMap1 = /* @__PURE__ */ slicesMap(keyCtr);
  var mayFailEq4 = /* @__PURE__ */ mayFailEq(/* @__PURE__ */ showSet(showString))(/* @__PURE__ */ eqSet(eqString));
  var pure8 = /* @__PURE__ */ pure(applicativeEither);
  var neg2 = /* @__PURE__ */ neg(joinSemilatticeBoolean);
  var bot2 = /* @__PURE__ */ bot(boundedJoinSemilatticeBoo);
  var boundedSlicesMap2 = /* @__PURE__ */ boundedSlicesMap(keyString);
  var boundedSlicesTuple2 = /* @__PURE__ */ boundedSlicesTuple(eqString)(showString);
  var difference4 = /* @__PURE__ */ difference2(ordString);
  var unions3 = /* @__PURE__ */ unions(foldableMap)(ordString);
  var union5 = /* @__PURE__ */ union2(ordString);
  var unions1 = /* @__PURE__ */ unions(foldableList)(ordString);
  var map43 = /* @__PURE__ */ map3(functorFn);
  var ContNone = /* @__PURE__ */ function() {
    function ContNone2() {
    }
    ;
    ContNone2.value = new ContNone2();
    return ContNone2;
  }();
  var ContExpr = /* @__PURE__ */ function() {
    function ContExpr2(value0) {
      this.value0 = value0;
    }
    ;
    ContExpr2.create = function(value0) {
      return new ContExpr2(value0);
    };
    return ContExpr2;
  }();
  var ContElim = /* @__PURE__ */ function() {
    function ContElim2(value0) {
      this.value0 = value0;
    }
    ;
    ContElim2.create = function(value0) {
      return new ContElim2(value0);
    };
    return ContElim2;
  }();
  var Var = /* @__PURE__ */ function() {
    function Var4(value0) {
      this.value0 = value0;
    }
    ;
    Var4.create = function(value0) {
      return new Var4(value0);
    };
    return Var4;
  }();
  var Op = /* @__PURE__ */ function() {
    function Op4(value0) {
      this.value0 = value0;
    }
    ;
    Op4.create = function(value0) {
      return new Op4(value0);
    };
    return Op4;
  }();
  var Int = /* @__PURE__ */ function() {
    function Int5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Int5.create = function(value0) {
      return function(value1) {
        return new Int5(value0, value1);
      };
    };
    return Int5;
  }();
  var Float = /* @__PURE__ */ function() {
    function Float5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Float5.create = function(value0) {
      return function(value1) {
        return new Float5(value0, value1);
      };
    };
    return Float5;
  }();
  var Str = /* @__PURE__ */ function() {
    function Str5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Str5.create = function(value0) {
      return function(value1) {
        return new Str5(value0, value1);
      };
    };
    return Str5;
  }();
  var Record = /* @__PURE__ */ function() {
    function Record5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Record5.create = function(value0) {
      return function(value1) {
        return new Record5(value0, value1);
      };
    };
    return Record5;
  }();
  var Constr = /* @__PURE__ */ function() {
    function Constr5(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Constr5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Constr5(value0, value1, value2);
        };
      };
    };
    return Constr5;
  }();
  var Matrix = /* @__PURE__ */ function() {
    function Matrix5(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Matrix5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Matrix5(value0, value1, value2, value3);
          };
        };
      };
    };
    return Matrix5;
  }();
  var Lambda = /* @__PURE__ */ function() {
    function Lambda4(value0) {
      this.value0 = value0;
    }
    ;
    Lambda4.create = function(value0) {
      return new Lambda4(value0);
    };
    return Lambda4;
  }();
  var Project = /* @__PURE__ */ function() {
    function Project4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Project4.create = function(value0) {
      return function(value1) {
        return new Project4(value0, value1);
      };
    };
    return Project4;
  }();
  var App2 = /* @__PURE__ */ function() {
    function App5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    App5.create = function(value0) {
      return function(value1) {
        return new App5(value0, value1);
      };
    };
    return App5;
  }();
  var Let = /* @__PURE__ */ function() {
    function Let4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Let4.create = function(value0) {
      return function(value1) {
        return new Let4(value0, value1);
      };
    };
    return Let4;
  }();
  var LetRec = /* @__PURE__ */ function() {
    function LetRec4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    LetRec4.create = function(value0) {
      return function(value1) {
        return new LetRec4(value0, value1);
      };
    };
    return LetRec4;
  }();
  var ElimVar = /* @__PURE__ */ function() {
    function ElimVar2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ElimVar2.create = function(value0) {
      return function(value1) {
        return new ElimVar2(value0, value1);
      };
    };
    return ElimVar2;
  }();
  var ElimConstr = /* @__PURE__ */ function() {
    function ElimConstr2(value0) {
      this.value0 = value0;
    }
    ;
    ElimConstr2.create = function(value0) {
      return new ElimConstr2(value0);
    };
    return ElimConstr2;
  }();
  var ElimRecord = /* @__PURE__ */ function() {
    function ElimRecord2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ElimRecord2.create = function(value0) {
      return function(value1) {
        return new ElimRecord2(value0, value1);
      };
    };
    return ElimRecord2;
  }();
  var VarDef = /* @__PURE__ */ function() {
    function VarDef4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    VarDef4.create = function(value0) {
      return function(value1) {
        return new VarDef4(value0, value1);
      };
    };
    return VarDef4;
  }();
  var Module = /* @__PURE__ */ function() {
    function Module3(value0) {
      this.value0 = value0;
    }
    ;
    Module3.create = function(value0) {
      return new Module3(value0);
    };
    return Module3;
  }();
  var functorVarDef = {
    map: function(f) {
      return function(m) {
        return new VarDef(map3(functorElim)(f)(m.value0), map3(functorExpr)(f)(m.value1));
      };
    }
  };
  var functorExpr = {
    map: function(f) {
      return function(m) {
        if (m instanceof Var) {
          return new Var(m.value0);
        }
        ;
        if (m instanceof Op) {
          return new Op(m.value0);
        }
        ;
        if (m instanceof Int) {
          return new Int(f(m.value0), m.value1);
        }
        ;
        if (m instanceof Float) {
          return new Float(f(m.value0), m.value1);
        }
        ;
        if (m instanceof Str) {
          return new Str(f(m.value0), m.value1);
        }
        ;
        if (m instanceof Record) {
          return new Record(f(m.value0), map14(map3(functorExpr)(f))(m.value1));
        }
        ;
        if (m instanceof Constr) {
          return new Constr(f(m.value0), m.value1, map15(map3(functorExpr)(f))(m.value2));
        }
        ;
        if (m instanceof Matrix) {
          return new Matrix(f(m.value0), map3(functorExpr)(f)(m.value1), m.value2, map3(functorExpr)(f)(m.value3));
        }
        ;
        if (m instanceof Lambda) {
          return new Lambda(map3(functorElim)(f)(m.value0));
        }
        ;
        if (m instanceof Project) {
          return new Project(map3(functorExpr)(f)(m.value0), m.value1);
        }
        ;
        if (m instanceof App2) {
          return new App2(map3(functorExpr)(f)(m.value0), map3(functorExpr)(f)(m.value1));
        }
        ;
        if (m instanceof Let) {
          return new Let(map3(functorVarDef)(f)(m.value0), map3(functorExpr)(f)(m.value1));
        }
        ;
        if (m instanceof LetRec) {
          return new LetRec(map15(map22(map3(functorElim)(f)))(m.value0), map3(functorExpr)(f)(m.value1));
        }
        ;
        throw new Error("Failed pattern match at Expr (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var functorElim = {
    map: function(f) {
      return function(m) {
        if (m instanceof ElimVar) {
          return new ElimVar(m.value0, map3(functorCont)(f)(m.value1));
        }
        ;
        if (m instanceof ElimConstr) {
          return new ElimConstr(map14(map3(functorCont)(f))(m.value0));
        }
        ;
        if (m instanceof ElimRecord) {
          return new ElimRecord(m.value0, map3(functorCont)(f)(m.value1));
        }
        ;
        throw new Error("Failed pattern match at Expr (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var functorCont = {
    map: function(f) {
      return function(m) {
        if (m instanceof ContNone) {
          return ContNone.value;
        }
        ;
        if (m instanceof ContExpr) {
          return new ContExpr(map3(functorExpr)(f)(m.value0));
        }
        ;
        if (m instanceof ContElim) {
          return new ContElim(map3(functorElim)(f)(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Expr (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var slicesVarDefBoolean = {
    maybeJoin: function(v) {
      return function(v1) {
        return apply3(map33(VarDef.create)(maybeJoin(slicesElimBoolean)(v.value0)(v1.value0)))(maybeJoin(slicesExprBoolean)(v.value1)(v1.value1));
      };
    },
    JoinSemilattice0: function() {
      return $lazy_joinSemilatticeVarDefBool(0);
    }
  };
  var slicesExprBoolean = {
    maybeJoin: function(v) {
      return function(v1) {
        if (v instanceof Var && v1 instanceof Var) {
          return map33(Var.create)(mayFailEq3(v.value0)(v1.value0));
        }
        ;
        if (v instanceof Op && v1 instanceof Op) {
          return map33(Op.create)(mayFailEq3(v.value0)(v1.value0));
        }
        ;
        if (v instanceof Int && v1 instanceof Int) {
          return map33(Int.create(join3(v.value0)(v1.value0)))(mayFailEq12(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Str && v1 instanceof Str) {
          return map33(Str.create(join3(v.value0)(v1.value0)))(mayFailEq3(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Float && v1 instanceof Float) {
          return map33(Float.create(join3(v.value0)(v1.value0)))(mayFailEq22(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Record && v1 instanceof Record) {
          return map33(Record.create(join3(v.value0)(v1.value0)))(maybeJoin(slicesMap2(slicesExprBoolean))(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Constr && v1 instanceof Constr) {
          return apply3(map33(Constr.create(join3(v.value0)(v1.value0)))(mayFailEq32(v.value1)(v1.value1)))(maybeJoin(slicesList(slicesExprBoolean))(v.value2)(v1.value2));
        }
        ;
        if (v instanceof Matrix && v1 instanceof Matrix) {
          return apply3(apply3(map33(Matrix.create(join3(v.value0)(v1.value0)))(maybeJoin(slicesExprBoolean)(v.value1)(v1.value1)))(lift23(Tuple.create)(mayFailEq3(v.value2.value0)(v1.value2.value0))(mayFailEq3(v.value2.value1)(v1.value2.value1))))(maybeJoin(slicesExprBoolean)(v.value3)(v1.value3));
        }
        ;
        if (v instanceof Lambda && v1 instanceof Lambda) {
          return map33(Lambda.create)(maybeJoin(slicesElimBoolean)(v.value0)(v1.value0));
        }
        ;
        if (v instanceof Project && v1 instanceof Project) {
          return apply3(map33(Project.create)(maybeJoin(slicesExprBoolean)(v.value0)(v1.value0)))(mayFailEq3(v.value1)(v1.value1));
        }
        ;
        if (v instanceof App2 && v1 instanceof App2) {
          return apply3(map33(App2.create)(maybeJoin(slicesExprBoolean)(v.value0)(v1.value0)))(maybeJoin(slicesExprBoolean)(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Let && v1 instanceof Let) {
          return apply3(map33(Let.create)(maybeJoin(slicesVarDefBoolean)(v.value0)(v1.value0)))(maybeJoin(slicesExprBoolean)(v.value1)(v1.value1));
        }
        ;
        if (v instanceof LetRec && v1 instanceof LetRec) {
          return apply3(map33(LetRec.create)(maybeJoin(slicesList(slicesTuple2(slicesElimBoolean)))(v.value0)(v1.value0)))(maybeJoin(slicesExprBoolean)(v.value1)(v1.value1));
        }
        ;
        return report("Incompatible expressions");
      };
    },
    JoinSemilattice0: function() {
      return $lazy_joinSemilatticeExprBoolea(0);
    }
  };
  var slicesElimBoolean = {
    maybeJoin: function(v) {
      return function(v1) {
        if (v instanceof ElimVar && v1 instanceof ElimVar) {
          return apply3(map33(ElimVar.create)(mayFailEq3(v.value0)(v1.value0)))(maybeJoin(slicesContBoolean)(v.value1)(v1.value1));
        }
        ;
        if (v instanceof ElimConstr && v1 instanceof ElimConstr) {
          return map33(ElimConstr.create)(maybeJoin(slicesMap1(slicesContBoolean))(v.value0)(v1.value0));
        }
        ;
        if (v instanceof ElimRecord && v1 instanceof ElimRecord) {
          return apply3(map33(ElimRecord.create)(mayFailEq4(v.value0)(v1.value0)))(maybeJoin(slicesContBoolean)(v.value1)(v1.value1));
        }
        ;
        return report("Incompatible eliminators");
      };
    },
    JoinSemilattice0: function() {
      return $lazy_joinSemilatticeElimBoolea(0);
    }
  };
  var slicesContBoolean = {
    maybeJoin: function(v) {
      return function(v1) {
        if (v instanceof ContNone && v1 instanceof ContNone) {
          return pure8(ContNone.value);
        }
        ;
        if (v instanceof ContExpr && v1 instanceof ContExpr) {
          return map33(ContExpr.create)(maybeJoin(slicesExprBoolean)(v.value0)(v1.value0));
        }
        ;
        if (v instanceof ContElim && v1 instanceof ContElim) {
          return map33(ContElim.create)(maybeJoin(slicesElimBoolean)(v.value0)(v1.value0));
        }
        ;
        return report("Incompatible continuations");
      };
    },
    JoinSemilattice0: function() {
      return $lazy_joinSemilatticeContBoolea(0);
    }
  };
  var $lazy_joinSemilatticeContBoolea = /* @__PURE__ */ $runtime_lazy5("joinSemilatticeContBoolea", "Expr", function() {
    return {
      join: definedJoin(slicesContBoolean),
      neg: map3(functorCont)(neg2)
    };
  });
  var $lazy_joinSemilatticeElimBoolea = /* @__PURE__ */ $runtime_lazy5("joinSemilatticeElimBoolea", "Expr", function() {
    return {
      join: definedJoin(slicesElimBoolean),
      neg: map3(functorElim)(neg2)
    };
  });
  var $lazy_joinSemilatticeExprBoolea = /* @__PURE__ */ $runtime_lazy5("joinSemilatticeExprBoolea", "Expr", function() {
    return {
      join: definedJoin(slicesExprBoolean),
      neg: map3(functorExpr)(neg2)
    };
  });
  var $lazy_joinSemilatticeVarDefBool = /* @__PURE__ */ $runtime_lazy5("joinSemilatticeVarDefBool", "Expr", function() {
    return {
      join: definedJoin(slicesVarDefBoolean),
      neg: map3(functorVarDef)(neg2)
    };
  });
  var joinSemilatticeExprBoolea = /* @__PURE__ */ $lazy_joinSemilatticeExprBoolea(168);
  var boundedSlicesVarDefBoolea = {
    botOf: function(v) {
      return new VarDef(botOf(boundedSlicesElimBoolean)(v.value0), botOf(boundedSlicesExprBoolean)(v.value1));
    },
    Slices0: function() {
      return slicesVarDefBoolean;
    }
  };
  var boundedSlicesExprBoolean = {
    botOf: function(v) {
      if (v instanceof Var) {
        return new Var(v.value0);
      }
      ;
      if (v instanceof Op) {
        return new Op(v.value0);
      }
      ;
      if (v instanceof Int) {
        return new Int(bot2, v.value1);
      }
      ;
      if (v instanceof Str) {
        return new Str(bot2, v.value1);
      }
      ;
      if (v instanceof Float) {
        return new Float(bot2, v.value1);
      }
      ;
      if (v instanceof Record) {
        return new Record(bot2, botOf(boundedSlicesMap2(boundedSlicesExprBoolean))(v.value1));
      }
      ;
      if (v instanceof Constr) {
        return new Constr(bot2, v.value1, botOf(boundedSlicesList(boundedSlicesExprBoolean))(v.value2));
      }
      ;
      if (v instanceof Matrix) {
        return new Matrix(bot2, botOf(boundedSlicesExprBoolean)(v.value1), new Tuple(v.value2.value0, v.value2.value1), botOf(boundedSlicesExprBoolean)(v.value3));
      }
      ;
      if (v instanceof Lambda) {
        return new Lambda(botOf(boundedSlicesElimBoolean)(v.value0));
      }
      ;
      if (v instanceof Project) {
        return new Project(botOf(boundedSlicesExprBoolean)(v.value0), v.value1);
      }
      ;
      if (v instanceof App2) {
        return new App2(botOf(boundedSlicesExprBoolean)(v.value0), botOf(boundedSlicesExprBoolean)(v.value1));
      }
      ;
      if (v instanceof Let) {
        return new Let(botOf(boundedSlicesVarDefBoolea)(v.value0), botOf(boundedSlicesExprBoolean)(v.value1));
      }
      ;
      if (v instanceof LetRec) {
        return new LetRec(botOf(boundedSlicesList(boundedSlicesTuple2(boundedSlicesElimBoolean)))(v.value0), botOf(boundedSlicesExprBoolean)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Expr (line 153, column 1 - line 166, column 65): " + [v.constructor.name]);
    },
    Slices0: function() {
      return slicesExprBoolean;
    }
  };
  var boundedSlicesElimBoolean = {
    botOf: function(v) {
      if (v instanceof ElimVar) {
        return new ElimVar(v.value0, botOf(boundedSlicesContBoolean)(v.value1));
      }
      ;
      if (v instanceof ElimConstr) {
        return new ElimConstr(map14(botOf(boundedSlicesContBoolean))(v.value0));
      }
      ;
      if (v instanceof ElimRecord) {
        return new ElimRecord(v.value0, botOf(boundedSlicesContBoolean)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Expr (line 123, column 1 - line 126, column 53): " + [v.constructor.name]);
    },
    Slices0: function() {
      return slicesElimBoolean;
    }
  };
  var boundedSlicesContBoolean = {
    botOf: function(v) {
      if (v instanceof ContNone) {
        return ContNone.value;
      }
      ;
      if (v instanceof ContExpr) {
        return new ContExpr(botOf(boundedSlicesExprBoolean)(v.value0));
      }
      ;
      if (v instanceof ContElim) {
        return new ContElim(botOf(boundedSlicesElimBoolean)(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Expr (line 138, column 1 - line 141, column 45): " + [v.constructor.name]);
    },
    Slices0: function() {
      return slicesContBoolean;
    }
  };
  var fv = function(dict) {
    return dict.fv;
  };
  var fVMapVar = function(dictFV) {
    var fv13 = fv(dictFV);
    return {
      fv: function(\u03C1) {
        return difference4(unions3(map14(fv13)(\u03C1)))(keys2(\u03C1));
      }
    };
  };
  var bv = function(dict) {
    return dict.bv;
  };
  var bVElim = {
    bv: function(v) {
      if (v instanceof ElimVar) {
        return union5(singleton6(v.value0))(bv(bVCont)(v.value1));
      }
      ;
      if (v instanceof ElimConstr) {
        return bv(bVCont)(snd(asSingletonMap(v.value0)));
      }
      ;
      if (v instanceof ElimRecord) {
        return bv(bVCont)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at Expr (line 92, column 1 - line 95, column 30): " + [v.constructor.name]);
    }
  };
  var bVCont = {
    bv: function(v) {
      if (v instanceof ContNone) {
        return empty5;
      }
      ;
      if (v instanceof ContElim) {
        return bv(bVElim)(v.value0);
      }
      ;
      if (v instanceof ContExpr) {
        return empty5;
      }
      ;
      throw new Error("Failed pattern match at Expr (line 100, column 1 - line 103, column 29): " + [v.constructor.name]);
    }
  };
  var bv1 = /* @__PURE__ */ bv(bVElim);
  var bVVarDef = {
    bv: function(v) {
      return bv1(v.value0);
    }
  };
  var bv2 = /* @__PURE__ */ bv(bVVarDef);
  var fVVarDef = {
    fv: function(v) {
      return fv(fVExpr)(v.value1);
    }
  };
  var fVExpr = {
    fv: function(v) {
      if (v instanceof Var) {
        return singleton6(v.value0);
      }
      ;
      if (v instanceof Op) {
        return singleton6(v.value0);
      }
      ;
      if (v instanceof Int) {
        return empty5;
      }
      ;
      if (v instanceof Float) {
        return empty5;
      }
      ;
      if (v instanceof Str) {
        return empty5;
      }
      ;
      if (v instanceof Record) {
        return unions3(map14(fv(fVExpr))(v.value1));
      }
      ;
      if (v instanceof Constr) {
        return unions1(map15(fv(fVExpr))(v.value2));
      }
      ;
      if (v instanceof Matrix) {
        return union5(fv(fVExpr)(v.value1))(fv(fVExpr)(v.value3));
      }
      ;
      if (v instanceof Lambda) {
        return fv(fVElim)(v.value0);
      }
      ;
      if (v instanceof Project) {
        return fv(fVExpr)(v.value0);
      }
      ;
      if (v instanceof App2) {
        return union5(fv(fVExpr)(v.value0))(fv(fVExpr)(v.value1));
      }
      ;
      if (v instanceof Let) {
        return union5(fv(fVVarDef)(v.value0))(difference4(fv(fVExpr)(v.value1))(bv2(v.value0)));
      }
      ;
      if (v instanceof LetRec) {
        return union5(unions1(map15(map43(fv(fVElim))(val))(v.value0)))(fv(fVExpr)(v.value1));
      }
      ;
      throw new Error("Failed pattern match at Expr (line 57, column 1 - line 70, column 68): " + [v.constructor.name]);
    }
  };
  var fVElim = {
    fv: function(v) {
      if (v instanceof ElimVar) {
        return difference4(fv(fVCont)(v.value1))(singleton6(v.value0));
      }
      ;
      if (v instanceof ElimConstr) {
        return unions3(map14(fv(fVCont))(v.value0));
      }
      ;
      if (v instanceof ElimRecord) {
        return fv(fVCont)(v.value1);
      }
      ;
      throw new Error("Failed pattern match at Expr (line 72, column 1 - line 75, column 31): " + [v.constructor.name]);
    }
  };
  var fVCont = {
    fv: function(v) {
      if (v instanceof ContNone) {
        return empty5;
      }
      ;
      if (v instanceof ContElim) {
        return fv(fVElim)(v.value0);
      }
      ;
      if (v instanceof ContExpr) {
        return fv(fVExpr)(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Expr (line 77, column 1 - line 80, column 28): " + [v.constructor.name]);
    }
  };
  var asExpr = function(v) {
    if (v instanceof ContExpr) {
      return v.value0;
    }
    ;
    return error2("Expression expected");
  };
  var asElim = function(v) {
    if (v instanceof ContElim) {
      return v.value0;
    }
    ;
    return error2("Eliminator expected");
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map50 = map3(Monad0.Bind1().Apply0().Functor0());
    var pure24 = pure(Monad0.Applicative0());
    return function(a) {
      return catchError1(map50(Right.create)(a))(function($52) {
        return pure24(Left.create($52));
      });
    };
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate4 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append3 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsArgument = function(dictShow) {
    var show11 = show(dictShow);
    return {
      genericShowArgs: function(v) {
        return [show11(v)];
      }
    };
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate4(" ")(append3([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShow = function(dictGeneric) {
    var from7 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x2) {
        return genericShow$prime1(from7(x2));
      };
    };
  };

  // output/Parsing/index.js
  var $runtime_lazy6 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var show3 = /* @__PURE__ */ show(showString);
  var unwrap5 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new ParseState2(value0, value1, value2);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift2(value0) {
      this.value0 = value0;
    }
    ;
    Lift2.create = function(value0) {
      return new Lift2(value0);
    };
    return Lift2;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var lazyParserT = {
    defer: function(f) {
      var m = defer2(f);
      return function(state1, more, lift1, $$throw2, done) {
        var v = force(m);
        return v(state1, more, lift1, $$throw2, done);
      };
    }
  };
  var genericPosition_ = {
    to: function(x2) {
      return x2;
    },
    from: function(x2) {
      return x2;
    }
  };
  var genericShow2 = /* @__PURE__ */ genericShow(genericPosition_)(/* @__PURE__ */ genericShowConstructor(/* @__PURE__ */ genericShowArgsArgument(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "column";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "index";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "line";
    }
  })(showRecordFieldsNil)(showInt))(showInt))(showInt))))({
    reflectSymbol: function() {
      return "Position";
    }
  }));
  var showPosition = {
    show: function(x2) {
      return genericShow2(x2);
    }
  };
  var show12 = /* @__PURE__ */ show(showPosition);
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state2, a) {
              return more(function(v2) {
                return done(state2, f(a));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state2, f) {
              return more(function(v3) {
                return v1(state2, more, lift1, $$throw2, function(state3, a) {
                  return more(function(v4) {
                    return done(state3, f(a));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state2, a) {
              return more(function(v2) {
                var v3 = next(a);
                return v3(state2, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadRecParserT = {
    tailRecM: function(next) {
      return function(initArg) {
        return function(state1, more, lift1, $$throw2, done) {
          var $lazy_loop = $runtime_lazy6("loop", "Parsing", function() {
            return function(state2, arg, gas) {
              var v = next(arg);
              return v(state2, more, lift1, $$throw2, function(state3, step2) {
                if (step2 instanceof Loop) {
                  var $188 = gas === 0;
                  if ($188) {
                    return more(function(v1) {
                      return $lazy_loop(269)(state3, step2.value0, 30);
                    });
                  }
                  ;
                  return $lazy_loop(271)(state3, step2.value0, gas - 1 | 0);
                }
                ;
                if (step2 instanceof Done) {
                  return done(state3, step2.value0);
                }
                ;
                throw new Error("Failed pattern match at Parsing (line 265, column 39 - line 273, column 43): " + [step2.constructor.name]);
              });
            };
          });
          var loop = $lazy_loop(262);
          return loop(state1, initArg, 30);
        };
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError2 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k) {
    return function(state1, v, v1, v2, done) {
      var v3 = k(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var showParseError = {
    show: function(v) {
      return "(ParseError " + (show3(v.value0) + (" " + (show12(v.value1) + ")")));
    }
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map50 = map3(Monad0.Bind1().Apply0().Functor0());
    var pure112 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step2) {
            var v1 = step2(unit2);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift) {
              $tco_done = true;
              return map50(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure112(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 144, column 13 - line 150, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM4(go)(function(v1) {
          return v(state1, More.create, Lift.create, function(state2, err) {
            return new Stop(state2, new Left(err));
          }, function(state2, res) {
            return new Stop(state2, new Right(res));
          });
        });
      };
    };
  };
  var position = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map50 = map3(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p) {
        var initialState = new ParseState(s, initialPos, false);
        return map50(fst)(runParserT$prime1(initialState)(p));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $253 = runParserT1(s);
    return function($254) {
      return unwrap5($253($254));
    };
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError2(new ParseError(message2, pos));
    };
  };
  var fail = function(message2) {
    return bindFlipped3(failWithPosition(message2))(position);
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };
  var monadPlusParserT = {
    Monad0: function() {
      return monadParserT;
    },
    Alternative1: function() {
      return alternativeParserT;
    }
  };

  // output/Parsing.Combinators/index.js
  var alt2 = /* @__PURE__ */ alt(altParserT);
  var defer4 = /* @__PURE__ */ defer(lazyParserT);
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorParserT);
  var pure9 = /* @__PURE__ */ pure(applicativeParserT);
  var applySecond2 = /* @__PURE__ */ applySecond(applyParserT);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecParserT);
  var bind6 = /* @__PURE__ */ bind(bindParserT);
  var map16 = /* @__PURE__ */ map3(functorParserT);
  var manyRec2 = /* @__PURE__ */ manyRec(monadRecParserT)(alternativeParserT);
  var applyFirst2 = /* @__PURE__ */ applyFirst(applyParserT);
  var empty6 = /* @__PURE__ */ empty3(plusParserT);
  var withLazyErrorMessage = function(p) {
    return function(msg) {
      return alt2(p)(defer4(function(v) {
        return fail("Expected " + msg(unit2));
      }));
    };
  };
  var withErrorMessage = function(p) {
    return function(msg) {
      return alt2(p)(fail("Expected " + msg));
    };
  };
  var $$try2 = function(v) {
    return function(v1, more, lift3, $$throw2, done) {
      return v(v1, more, lift3, function(v2, err) {
        return $$throw2(new ParseState(v2.value0, v2.value1, v1.value2), err);
      }, done);
    };
  };
  var skipMany1 = function(p) {
    var go = function(v) {
      return alt2(voidLeft2(p)(new Loop(unit2)))(pure9(new Done(unit2)));
    };
    return applySecond2(p)(tailRecM3(go)(unit2));
  };
  var skipMany = function(p) {
    return alt2(skipMany1(p))(pure9(unit2));
  };
  var sepBy1 = function(p) {
    return function(sep) {
      return bind6(p)(function(a) {
        return bind6(manyRec2(applySecond2(sep)(p)))(function(as3) {
          return pure9(cons$prime(a)(as3));
        });
      });
    };
  };
  var sepBy = function(p) {
    return function(sep) {
      return alt2(map16(toList3)(sepBy1(p)(sep)))(pure9(Nil.value));
    };
  };
  var option = function(a) {
    return function(p) {
      return alt2(p)(pure9(a));
    };
  };
  var notFollowedBy = function(p) {
    return $$try2(alt2(applySecond2($$try2(p))(fail("Negated parser succeeded")))(pure9(unit2)));
  };
  var choice = function(dictFoldable) {
    var go = function(p1) {
      return function(v) {
        if (v instanceof Nothing) {
          return new Just(p1);
        }
        ;
        if (v instanceof Just) {
          return new Just(alt2(p1)(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Combinators (line 357, column 11 - line 359, column 32): " + [v.constructor.name]);
      };
    };
    var $92 = fromMaybe(empty6);
    var $93 = foldr(dictFoldable)(go)(Nothing.value);
    return function($94) {
      return $92($93($94));
    };
  };
  var between = function(open2) {
    return function(close) {
      return function(p) {
        return applyFirst2(applySecond2(open2)(p))(close);
      };
    };
  };
  var asErrorMessage = /* @__PURE__ */ flip(withErrorMessage);

  // output/Parsing.Expr/index.js
  var bind7 = /* @__PURE__ */ bind(bindParserT);
  var pure10 = /* @__PURE__ */ pure(applicativeParserT);
  var alt3 = /* @__PURE__ */ alt(altParserT);
  var foldr4 = /* @__PURE__ */ foldr(foldableArray);
  var choice2 = /* @__PURE__ */ choice(foldableList);
  var identity15 = /* @__PURE__ */ identity4(categoryFn);
  var foldl5 = /* @__PURE__ */ foldl(foldableArray);
  var AssocNone = /* @__PURE__ */ function() {
    function AssocNone2() {
    }
    ;
    AssocNone2.value = new AssocNone2();
    return AssocNone2;
  }();
  var AssocLeft = /* @__PURE__ */ function() {
    function AssocLeft2() {
    }
    ;
    AssocLeft2.value = new AssocLeft2();
    return AssocLeft2;
  }();
  var AssocRight = /* @__PURE__ */ function() {
    function AssocRight2() {
    }
    ;
    AssocRight2.value = new AssocRight2();
    return AssocRight2;
  }();
  var Infix = /* @__PURE__ */ function() {
    function Infix2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Infix2.create = function(value0) {
      return function(value1) {
        return new Infix2(value0, value1);
      };
    };
    return Infix2;
  }();
  var Prefix = /* @__PURE__ */ function() {
    function Prefix2(value0) {
      this.value0 = value0;
    }
    ;
    Prefix2.create = function(value0) {
      return new Prefix2(value0);
    };
    return Prefix2;
  }();
  var Postfix = /* @__PURE__ */ function() {
    function Postfix2(value0) {
      this.value0 = value0;
    }
    ;
    Postfix2.create = function(value0) {
      return new Postfix2(value0);
    };
    return Postfix2;
  }();
  var termP = function(prefixP) {
    return function(term) {
      return function(postfixP) {
        return bind7(prefixP)(function(pre) {
          return bind7(term)(function(x2) {
            return bind7(postfixP)(function(post2) {
              return pure10(post2(pre(x2)));
            });
          });
        });
      };
    };
  };
  var splitOp = function(v) {
    return function(accum) {
      if (v instanceof Infix && v.value1 instanceof AssocNone) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: new Cons(v.value0, accum.nassoc),
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Infix && v.value1 instanceof AssocLeft) {
        return {
          rassoc: accum.rassoc,
          lassoc: new Cons(v.value0, accum.lassoc),
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Infix && v.value1 instanceof AssocRight) {
        return {
          rassoc: new Cons(v.value0, accum.rassoc),
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Prefix) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: new Cons(v.value0, accum.prefix),
          postfix: accum.postfix
        };
      }
      ;
      if (v instanceof Postfix) {
        return {
          rassoc: accum.rassoc,
          lassoc: accum.lassoc,
          nassoc: accum.nassoc,
          prefix: accum.prefix,
          postfix: new Cons(v.value0, accum.postfix)
        };
      }
      ;
      throw new Error("Failed pattern match at Parsing.Expr (line 78, column 1 - line 78, column 80): " + [v.constructor.name, accum.constructor.name]);
    };
  };
  var rassocP1 = function(x2) {
    return function(rassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return alt3(rassocP(x2)(rassocOp)(prefixP)(term)(postfixP))(pure10(x2));
          };
        };
      };
    };
  };
  var rassocP = function(x2) {
    return function(rassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind7(rassocOp)(function(f) {
              return bind7(bind7(termP(prefixP)(term)(postfixP))(function(z) {
                return rassocP1(z)(rassocOp)(prefixP)(term)(postfixP);
              }))(function(y2) {
                return pure10(f(x2)(y2));
              });
            });
          };
        };
      };
    };
  };
  var nassocP = function(x2) {
    return function(nassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind7(nassocOp)(function(f) {
              return bind7(termP(prefixP)(term)(postfixP))(function(y2) {
                return pure10(f(x2)(y2));
              });
            });
          };
        };
      };
    };
  };
  var lassocP1 = function(x2) {
    return function(lassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return alt3(lassocP(x2)(lassocOp)(prefixP)(term)(postfixP))(pure10(x2));
          };
        };
      };
    };
  };
  var lassocP = function(x2) {
    return function(lassocOp) {
      return function(prefixP) {
        return function(term) {
          return function(postfixP) {
            return bind7(lassocOp)(function(f) {
              return bind7(termP(prefixP)(term)(postfixP))(function(y2) {
                return lassocP1(f(x2)(y2))(lassocOp)(prefixP)(term)(postfixP);
              });
            });
          };
        };
      };
    };
  };
  var makeParser = function(term) {
    return function(ops) {
      var accum = foldr4(splitOp)({
        rassoc: Nil.value,
        lassoc: Nil.value,
        nassoc: Nil.value,
        prefix: Nil.value,
        postfix: Nil.value
      })(ops);
      var lassocOp = choice2(accum.lassoc);
      var nassocOp = choice2(accum.nassoc);
      var postfixOp = withErrorMessage(choice2(accum.postfix))("");
      var postfixP = alt3(postfixOp)(pure10(identity15));
      var prefixOp = withErrorMessage(choice2(accum.prefix))("");
      var prefixP = alt3(prefixOp)(pure10(identity15));
      var rassocOp = choice2(accum.rassoc);
      return bind7(termP(prefixP)(term)(postfixP))(function(x2) {
        return alt3(rassocP(x2)(rassocOp)(prefixP)(term)(postfixP))(alt3(lassocP(x2)(lassocOp)(prefixP)(term)(postfixP))(alt3(nassocP(x2)(nassocOp)(prefixP)(term)(postfixP))(withErrorMessage(pure10(x2))("operator"))));
      });
    };
  };
  var buildExprParser = function(operators2) {
    return function(simpleExpr) {
      return foldl5(makeParser)(simpleExpr)(operators2);
    };
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    };
    function finalCell(head6) {
      return new ConsCell(head6, emptyList);
    }
    function consList(x2) {
      return function(xs) {
        return new ConsCell(x2, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply10) {
      return function(map50) {
        return function(f) {
          var buildFrom = function(x2, ys) {
            return apply10(map50(consList)(f(x2)))(ys);
          };
          var go = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map50(finalCell)(f(array[array.length - 1]));
            var result = go(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map50(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Function.Uncurried/foreign.js
  var mkFn5 = function(fn) {
    return function(a, b, c, d, e) {
      return fn(a)(b)(c)(d)(e);
    };
  };
  var runFn3 = function(fn) {
    return function(a) {
      return function(b) {
        return function(c) {
          return fn(a, b, c);
        };
      };
    };
  };

  // output/Parsing.String/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust4 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show4 = /* @__PURE__ */ show(showString);
  var show22 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum4(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum4(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var updatePosString = function($copy_pos) {
    return function($copy_before) {
      return function($copy_after) {
        var $tco_var_pos = $copy_pos;
        var $tco_var_before = $copy_before;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(pos, before, after) {
          var v = uncons4(before);
          if (v instanceof Nothing) {
            $tco_done = true;
            return pos;
          }
          ;
          if (v instanceof Just) {
            var newPos = function() {
              if ($$null2(v.value0.tail)) {
                return updatePosSingle(pos)(v.value0.head)(after);
              }
              ;
              if (otherwise) {
                return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 160, column 7 - line 162, column 52): " + []);
            }();
            $tco_var_pos = newPos;
            $tco_var_before = v.value0.tail;
            $copy_after = after;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parsing.String (line 156, column 36 - line 163, column 38): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
        }
        ;
        return $tco_result;
      };
    };
  };
  var satisfyCodePoint = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons4(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var $66 = f(v3.value0.head);
                if ($66) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), v3.value0.head);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 131, column 7 - line 138, column 73): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons4(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum4(v3.value0.head);
                var $75 = cp < 0 || cp > 65535;
                if ($75) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust4(toEnum2(cp));
                var $76 = f(ch);
                if ($76) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 109, column 7 - line 124, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var eof = /* @__PURE__ */ mkFn5(function(v) {
    return function(v1) {
      return function(v2) {
        return function($$throw2) {
          return function(done) {
            var $92 = $$null2(v.value0);
            if ($92) {
              return done(new ParseState(v.value0, v.value1, true), unit2);
            }
            ;
            return $$throw2(v, new ParseError("Expected EOF", v.value1));
          };
        };
      };
    };
  });
  var consumeWith = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = f(v.value0);
              if (v3 instanceof Left) {
                return $$throw2(v, new ParseError(v3.value0, v.value1));
              }
              ;
              if (v3 instanceof Right) {
                return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 297, column 7 - line 301, column 121): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var string = function(str2) {
    return consumeWith(function(input) {
      var v = stripPrefix(str2)(input);
      if (v instanceof Just) {
        return new Right({
          value: str2,
          consumed: str2,
          remainder: v.value0
        });
      }
      ;
      return new Left("Expected " + show4(str2));
    });
  };
  var $$char = function(c) {
    return withErrorMessage(satisfy(function(v) {
      return v === c;
    }))(show22(c));
  };

  // output/Parsing.String.Basic/index.js
  var elem1 = /* @__PURE__ */ elem2(eqChar);
  var show13 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(showChar));
  var notElem1 = /* @__PURE__ */ notElem2(eqChar);
  var satisfyCP = function(p) {
    return satisfy(function($30) {
      return p(codePointFromChar($30));
    });
  };
  var space = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isSpace))("space");
  var upper2 = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isUpper))("uppercase letter");
  var oneOf2 = function(ss) {
    return withLazyErrorMessage(satisfy(flip(elem1)(ss)))(function(v) {
      return "one of " + show13(ss);
    });
  };
  var octDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isOctDigit))("oct digit");
  var noneOf = function(ss) {
    return withLazyErrorMessage(satisfy(flip(notElem1)(ss)))(function(v) {
      return "none of " + show13(ss);
    });
  };
  var letter = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlpha))("letter");
  var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
  var digit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isDecDigit))("digit");
  var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

  // output/Data.String.Unicode/index.js
  var map17 = /* @__PURE__ */ map3(functorArray);
  var convert = function(f) {
    var $6 = map17(f);
    return function($7) {
      return fromCodePointArray($6(toCodePointArray($7)));
    };
  };
  var toLowerSimple2 = /* @__PURE__ */ convert(toLowerSimple);
  var toUpperSimple2 = /* @__PURE__ */ convert(toUpperSimple);

  // output/Parsing.Token/index.js
  var bind8 = /* @__PURE__ */ bind(bindParserT);
  var pure11 = /* @__PURE__ */ pure(applicativeParserT);
  var sort2 = /* @__PURE__ */ sort(ordString);
  var map18 = /* @__PURE__ */ map3(functorArray);
  var applySecond3 = /* @__PURE__ */ applySecond(applyParserT);
  var compare4 = /* @__PURE__ */ compare(ordString);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var fix2 = /* @__PURE__ */ fix(lazyParserT);
  var alt4 = /* @__PURE__ */ alt(altParserT);
  var $$void4 = /* @__PURE__ */ $$void(functorParserT);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorParserT);
  var identity16 = /* @__PURE__ */ identity4(categoryFn);
  var many3 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var map19 = /* @__PURE__ */ map3(functorMaybe);
  var some3 = /* @__PURE__ */ some(alternativeParserT)(lazyParserT);
  var foldl6 = /* @__PURE__ */ foldl(foldableArray);
  var applyFirst3 = /* @__PURE__ */ applyFirst(applyParserT);
  var show5 = /* @__PURE__ */ show(showString);
  var bind1 = /* @__PURE__ */ bind(bindMaybe);
  var pure12 = /* @__PURE__ */ pure(applicativeMaybe);
  var foldr5 = /* @__PURE__ */ foldr(foldableArray);
  var map23 = /* @__PURE__ */ map3(functorParserT);
  var choice3 = /* @__PURE__ */ choice(foldableArray);
  var many1 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var toUnfoldable8 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var foldr12 = /* @__PURE__ */ foldr(foldableList);
  var unGenLanguageDef = function(v) {
    return v;
  };
  var theReservedNames = function(v) {
    if (v.caseSensitive) {
      return sort2(v.reservedNames);
    }
    ;
    if (otherwise) {
      return sort2(map18(toLower)(v.reservedNames));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 825, column 1 - line 825, column 70): " + [v.constructor.name]);
  };
  var simpleSpace = /* @__PURE__ */ skipMany1(/* @__PURE__ */ satisfyCodePoint(isSpace));
  var oneLineComment = function(v) {
    return applySecond3($$try2(string(v.commentLine)))(skipMany(satisfy(function(v1) {
      return v1 !== "\n";
    })));
  };
  var isReserved = function($copy_names) {
    return function($copy_name) {
      var $tco_var_names = $copy_names;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(names, name3) {
        var v = uncons(names);
        if (v instanceof Nothing) {
          $tco_done = true;
          return false;
        }
        ;
        if (v instanceof Just) {
          var v1 = compare4(v.value0.head)(name3);
          if (v1 instanceof LT) {
            $tco_var_names = v.value0.tail;
            $copy_name = name3;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return true;
          }
          ;
          if (v1 instanceof GT) {
            $tco_done = true;
            return false;
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 820, column 35 - line 823, column 18): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 818, column 3 - line 823, column 18): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_names, $copy_name);
      }
      ;
      return $tco_result;
    };
  };
  var isReservedName = function(v) {
    return function(name3) {
      var caseName = function() {
        if (v.caseSensitive) {
          return name3;
        }
        ;
        if (otherwise) {
          return toLower(name3);
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 812, column 3 - line 814, column 31): " + []);
      }();
      return isReserved(theReservedNames(v))(caseName);
    };
  };
  var inCommentSingle = function(v) {
    var startEnd = append4(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix2(function(p) {
      return alt4($$void4($$try2(string(v.commentEnd))))(alt4(applySecond3(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond3(oneOf2(startEnd))(p))("end of comment")));
    });
  };
  var multiLineComment = function(v) {
    return applySecond3($$try2(string(v.commentStart)))(inComment(v));
  };
  var inCommentMulti = function(v) {
    var startEnd = append4(toCharArray(v.commentEnd))(toCharArray(v.commentStart));
    return fix2(function(p) {
      return alt4($$void4($$try2(string(v.commentEnd))))(alt4(applySecond3(multiLineComment(v))(p))(alt4(applySecond3(skipMany1(noneOf(startEnd)))(p))(withErrorMessage(applySecond3(oneOf2(startEnd))(p))("end of comment"))));
    });
  };
  var inComment = function(v) {
    if (v.nestedComments) {
      return inCommentMulti(v);
    }
    ;
    return inCommentSingle(v);
  };
  var whiteSpace$prime = function(v) {
    if ($$null2(v.commentLine) && $$null2(v.commentStart)) {
      return skipMany(withErrorMessage(simpleSpace)(""));
    }
    ;
    if ($$null2(v.commentLine)) {
      return skipMany(alt4(simpleSpace)(withErrorMessage(multiLineComment(v))("")));
    }
    ;
    if ($$null2(v.commentStart)) {
      return skipMany(alt4(simpleSpace)(withErrorMessage(oneLineComment(v))("")));
    }
    ;
    if (otherwise) {
      return skipMany(alt4(simpleSpace)(alt4(oneLineComment(v))(withErrorMessage(multiLineComment(v))(""))));
    }
    ;
    throw new Error("Failed pattern match at Parsing.Token (line 834, column 1 - line 834, column 74): " + [v.constructor.name]);
  };
  var makeTokenParser = function(v) {
    var stringLetter = satisfy(function(c) {
      return c !== '"' && (c !== "\\" && c > "");
    });
    var sign2 = function(dictRing) {
      return alt4(voidLeft3($$char("-"))(negate(dictRing)))(alt4(voidLeft3($$char("+"))(identity16))(pure11(identity16)));
    };
    var sign1 = sign2(ringInt);
    var oper = function() {
      var go = bind8(v.opStart)(function(c) {
        return bind8(many3(v.opLetter))(function(cs) {
          return pure11(singleton5(c) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go)("operator");
    }();
    var number4 = function(base) {
      return function(baseDigit) {
        var folder = function(v1) {
          return function(v2) {
            if (v1 instanceof Nothing) {
              return Nothing.value;
            }
            ;
            if (v1 instanceof Just) {
              return map19(function(v3) {
                return (base * v1.value0 | 0) + v3 | 0;
              })(hexDigitToInt(codePointFromChar(v2)));
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 704, column 5 - line 704, column 45): " + [v1.constructor.name, v2.constructor.name]);
          };
        };
        return bind8(some3(baseDigit))(function(digits) {
          return maybe(fail("not digits"))(pure11)(foldl6(folder)(new Just(0))(digits));
        });
      };
    };
    var octal = applySecond3(oneOf2(["o", "O"]))(number4(8)(octDigit));
    var lexeme = function(p) {
      return applyFirst3(p)(whiteSpace$prime(v));
    };
    var reservedOp = function(name3) {
      var go = bind8(string(name3))(function() {
        return withErrorMessage(notFollowedBy(v.opLetter))("end of " + name3);
      });
      return lexeme($$try2(go));
    };
    var symbol = function(name3) {
      return voidLeft3(lexeme(string(name3)))(name3);
    };
    var parens2 = function(p) {
      return between(symbol("("))(symbol(")"))(p);
    };
    var semi = symbol(";");
    var semiSep = function(p) {
      return sepBy(p)(semi);
    };
    var semiSep1 = function(p) {
      return sepBy1(p)(semi);
    };
    var isReservedOp = function(name3) {
      return isReserved(sort2(v.reservedOpNames))(name3);
    };
    var operator = function() {
      var go = bind8(oper)(function(name3) {
        var $113 = isReservedOp(name3);
        if ($113) {
          return fail("reserved operator " + name3);
        }
        ;
        return pure11(name3);
      });
      return lexeme($$try2(go));
    }();
    var ident2 = function() {
      var go = bind8(v.identStart)(function(c) {
        return bind8(many3(v.identLetter))(function(cs) {
          return pure11(singleton5(c) + fromCharArray(cs));
        });
      });
      return withErrorMessage(go)("identifier");
    }();
    var identifier = function() {
      var go = bind8(ident2)(function(name3) {
        var $114 = isReservedName(v)(name3);
        if ($114) {
          return fail("reserved word " + show5(name3));
        }
        ;
        return pure11(name3);
      });
      return lexeme($$try2(go));
    }();
    var hexadecimal2 = applySecond3(oneOf2(["x", "X"]))(number4(16)(hexDigit));
    var fraction = function() {
      var op = function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return Nothing.value;
          }
          ;
          if (v2 instanceof Just) {
            return bind1(hexDigitToInt(codePointFromChar(v1)))(function(int$prime) {
              return pure12((v2.value0 + toNumber(int$prime)) / 10);
            });
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 651, column 5 - line 651, column 47): " + [v1.constructor.name, v2.constructor.name]);
        };
      };
      return asErrorMessage("fraction")(bind8($$char("."))(function() {
        return bind8(withErrorMessage(some3(digit))("fraction"))(function(digits) {
          return maybe(fail("not digit"))(pure11)(foldr5(op)(new Just(0))(digits));
        });
      }));
    }();
    var escapeGap = withErrorMessage(applySecond3(some3(space))($$char("\\")))("end of string gap");
    var escapeEmpty = $$char("&");
    var escMap = zip(["a", "b", "f", "n", "r", "t", "v", "\\", '"', "'"])(["\x07", "\b", "\f", "\n", "\r", "	", "\v", "\\", '"', "'"]);
    var dot = symbol(".");
    var decimal = number4(10)(digit);
    var exponent$prime = function() {
      var power = function(e) {
        if (e < 0) {
          return 1 / power(-e | 0);
        }
        ;
        if (otherwise) {
          return pow(10)(toNumber(e));
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 664, column 5 - line 664, column 27): " + [e.constructor.name]);
      };
      return asErrorMessage("exponent")(bind8(oneOf2(["e", "E"]))(function() {
        return bind8(sign1)(function(f) {
          return bind8(withErrorMessage(decimal)("exponent"))(function(e) {
            return pure11(power(f(e)));
          });
        });
      }));
    }();
    var fractExponent = function(n) {
      var justExponent = bind8(exponent$prime)(function(expo) {
        return pure11(toNumber(n) * expo);
      });
      var fractExponent$prime = bind8(fraction)(function(fract) {
        return bind8(option(1)(exponent$prime))(function(expo) {
          return pure11((toNumber(n) + fract) * expo);
        });
      });
      return alt4(fractExponent$prime)(justExponent);
    };
    var fractFloat = function(n) {
      return map23(Right.create)(fractExponent(n));
    };
    var decimalFloat = bind8(decimal)(function(n) {
      return option(new Left(n))(fractFloat(n));
    });
    var zeroNumFloat = alt4(map23(Left.create)(alt4(hexadecimal2)(octal)))(alt4(decimalFloat)(alt4(fractFloat(0))(pure11(new Left(0)))));
    var natFloat = alt4(applySecond3($$char("0"))(zeroNumFloat))(decimalFloat);
    var naturalOrFloat = withErrorMessage(lexeme(natFloat))("number");
    var floating = bind8(decimal)(fractExponent);
    var $$float = withErrorMessage(lexeme(floating))("float");
    var zeroNumber = withErrorMessage(applySecond3($$char("0"))(alt4(hexadecimal2)(alt4(octal)(alt4(decimal)(pure11(0))))))("");
    var nat = alt4(zeroNumber)(decimal);
    var $$int = bind8(lexeme(sign1))(function(f) {
      return bind8(nat)(function(n) {
        return pure11(f(n));
      });
    });
    var integer = withErrorMessage(lexeme($$int))("integer");
    var natural = withErrorMessage(lexeme(nat))("natural");
    var comma2 = symbol(",");
    var commaSep = function(p) {
      return sepBy(p)(comma2);
    };
    var commaSep1 = function(p) {
      return sepBy1(p)(comma2);
    };
    var colon2 = symbol(":");
    var charNum = bind8(alt4(decimal)(alt4(applySecond3($$char("o"))(number4(8)(octDigit)))(applySecond3($$char("x"))(number4(16)(hexDigit)))))(function(code) {
      var $119 = code > 1114111;
      if ($119) {
        return fail("invalid escape sequence");
      }
      ;
      var v1 = fromCharCode2(code);
      if (v1 instanceof Just) {
        return pure11(v1.value0);
      }
      ;
      if (v1 instanceof Nothing) {
        return fail("invalid character code (should not happen)");
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 498, column 10 - line 500, column 67): " + [v1.constructor.name]);
    });
    var charLetter = satisfy(function(c) {
      return c !== "'" && (c !== "\\" && c > "");
    });
    var charEsc = function() {
      var parseEsc = function(v1) {
        return voidLeft3($$char(v1.value0))(v1.value1);
      };
      return choice3(map18(parseEsc)(escMap));
    }();
    var charControl = bind8($$char("^"))(function() {
      return bind8(upper2)(function(code) {
        var v1 = fromCharCode2((toCharCode2(code) - toCharCode2("A") | 0) + 1 | 0);
        if (v1 instanceof Just) {
          return pure11(v1.value0);
        }
        ;
        if (v1 instanceof Nothing) {
          return fail("invalid character code (should not happen)");
        }
        ;
        throw new Error("Failed pattern match at Parsing.Token (line 488, column 5 - line 490, column 67): " + [v1.constructor.name]);
      });
    });
    var caseString = function(name3) {
      if (v.caseSensitive) {
        return voidLeft3(string(name3))(name3);
      }
      ;
      if (otherwise) {
        var msg = show5(name3);
        var caseChar = function(c) {
          var v1 = function(v2) {
            if (otherwise) {
              return $$char(c);
            }
            ;
            throw new Error("Failed pattern match at Parsing.Token (line 355, column 1 - line 355, column 80): " + [c.constructor.name]);
          };
          var $130 = isAlpha(codePointFromChar(c));
          if ($130) {
            var $131 = toChar(toLowerSimple2(singleton5(c)));
            if ($131 instanceof Just) {
              var $132 = toChar(toUpperSimple2(singleton5(c)));
              if ($132 instanceof Just) {
                return alt4($$char($131.value0))($$char($132.value0));
              }
              ;
              return v1(true);
            }
            ;
            return v1(true);
          }
          ;
          return v1(true);
        };
        var walk = function(name$prime) {
          var v1 = uncons3(name$prime);
          if (v1 instanceof Nothing) {
            return pure11(unit2);
          }
          ;
          if (v1 instanceof Just) {
            return applySecond3(withErrorMessage(caseChar(v1.value0.head))(msg))(walk(v1.value0.tail));
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 757, column 22 - line 759, column 72): " + [v1.constructor.name]);
        };
        return voidLeft3(walk(name3))(name3);
      }
      ;
      throw new Error("Failed pattern match at Parsing.Token (line 751, column 3 - line 751, column 50): " + [name3.constructor.name]);
    };
    var reserved = function(name3) {
      var go = applySecond3(caseString(name3))(withErrorMessage(notFollowedBy(v.identLetter))("end of " + name3));
      return lexeme($$try2(go));
    };
    var brackets = function(p) {
      return between(symbol("["))(symbol("]"))(p);
    };
    var braces = function(p) {
      return between(symbol("{"))(symbol("}"))(p);
    };
    var ascii3codes = ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "SUB", "ESC", "DEL"];
    var ascii3 = ["\0", "", "", "", "", "", "", "\x07", "", "", "", "", "", "", "", "", "", "", "\x1B", "\x7F"];
    var ascii2codes = ["BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP"];
    var ascii2 = ["\b", "	", "\n", "\v", "\f", "\r", "", "", "", "", "", "", "", " "];
    var asciiMap = zip(append4(ascii3codes)(ascii2codes))(append4(ascii3)(ascii2));
    var charAscii = function() {
      var parseAscii = function(v1) {
        return $$try2(voidLeft3(string(v1.value0))(v1.value1));
      };
      return choice3(map18(parseAscii)(asciiMap));
    }();
    var escapeCode = alt4(charEsc)(alt4(charNum)(alt4(charAscii)(withErrorMessage(charControl)("escape code"))));
    var charEscape = applySecond3($$char("\\"))(escapeCode);
    var characterChar = alt4(charLetter)(withErrorMessage(charEscape)("literal character"));
    var charLiteral = function() {
      var go = between($$char("'"))(withErrorMessage($$char("'"))("end of character"))(characterChar);
      return withErrorMessage(lexeme(go))("character");
    }();
    var stringEscape = bind8($$char("\\"))(function() {
      return alt4(voidLeft3(escapeGap)(Nothing.value))(alt4(voidLeft3(escapeEmpty)(Nothing.value))(map23(Just.create)(escapeCode)));
    });
    var stringChar = alt4(map23(Just.create)(stringLetter))(withErrorMessage(stringEscape)("string character"));
    var stringLiteral = function() {
      var folder = function(v1) {
        return function(chars) {
          if (v1 instanceof Nothing) {
            return chars;
          }
          ;
          if (v1 instanceof Just) {
            return new Cons(v1.value0, chars);
          }
          ;
          throw new Error("Failed pattern match at Parsing.Token (line 455, column 5 - line 455, column 51): " + [v1.constructor.name, chars.constructor.name]);
        };
      };
      var go = bind8(between($$char('"'))(withErrorMessage($$char('"'))("end of string"))(many1(stringChar)))(function(maybeChars) {
        return pure11(fromCharArray(toUnfoldable8(foldr12(folder)(Nil.value)(maybeChars))));
      });
      return lexeme(withErrorMessage(go)("literal string"));
    }();
    var angles = function(p) {
      return between(symbol("<"))(symbol(">"))(p);
    };
    return {
      identifier,
      reserved,
      operator,
      reservedOp,
      charLiteral,
      stringLiteral,
      natural,
      integer,
      "float": $$float,
      naturalOrFloat,
      decimal,
      hexadecimal: hexadecimal2,
      octal,
      symbol,
      lexeme,
      whiteSpace: whiteSpace$prime(v),
      parens: parens2,
      braces,
      angles,
      brackets,
      semi,
      comma: comma2,
      colon: colon2,
      dot,
      semiSep,
      semiSep1,
      commaSep,
      commaSep1
    };
  };

  // output/Parsing.Language/index.js
  var alt5 = /* @__PURE__ */ alt(altParserT);
  var emptyDef = /* @__PURE__ */ function() {
    var op$prime = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    return {
      commentStart: "",
      commentEnd: "",
      commentLine: "",
      nestedComments: true,
      identStart: alt5(letter)($$char("_")),
      identLetter: alt5(alphaNum)(oneOf2(["_", "'"])),
      opStart: op$prime,
      opLetter: op$prime,
      reservedOpNames: [],
      reservedNames: [],
      caseSensitive: true
    };
  }();

  // output/Primitive.Parse/index.js
  var opDef = function(op) {
    return function(prec) {
      return function(assoc) {
        return new Tuple(op, {
          op,
          prec,
          assoc
        });
      };
    };
  };
  var opDefs = /* @__PURE__ */ function() {
    return fromFoldable3(ordString)(foldableArray)([opDef(".")(8)(AssocLeft.value), opDef("!")(8)(AssocLeft.value), opDef("**")(8)(AssocRight.value), opDef("*")(7)(AssocLeft.value), opDef("/")(7)(AssocLeft.value), opDef("+")(6)(AssocLeft.value), opDef("-")(6)(AssocLeft.value), opDef(":")(6)(AssocRight.value), opDef("++")(5)(AssocRight.value), opDef("==")(4)(AssocNone.value), opDef("/=")(4)(AssocNone.value), opDef("<")(4)(AssocLeft.value), opDef(">")(4)(AssocLeft.value), opDef("<=")(4)(AssocLeft.value), opDef(">=")(4)(AssocLeft.value)]);
  }();

  // output/SExpr/index.js
  var PEnd = /* @__PURE__ */ function() {
    function PEnd2() {
    }
    ;
    PEnd2.value = new PEnd2();
    return PEnd2;
  }();
  var PNext = /* @__PURE__ */ function() {
    function PNext2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PNext2.create = function(value0) {
      return function(value1) {
        return new PNext2(value0, value1);
      };
    };
    return PNext2;
  }();
  var PVar = /* @__PURE__ */ function() {
    function PVar2(value0) {
      this.value0 = value0;
    }
    ;
    PVar2.create = function(value0) {
      return new PVar2(value0);
    };
    return PVar2;
  }();
  var PConstr = /* @__PURE__ */ function() {
    function PConstr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PConstr2.create = function(value0) {
      return function(value1) {
        return new PConstr2(value0, value1);
      };
    };
    return PConstr2;
  }();
  var PRecord = /* @__PURE__ */ function() {
    function PRecord2(value0) {
      this.value0 = value0;
    }
    ;
    PRecord2.create = function(value0) {
      return new PRecord2(value0);
    };
    return PRecord2;
  }();
  var PListEmpty = /* @__PURE__ */ function() {
    function PListEmpty2() {
    }
    ;
    PListEmpty2.value = new PListEmpty2();
    return PListEmpty2;
  }();
  var PListNonEmpty = /* @__PURE__ */ function() {
    function PListNonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    PListNonEmpty2.create = function(value0) {
      return function(value1) {
        return new PListNonEmpty2(value0, value1);
      };
    };
    return PListNonEmpty2;
  }();
  var Var2 = /* @__PURE__ */ function() {
    function Var4(value0) {
      this.value0 = value0;
    }
    ;
    Var4.create = function(value0) {
      return new Var4(value0);
    };
    return Var4;
  }();
  var Op2 = /* @__PURE__ */ function() {
    function Op4(value0) {
      this.value0 = value0;
    }
    ;
    Op4.create = function(value0) {
      return new Op4(value0);
    };
    return Op4;
  }();
  var Int2 = /* @__PURE__ */ function() {
    function Int5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Int5.create = function(value0) {
      return function(value1) {
        return new Int5(value0, value1);
      };
    };
    return Int5;
  }();
  var Float2 = /* @__PURE__ */ function() {
    function Float5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Float5.create = function(value0) {
      return function(value1) {
        return new Float5(value0, value1);
      };
    };
    return Float5;
  }();
  var Str2 = /* @__PURE__ */ function() {
    function Str5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Str5.create = function(value0) {
      return function(value1) {
        return new Str5(value0, value1);
      };
    };
    return Str5;
  }();
  var Constr2 = /* @__PURE__ */ function() {
    function Constr5(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Constr5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Constr5(value0, value1, value2);
        };
      };
    };
    return Constr5;
  }();
  var Record2 = /* @__PURE__ */ function() {
    function Record5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Record5.create = function(value0) {
      return function(value1) {
        return new Record5(value0, value1);
      };
    };
    return Record5;
  }();
  var Matrix2 = /* @__PURE__ */ function() {
    function Matrix5(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Matrix5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Matrix5(value0, value1, value2, value3);
          };
        };
      };
    };
    return Matrix5;
  }();
  var Lambda2 = /* @__PURE__ */ function() {
    function Lambda4(value0) {
      this.value0 = value0;
    }
    ;
    Lambda4.create = function(value0) {
      return new Lambda4(value0);
    };
    return Lambda4;
  }();
  var Project2 = /* @__PURE__ */ function() {
    function Project4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Project4.create = function(value0) {
      return function(value1) {
        return new Project4(value0, value1);
      };
    };
    return Project4;
  }();
  var App3 = /* @__PURE__ */ function() {
    function App5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    App5.create = function(value0) {
      return function(value1) {
        return new App5(value0, value1);
      };
    };
    return App5;
  }();
  var BinaryApp = /* @__PURE__ */ function() {
    function BinaryApp2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    BinaryApp2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new BinaryApp2(value0, value1, value2);
        };
      };
    };
    return BinaryApp2;
  }();
  var MatchAs = /* @__PURE__ */ function() {
    function MatchAs2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MatchAs2.create = function(value0) {
      return function(value1) {
        return new MatchAs2(value0, value1);
      };
    };
    return MatchAs2;
  }();
  var IfElse = /* @__PURE__ */ function() {
    function IfElse2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    IfElse2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new IfElse2(value0, value1, value2);
        };
      };
    };
    return IfElse2;
  }();
  var ListEmpty = /* @__PURE__ */ function() {
    function ListEmpty2(value0) {
      this.value0 = value0;
    }
    ;
    ListEmpty2.create = function(value0) {
      return new ListEmpty2(value0);
    };
    return ListEmpty2;
  }();
  var ListNonEmpty = /* @__PURE__ */ function() {
    function ListNonEmpty2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    ListNonEmpty2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new ListNonEmpty2(value0, value1, value2);
        };
      };
    };
    return ListNonEmpty2;
  }();
  var ListEnum = /* @__PURE__ */ function() {
    function ListEnum2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ListEnum2.create = function(value0) {
      return function(value1) {
        return new ListEnum2(value0, value1);
      };
    };
    return ListEnum2;
  }();
  var ListComp = /* @__PURE__ */ function() {
    function ListComp2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    ListComp2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new ListComp2(value0, value1, value2);
        };
      };
    };
    return ListComp2;
  }();
  var Let2 = /* @__PURE__ */ function() {
    function Let4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Let4.create = function(value0) {
      return function(value1) {
        return new Let4(value0, value1);
      };
    };
    return Let4;
  }();
  var LetRec2 = /* @__PURE__ */ function() {
    function LetRec4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    LetRec4.create = function(value0) {
      return function(value1) {
        return new LetRec4(value0, value1);
      };
    };
    return LetRec4;
  }();
  var End = /* @__PURE__ */ function() {
    function End2(value0) {
      this.value0 = value0;
    }
    ;
    End2.create = function(value0) {
      return new End2(value0);
    };
    return End2;
  }();
  var Next = /* @__PURE__ */ function() {
    function Next2(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Next2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Next2(value0, value1, value2);
        };
      };
    };
    return Next2;
  }();
  var Guard = /* @__PURE__ */ function() {
    function Guard2(value0) {
      this.value0 = value0;
    }
    ;
    Guard2.create = function(value0) {
      return new Guard2(value0);
    };
    return Guard2;
  }();
  var Generator = /* @__PURE__ */ function() {
    function Generator2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Generator2.create = function(value0) {
      return function(value1) {
        return new Generator2(value0, value1);
      };
    };
    return Generator2;
  }();
  var Declaration = /* @__PURE__ */ function() {
    function Declaration2(value0) {
      this.value0 = value0;
    }
    ;
    Declaration2.create = function(value0) {
      return new Declaration2(value0);
    };
    return Declaration2;
  }();
  var VarDef2 = /* @__PURE__ */ function() {
    function VarDef4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    VarDef4.create = function(value0) {
      return function(value1) {
        return new VarDef4(value0, value1);
      };
    };
    return VarDef4;
  }();
  var Module2 = /* @__PURE__ */ function() {
    function Module3(value0) {
      this.value0 = value0;
    }
    ;
    Module3.create = function(value0) {
      return new Module3(value0);
    };
    return Module3;
  }();

  // output/Util.Parse/index.js
  var map20 = /* @__PURE__ */ map3(functorParserT);
  var map110 = /* @__PURE__ */ map3(functorFn);
  var some1 = /* @__PURE__ */ some2(alternativeParserT)(lazyParserT);
  var apply4 = /* @__PURE__ */ apply(applyParserT);
  var many4 = /* @__PURE__ */ many2(alternativeParserT)(lazyParserT);
  var applySecond4 = /* @__PURE__ */ applySecond(applyParserT);
  var alt6 = /* @__PURE__ */ alt(altParserT);
  var mapFlipped3 = /* @__PURE__ */ mapFlipped(functorParserT);
  var pure13 = /* @__PURE__ */ pure(applicativeParserT);
  var some4 = function(p) {
    return map20(map110(definitely$prime)(fromList))(some1(p));
  };
  var sepBy1_try = function(p) {
    return function(sep) {
      return apply4(map20(cons$prime)(p))(many4($$try2(applySecond4(sep)(p))));
    };
  };
  var sepBy_try = function(p) {
    return function(sep) {
      return alt6(mapFlipped3(sepBy1_try(p)(sep))(toList3))(pure13(Nil.value));
    };
  };

  // output/Parse/index.js
  var eq5 = /* @__PURE__ */ eq(eqInt);
  var comparing2 = /* @__PURE__ */ comparing(ordInt);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable(foldableList);
  var mapFlipped4 = /* @__PURE__ */ mapFlipped(functorList);
  var map21 = /* @__PURE__ */ map3(functorList);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable(foldableNonEmptyList);
  var map111 = /* @__PURE__ */ map3(functorArray);
  var alt7 = /* @__PURE__ */ alt(altParserT);
  var $$void5 = /* @__PURE__ */ $$void(functorParserT);
  var applyFirst4 = /* @__PURE__ */ applyFirst(applyParserT);
  var applySecond5 = /* @__PURE__ */ applySecond(applyParserT);
  var elem3 = /* @__PURE__ */ elem2(eqString);
  var bind9 = /* @__PURE__ */ bind(bindParserT);
  var lift24 = /* @__PURE__ */ lift2(applyParserT);
  var map24 = /* @__PURE__ */ map3(functorParserT);
  var mapFlipped1 = /* @__PURE__ */ mapFlipped(functorParserT);
  var pure14 = /* @__PURE__ */ pure(applicativeParserT);
  var apply5 = /* @__PURE__ */ apply(applyParserT);
  var fix3 = /* @__PURE__ */ fix(lazyParserT);
  var flap3 = /* @__PURE__ */ flap(functorParserT);
  var choose2 = /* @__PURE__ */ choose(altParserT);
  var pure15 = /* @__PURE__ */ pure(applicativeNonEmptyList);
  var empty7 = /* @__PURE__ */ empty3(plusList);
  var voidLeft4 = /* @__PURE__ */ voidLeft(functorParserT);
  var identity17 = /* @__PURE__ */ identity4(categoryFn);
  var foldr6 = /* @__PURE__ */ foldr(foldableList);
  var fanin2 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
  var append12 = /* @__PURE__ */ append(semigroupList);
  var str = {
    arrayLBracket: "[|",
    arrayRBracket: "|]",
    as: "as",
    backslash: "\\",
    backtick: "`",
    bar: "|",
    colon: ":",
    dot: ".",
    ellipsis: "..",
    else_: "else",
    equals: "=",
    fun: "fun",
    if_: "if",
    in_: "in",
    lArrow: "<-",
    lBracket: "[",
    let_: "let",
    match: "match",
    rArrow: "->",
    rBracket: "]",
    then_: "then"
  };
  var selState = false;
  var operators = function(binaryOp) {
    var ops = groupBy(on(eq5)(function(v) {
      return v.prec;
    }))(sortBy2(function(x2) {
      var $61 = comparing2(function(v) {
        return v.prec;
      })(x2);
      return function($62) {
        return invert($61($62));
      };
    })(values(opDefs)));
    return fromFoldable6(mapFlipped4(map21(fromFoldable13)(ops))(map111(function(v) {
      return new Infix($$try2(binaryOp(v.op)), v.assoc);
    })));
  };
  var languageDef = /* @__PURE__ */ function() {
    var opChar = oneOf2([":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~"]);
    var v = unGenLanguageDef(emptyDef);
    return {
      commentStart: "{-",
      commentEnd: "-}",
      commentLine: "--",
      nestedComments: true,
      identStart: alt7(letter)($$char("_")),
      identLetter: alt7(alphaNum)(oneOf2(["_", "'"])),
      opStart: opChar,
      opLetter: opChar,
      reservedNames: [str.as, str.else_, str.fun, str.if_, str.in_, str.let_, str.match, str.then_],
      reservedOpNames: [str.bar, str.ellipsis, str.equals, str.lArrow, str.rArrow],
      caseSensitive: true
    };
  }();
  var token = /* @__PURE__ */ makeTokenParser(languageDef);
  var rArrow = /* @__PURE__ */ function() {
    return token.reservedOp(str.rArrow);
  }();
  var rBracket = /* @__PURE__ */ function() {
    return $$void5(token.symbol(str.rBracket));
  }();
  var topLevel = function(p) {
    return applyFirst4(applySecond5(token.whiteSpace)(p))(eof);
  };
  var lBracket = /* @__PURE__ */ function() {
    return $$void5(token.symbol(str.lBracket));
  }();
  var lArrow = /* @__PURE__ */ function() {
    return token.reservedOp(str.lArrow);
  }();
  var keyword = function(str$prime) {
    var $48 = elem3(str$prime)(unGenLanguageDef(languageDef).reservedNames);
    if ($48) {
      return token.reserved(str$prime);
    }
    ;
    return error2(str$prime + " is not a reserved word");
  };
  var ident = /* @__PURE__ */ function() {
    return bind9(token.identifier)(function(x2) {
      return onlyIf(!isCtrName(x2))(monadPlusParserT)(x2);
    });
  }();
  var field = function(p) {
    return lift24(Tuple.create)(ident)(applySecond5(token.colon)(p));
  };
  var equals = /* @__PURE__ */ function() {
    return token.reservedOp(str.equals);
  }();
  var patternDelim = /* @__PURE__ */ alt7(rArrow)(equals);
  var ellipsis = /* @__PURE__ */ function() {
    return token.reservedOp(str.ellipsis);
  }();
  var ctr = /* @__PURE__ */ function() {
    return bind9(token.identifier)(function(x2) {
      return onlyIf(isCtrName(x2))(monadPlusParserT)(x2);
    });
  }();
  var simplePattern = function(pattern$prime) {
    var $$var = map24(PVar.create)(ident);
    var record2 = token.braces(mapFlipped1(sepBy(field(pattern$prime))(token.comma))(PRecord.create));
    var pair = token.parens(bind9(applyFirst4(pattern$prime)(token.comma))(function(\u03C0) {
      return bind9(pattern$prime)(function(\u03C0$prime) {
        return pure14(new PConstr(cPair, new Cons(\u03C0, new Cons(\u03C0$prime, Nil.value))));
      });
    }));
    var listNonEmpty = function() {
      var listRest = function(listRest$prime) {
        return alt7(applySecond5(rBracket)(pure14(PEnd.value)))(applySecond5(token.comma)(apply5(map24(PNext.create)(pattern$prime))(listRest$prime)));
      };
      return applySecond5(lBracket)(apply5(map24(PListNonEmpty.create)(pattern$prime))(fix3(listRest)));
    }();
    var listEmpty = token.brackets(pure14(PListEmpty.value));
    var constr2 = flap3(map24(PConstr.create)(ctr))(Nil.value);
    return alt7($$try2(listEmpty))(alt7(listNonEmpty)(alt7($$try2(constr2))(alt7($$try2(record2))(alt7($$try2($$var))(alt7($$try2(token.parens(pattern$prime)))(pair))))));
  };
  var pattern = /* @__PURE__ */ function() {
    var infixCtr = function(op) {
      return bind9(token.operator)(function(op$prime) {
        return onlyIf(isCtrOp(op$prime) && op === op$prime)(monadPlusParserT)(function(\u03C0) {
          return function(\u03C0$prime) {
            return new PConstr(op$prime, new Cons(\u03C0, new Cons(\u03C0$prime, Nil.value)));
          };
        });
      });
    };
    var appChain_pattern = function(pattern$prime) {
      var rest = function(v) {
        if (v instanceof PConstr) {
          var ctrArgs = bind9(simplePattern(pattern$prime))(function(\u03C0$prime) {
            return rest(new PConstr(v.value0, snoc2(v.value1)(\u03C0$prime)));
          });
          return alt7(ctrArgs)(pure14(v));
        }
        ;
        return pure14(v);
      };
      return bind9(simplePattern(pattern$prime))(rest);
    };
    return fix3(function() {
      var $63 = buildExprParser(operators(infixCtr));
      return function($64) {
        return $63(appChain_pattern($64));
      };
    }());
  }();
  var varDefs = function(expr$prime) {
    var clause = apply5(map24(VarDef2.create)(applyFirst4(pattern)(equals)))(expr$prime);
    return applySecond5(keyword(str.let_))(sepBy1_try(clause)(token.semi));
  };
  var branch_uncurried = function(expr$prime) {
    return function(delim) {
      return lift24(Tuple.create)(pattern)(applySecond5(delim)(expr$prime));
    };
  };
  var branch_curried = function(expr$prime) {
    return function(delim) {
      return lift24(Tuple.create)(some4(simplePattern(pattern)))(applySecond5(delim)(expr$prime));
    };
  };
  var recDefs = function(expr$prime) {
    var clause = lift24(Tuple.create)(ident)(branch_curried(expr$prime)(equals));
    return applySecond5(keyword(str.let_))(sepBy1_try(clause)(token.semi));
  };
  var defs = function(expr$prime) {
    return map24(singleton3)(choose2($$try2(varDefs(expr$prime)))(recDefs(expr$prime)));
  };
  var branchMany = function(expr$prime) {
    return function(branch_) {
      return token.braces(sepBy1(branch_(expr$prime)(rArrow))(token.semi));
    };
  };
  var branches = function(expr$prime) {
    return function(branch_) {
      return alt7(map24(pure15)(branch_(expr$prime)(patternDelim)))(branchMany(expr$prime)(branch_));
    };
  };
  var bar = /* @__PURE__ */ function() {
    return token.reservedOp(str.bar);
  }();
  var backtick = /* @__PURE__ */ function() {
    return $$void5(token.symbol(str.backtick));
  }();
  var expr_ = /* @__PURE__ */ function() {
    var binaryOp = function(op) {
      return bind9(token.operator)(function(op$prime) {
        return onlyIf(op === op$prime)(monadPlusParserT)(function() {
          var $53 = op === str.dot;
          if ($53) {
            return function(e) {
              return function(e$prime) {
                if (e$prime instanceof Var2) {
                  return new Project2(e, e$prime.value0);
                }
                ;
                return error2("Field names are not first class.");
              };
            };
          }
          ;
          var $56 = isCtrOp(op$prime);
          if ($56) {
            return function(e) {
              return function(e$prime) {
                return new Constr2(selState, op$prime, new Cons(e, new Cons(e$prime, empty7)));
              };
            };
          }
          ;
          return function(e) {
            return function(e$prime) {
              return new BinaryApp(e, op, e$prime);
            };
          };
        }());
      });
    };
    var backtickOp = flip(Infix.create)(AssocLeft.value)(bind9(between(backtick)(backtick)(ident))(function(x2) {
      return pure14(function(e) {
        return function(e$prime) {
          return new BinaryApp(e, x2, e$prime);
        };
      });
    }));
    var appChain = function(expr$prime) {
      var simpleExpr = function() {
        var variable = mapFlipped1(ident)(Var2.create);
        var string3 = map24(Str2.create(selState))(token.stringLiteral);
        var signOpt = function(dictRing) {
          return alt7(voidLeft4($$char("-"))(negate(dictRing)))(alt7(voidLeft4($$char("+"))(identity17))(pure14(identity17)));
        };
        var record2 = token.braces(mapFlipped1(sepBy(field(expr$prime))(token.comma))(Record2.create(selState)));
        var parensOp = map24(Op2.create)(token.parens(token.operator));
        var pair = token.parens(apply5(apply5(pure14(function(e) {
          return function(e$prime) {
            return new Constr2(selState, cPair, new Cons(e, new Cons(e$prime, empty7)));
          };
        }))(applyFirst4(expr$prime)(token.comma)))(expr$prime));
        var nil3 = token.brackets(pure14(new ListEmpty(selState)));
        var matrix = between(token.symbol(str.arrayLBracket))(token.symbol(str.arrayRBracket))(apply5(apply5(map24(Matrix2.create(selState))(applyFirst4(expr$prime)(bar)))(token.parens(lift24(Tuple.create)(ident)(applySecond5(token.comma)(ident)))))(applySecond5(keyword(str.in_))(expr$prime)));
        var matchAs = apply5(map24(MatchAs.create)(applyFirst4(applySecond5(keyword(str.match))(expr$prime))(keyword(str.as))))(branches(expr$prime)(branch_uncurried));
        var listNonEmpty = function() {
          var listRest = function(listRest$prime) {
            return alt7(applySecond5(rBracket)(pure14(new End(selState))))(applySecond5(token.comma)(apply5(map24(Next.create(selState))(expr$prime))(listRest$prime)));
          };
          return applySecond5(lBracket)(apply5(map24(ListNonEmpty.create(selState))(expr$prime))(fix3(listRest)));
        }();
        var listEnum = token.brackets(apply5(applyFirst4(apply5(pure14(ListEnum.create))(expr$prime))(ellipsis))(expr$prime));
        var listComp = function() {
          var qualifier = alt7(apply5(applyFirst4(map24(Generator.create)(pattern))(lArrow))(expr$prime))(alt7(map24(Declaration.create)(apply5(map24(VarDef2.create)(applyFirst4(applySecond5(keyword(str.let_))(pattern))(equals)))(expr$prime)))(map24(Guard.create)(expr$prime)));
          return token.brackets(apply5(applyFirst4(apply5(pure14(ListComp.create(selState)))(expr$prime))(bar))(sepBy1(qualifier)(token.comma)));
        }();
        var lambda = map24(Lambda2.create)(applySecond5(keyword(str.fun))(branches(expr$prime)(branch_curried)));
        var $$int = bind9(signOpt(ringInt))(function(sign2) {
          return map24(function() {
            var $65 = Int2.create(selState);
            return function($66) {
              return $65(sign2($66));
            };
          }())(token.natural);
        });
        var ifElse = apply5(applyFirst4(apply5(applyFirst4(apply5(pure14(IfElse.create))(applySecond5(keyword(str.if_))(expr$prime)))(keyword(str.then_)))(expr$prime))(keyword(str.else_)))(expr$prime);
        var $$float = bind9(signOpt(ringNumber))(function(sign2) {
          return map24(function() {
            var $67 = Float2.create(selState);
            return function($68) {
              return $67(sign2($68));
            };
          }())(token["float"]);
        });
        var defsExpr = bind9(map24(function($69) {
          return concat2(toList3($69));
        })(sepBy1(defs(expr$prime))(token.semi)))(function(defs$prime) {
          return flap3(map24(foldr6(function(def) {
            return fanin2(Let2.create)(LetRec2.create)(def);
          }))(applySecond5(keyword(str.in_))(expr$prime)))(defs$prime);
        });
        var constr2 = flap3(map24(Constr2.create(selState))(ctr))(empty7);
        return alt7(matrix)(alt7($$try2(nil3))(alt7(listNonEmpty)(alt7(listComp)(alt7(listEnum)(alt7($$try2(constr2))(alt7(record2)(alt7($$try2(variable))(alt7($$try2($$float))(alt7($$try2($$int))(alt7(string3)(alt7(defsExpr)(alt7(matchAs)(alt7($$try2(token.parens(expr$prime)))(alt7($$try2(parensOp))(alt7(pair)(alt7(lambda)(ifElse)))))))))))))))));
      }();
      var rest = function(v) {
        if (v instanceof Constr2) {
          var ctrArgs = bind9(simpleExpr)(function(e$prime) {
            return rest(new Constr2(v.value0, v.value1, append12(v.value2)(new Cons(e$prime, empty7))));
          });
          return alt7(ctrArgs)(pure14(v));
        }
        ;
        return alt7(bind9(map24(App3.create(v))(simpleExpr))(rest))(pure14(v));
      };
      return bind9(simpleExpr)(rest);
    };
    return fix3(function() {
      var $70 = buildExprParser(cons2([backtickOp])(operators(binaryOp)));
      return function($71) {
        return $70(appChain($71));
      };
    }());
  }();
  var module_ = /* @__PURE__ */ function() {
    return map24(function($72) {
      return Module2.create(concat2($72));
    })(topLevel(applyFirst4(sepBy_try(defs(expr_))(token.semi))(token.semi)));
  }();
  var program = /* @__PURE__ */ topLevel(expr_);

  // output/Text.Pretty/index.js
  var wrap2 = /* @__PURE__ */ wrap();
  var foldl7 = /* @__PURE__ */ foldl(foldableArray);
  var max4 = /* @__PURE__ */ max2(ordInt);
  var map25 = /* @__PURE__ */ map3(functorArray);
  var intercalate6 = /* @__PURE__ */ intercalate2(foldableArray)(monoidString);
  var voidLeft5 = /* @__PURE__ */ voidLeft(functorArray);
  var replicate3 = /* @__PURE__ */ replicate2(unfoldableArray);
  var append5 = /* @__PURE__ */ append(semigroupString);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var ala2 = /* @__PURE__ */ ala()()();
  var Columns = function(x2) {
    return x2;
  };
  var text = function(s) {
    var lines = split(wrap2("\n"))(s);
    return {
      width: foldl7(max4)(0)(map25(length5)(lines)),
      height: length(lines),
      lines
    };
  };
  var render = function(v) {
    return intercalate6("\n")(v.lines);
  };
  var empty8 = function(w) {
    return function(h) {
      return {
        width: w,
        height: h,
        lines: function() {
          if (h === 0) {
            return [];
          }
          ;
          return voidLeft5(range(1)(h))("");
        }()
      };
    };
  };
  var beside = function(v) {
    return function(v1) {
      var height_ = max4(v.height)(v1.height);
      var emptyLine = function(w) {
        return fromCharArray(replicate3(w)(" "));
      };
      var padRight = function(w) {
        return function(s) {
          return s + emptyLine(w - length5(s) | 0);
        };
      };
      var adjust = function(d) {
        return append13(d.lines)(replicate3(height_ - d.height | 0)(emptyLine(d.width)));
      };
      return {
        width: v.width + v1.width | 0,
        height: height_,
        lines: take(height_)(zipWith(append5)(map25(padRight(v.width))(adjust(v)))(adjust(v1)))
      };
    };
  };
  var semigroupColumns = {
    append: function(v) {
      return function(v1) {
        return beside(v)(v1);
      };
    }
  };
  var monoidColumns = {
    mempty: /* @__PURE__ */ empty8(0)(0),
    Semigroup0: function() {
      return semigroupColumns;
    }
  };
  var hcat = function(dictFoldable) {
    return ala2(Columns)(foldMap(dictFoldable)(monoidColumns));
  };
  var atop = function(v) {
    return function(v1) {
      return {
        width: max4(v.width)(v1.width),
        height: v.height + v1.height | 0,
        lines: append13(v.lines)(v1.lines)
      };
    };
  };

  // output/Val/index.js
  var $runtime_lazy7 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var map26 = /* @__PURE__ */ map3(functorMap);
  var map112 = /* @__PURE__ */ map3(functorList);
  var map27 = /* @__PURE__ */ map3(functorArray);
  var map34 = /* @__PURE__ */ map3(functorTuple);
  var map44 = /* @__PURE__ */ map3(functorElim);
  var map53 = /* @__PURE__ */ map3(functorEither);
  var join4 = /* @__PURE__ */ join2(joinSemilatticeBoolean);
  var mayFailEq5 = /* @__PURE__ */ mayFailEq(showInt)(eqInt);
  var mayFailEq13 = /* @__PURE__ */ mayFailEq(showNumber)(eqNumber);
  var mayFailEq23 = /* @__PURE__ */ mayFailEq(showString)(eqString);
  var slicesMap3 = /* @__PURE__ */ slicesMap(keyString);
  var apply6 = /* @__PURE__ */ apply(applyEither);
  var mayFailEq33 = /* @__PURE__ */ mayFailEq(showCtr)(eqCtr);
  var lift25 = /* @__PURE__ */ lift2(applyEither);
  var maybeJoin2 = /* @__PURE__ */ maybeJoin(/* @__PURE__ */ slicesMap3(slicesElimBoolean));
  var maybeJoin1 = /* @__PURE__ */ maybeJoin(slicesElimBoolean);
  var bot3 = /* @__PURE__ */ bot(boundedJoinSemilatticeBoo);
  var botOf2 = /* @__PURE__ */ botOf(boundedSlicesElimBoolean);
  var pop2 = /* @__PURE__ */ pop(ordString);
  var filterKeys2 = /* @__PURE__ */ filterKeys(ordString);
  var member3 = /* @__PURE__ */ member2(ordString);
  var lookup3 = /* @__PURE__ */ lookup(ordString);
  var mustLookup2 = /* @__PURE__ */ mustLookup(ordString);
  var append22 = /* @__PURE__ */ append(semigroupList);
  var toUnfoldable9 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var intersection3 = /* @__PURE__ */ intersection(ordString);
  var fv2 = /* @__PURE__ */ fv(fVElim);
  var union6 = /* @__PURE__ */ union2(ordString);
  var disjUnion2 = /* @__PURE__ */ disjUnion(ordString);
  var difference6 = /* @__PURE__ */ difference2(ordString);
  var joinSemilatticeMap2 = /* @__PURE__ */ joinSemilatticeMap(keyString);
  var not3 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean));
  var Int3 = /* @__PURE__ */ function() {
    function Int5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Int5.create = function(value0) {
      return function(value1) {
        return new Int5(value0, value1);
      };
    };
    return Int5;
  }();
  var Float3 = /* @__PURE__ */ function() {
    function Float5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Float5.create = function(value0) {
      return function(value1) {
        return new Float5(value0, value1);
      };
    };
    return Float5;
  }();
  var Str3 = /* @__PURE__ */ function() {
    function Str5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Str5.create = function(value0) {
      return function(value1) {
        return new Str5(value0, value1);
      };
    };
    return Str5;
  }();
  var Record3 = /* @__PURE__ */ function() {
    function Record5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Record5.create = function(value0) {
      return function(value1) {
        return new Record5(value0, value1);
      };
    };
    return Record5;
  }();
  var Constr3 = /* @__PURE__ */ function() {
    function Constr5(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Constr5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Constr5(value0, value1, value2);
        };
      };
    };
    return Constr5;
  }();
  var Matrix3 = /* @__PURE__ */ function() {
    function Matrix5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Matrix5.create = function(value0) {
      return function(value1) {
        return new Matrix5(value0, value1);
      };
    };
    return Matrix5;
  }();
  var Primitive = /* @__PURE__ */ function() {
    function Primitive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Primitive2.create = function(value0) {
      return function(value1) {
        return new Primitive2(value0, value1);
      };
    };
    return Primitive2;
  }();
  var Closure = /* @__PURE__ */ function() {
    function Closure2(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Closure2.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Closure2(value0, value1, value2, value3);
          };
        };
      };
    };
    return Closure2;
  }();
  var functorVal = {
    map: function(f) {
      return function(v) {
        if (v instanceof Int3) {
          return new Int3(f(v.value0), v.value1);
        }
        ;
        if (v instanceof Float3) {
          return new Float3(f(v.value0), v.value1);
        }
        ;
        if (v instanceof Str3) {
          return new Str3(f(v.value0), v.value1);
        }
        ;
        if (v instanceof Record3) {
          return new Record3(f(v.value0), map26(map3(functorVal)(f))(v.value1));
        }
        ;
        if (v instanceof Constr3) {
          return new Constr3(f(v.value0), v.value1, map112(map3(functorVal)(f))(v.value2));
        }
        ;
        if (v instanceof Matrix3) {
          return new Matrix3(f(v.value0), new Tuple(new Tuple(map27(map27(map3(functorVal)(f)))(v.value1.value0.value0), map34(f)(v.value1.value0.value1)), map34(f)(v.value1.value1)));
        }
        ;
        if (v instanceof Primitive) {
          return new Primitive(v.value0, map112(map3(functorVal)(f))(v.value1));
        }
        ;
        if (v instanceof Closure) {
          return new Closure(f(v.value0), map26(map3(functorVal)(f))(v.value1), map26(map44(f))(v.value2), map44(f)(v.value3));
        }
        ;
        throw new Error("Failed pattern match at Val (line 110, column 1 - line 119, column 90): " + [f.constructor.name, v.constructor.name]);
      };
    }
  };
  var slicesValBoolean = {
    maybeJoin: function(v) {
      return function(v1) {
        if (v instanceof Int3 && v1 instanceof Int3) {
          return map53(Int3.create(join4(v.value0)(v1.value0)))(mayFailEq5(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Float3 && v1 instanceof Float3) {
          return map53(Float3.create(join4(v.value0)(v1.value0)))(mayFailEq13(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Str3 && v1 instanceof Str3) {
          return map53(Str3.create(join4(v.value0)(v1.value0)))(mayFailEq23(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Record3 && v1 instanceof Record3) {
          return map53(Record3.create(join4(v.value0)(v1.value0)))(maybeJoin(slicesMap3(slicesValBoolean))(v.value1)(v1.value1));
        }
        ;
        if (v instanceof Constr3 && v1 instanceof Constr3) {
          return apply6(map53(Constr3.create(join4(v.value0)(v1.value0)))(mayFailEq33(v.value1)(v1.value1)))(maybeJoin(slicesList(slicesValBoolean))(v.value2)(v1.value2));
        }
        ;
        if (v instanceof Matrix3 && v1 instanceof Matrix3) {
          return map53(Matrix3.create(join4(v.value0)(v1.value0)))(lift25(Tuple.create)(lift25(Tuple.create)(maybeJoin(slicesArray(slicesArray(slicesValBoolean)))(v.value1.value0.value0)(v1.value1.value0.value0))(map53(flip(Tuple.create)(join4(v.value1.value0.value1.value1)(v1.value1.value0.value1.value1)))(mayFailEq5(v.value1.value0.value1.value0)(v1.value1.value0.value1.value0))))(map53(flip(Tuple.create)(join4(v.value1.value1.value1)(v1.value1.value1.value1)))(mayFailEq5(v.value1.value1.value0)(v1.value1.value1.value0))));
        }
        ;
        if (v instanceof Closure && v1 instanceof Closure) {
          return apply6(apply6(map53(Closure.create(join4(v.value0)(v1.value0)))(maybeJoin(slicesMap3(slicesValBoolean))(v.value1)(v1.value1)))(maybeJoin2(v.value2)(v1.value2)))(maybeJoin1(v.value3)(v1.value3));
        }
        ;
        if (v instanceof Primitive && v1 instanceof Primitive) {
          return map53(Primitive.create(v.value0))(maybeJoin(slicesList(slicesValBoolean))(v.value1)(v1.value1));
        }
        ;
        return report("Incompatible values");
      };
    },
    JoinSemilattice0: function() {
      return $lazy_joinSemilatticeValBoolean(0);
    }
  };
  var $lazy_joinSemilatticeValBoolean = /* @__PURE__ */ $runtime_lazy7("joinSemilatticeValBoolean", "Val", function() {
    return {
      join: definedJoin(slicesValBoolean),
      neg: map3(functorVal)(neg(joinSemilatticeBoolean))
    };
  });
  var joinSemilatticeValBoolean = /* @__PURE__ */ $lazy_joinSemilatticeValBoolean(121);
  var boundedSlicesValBoolean = {
    botOf: function(v) {
      if (v instanceof Int3) {
        return new Int3(bot3, v.value1);
      }
      ;
      if (v instanceof Float3) {
        return new Float3(bot3, v.value1);
      }
      ;
      if (v instanceof Str3) {
        return new Str3(bot3, v.value1);
      }
      ;
      if (v instanceof Record3) {
        return new Record3(bot3, map26(botOf(boundedSlicesValBoolean))(v.value1));
      }
      ;
      if (v instanceof Constr3) {
        return new Constr3(bot3, v.value1, map112(botOf(boundedSlicesValBoolean))(v.value2));
      }
      ;
      if (v instanceof Matrix3) {
        return new Matrix3(bot3, new Tuple(new Tuple(map27(map27(botOf(boundedSlicesValBoolean)))(v.value1.value0.value0), new Tuple(v.value1.value0.value1.value0, bot3)), new Tuple(v.value1.value1.value0, bot3)));
      }
      ;
      if (v instanceof Primitive) {
        return new Primitive(v.value0, map112(botOf(boundedSlicesValBoolean))(v.value1));
      }
      ;
      if (v instanceof Closure) {
        return new Closure(bot3, map26(botOf(boundedSlicesValBoolean))(v.value1), map26(botOf2)(v.value2), botOf2(v.value3));
      }
      ;
      throw new Error("Failed pattern match at Val (line 142, column 1 - line 151, column 87): " + [v.constructor.name]);
    },
    Slices0: function() {
      return slicesValBoolean;
    }
  };
  var updateMatrix = function(i) {
    return function(j) {
      return function(\u03B4v) {
        return function(v) {
          var vs_i = unsafeIndex4(v.value0.value0)(i - 1 | 0);
          var v_j = unsafeIndex4(vs_i)(j - 1 | 0);
          var vss$prime = unsafeUpdateAt(i - 1 | 0)(unsafeUpdateAt(j - 1 | 0)(\u03B4v(v_j))(vs_i))(v.value0.value0);
          return new Tuple(new Tuple(vss$prime, v.value0.value1), v.value1);
        };
      };
    };
  };
  var update2 = function(v) {
    return function(\u03B3) {
      if (v instanceof Nil) {
        if (isEmpty(\u03B3)) {
          return Nil.value;
        }
        ;
        if (otherwise) {
          return error2(absurd2);
        }
        ;
      }
      ;
      if (v instanceof Cons) {
        var v2 = pop2(v.value0.value0)(\u03B3);
        if (v2 instanceof Just) {
          return new Cons(new Tuple(v.value0.value0, v2.value0.value0), update2(v.value1)(v2.value0.value1));
        }
        ;
        if (v2 instanceof Nothing) {
          return new Cons(new Tuple(v.value0.value0, v.value0.value1), update2(v.value1)(\u03B3));
        }
        ;
        throw new Error("Failed pattern match at Val (line 54, column 4 - line 56, column 45): " + [v2.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Val (line 50, column 1 - line 50, column 65): " + [v.constructor.name, \u03B3.constructor.name]);
    };
  };
  var restrict = function(\u03B3) {
    return function(xs) {
      return filterKeys2(function(v) {
        return member3(v)(xs);
      })(\u03B3);
    };
  };
  var lookup$prime = function(x2) {
    return function(\u03B3) {
      return orElse("variable " + (x2 + " not found"))(lookup3(x2)(\u03B3));
    };
  };
  var dom2 = keys2;
  var reaches = function(\u03C1) {
    return function(xs) {
      var dom_\u03C1 = dom2(\u03C1);
      var go = function($copy_v) {
        return function($copy_acc) {
          var $tco_var_v = $copy_v;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v, acc) {
            if (v instanceof Nil) {
              $tco_done = true;
              return acc;
            }
            ;
            if (v instanceof Cons && member3(v.value0)(acc)) {
              $tco_var_v = v.value1;
              $copy_acc = acc;
              return;
            }
            ;
            if (v instanceof Cons && otherwise) {
              var \u03C3 = mustLookup2(v.value0)(\u03C1);
              $tco_var_v = append22(toUnfoldable9(intersection3(fv2(\u03C3))(dom_\u03C1)))(v.value1);
              $copy_acc = union6(singleton6(v.value0))(acc);
              return;
            }
            ;
            throw new Error("Failed pattern match at Val (line 74, column 4 - line 74, column 36): " + [v.constructor.name, acc.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_acc);
          }
          ;
          return $tco_result;
        };
      };
      return go(toUnfoldable9(xs))(empty5);
    };
  };
  var $$for = function(\u03C1) {
    return function(\u03C3) {
      return restrict(\u03C1)(reaches(\u03C1)(intersection3(fv2(\u03C3))(dom2(\u03C1))));
    };
  };
  var weakJoin = function(dictSlices) {
    var join12 = join2(joinSemilatticeMap2(dictSlices));
    return function(m) {
      return function(m$prime) {
        var v = new Tuple(dom2(m), dom2(m$prime));
        return disjUnion2(disjUnion2(restrict(m)(difference6(v.value0)(v.value1)))(join12(restrict(m)(intersection3(v.value0)(v.value1)))(restrict(m$prime)(intersection3(v.value0)(v.value1)))))(restrict(m$prime)(difference6(v.value1)(v.value0)));
      };
    };
  };
  var append_inv = function(xs) {
    return function(\u03B3) {
      return new Tuple(filterKeys2(function(v) {
        return not3(member3(v))(xs);
      })(\u03B3), restrict(\u03B3)(xs));
    };
  };
  var append6 = /* @__PURE__ */ unionWith(ordString)(/* @__PURE__ */ $$const(/* @__PURE__ */ identity4(categoryFn)));

  // output/Pretty/index.js
  var eq6 = /* @__PURE__ */ eq(eqCtr);
  var show6 = /* @__PURE__ */ show(showCtr);
  var hcat2 = /* @__PURE__ */ hcat(foldableList);
  var identity18 = /* @__PURE__ */ identity4(categoryFn);
  var map28 = /* @__PURE__ */ map3(functorList);
  var mapFlipped5 = /* @__PURE__ */ mapFlipped(functorList);
  var show14 = /* @__PURE__ */ show(showInt);
  var show23 = /* @__PURE__ */ show(showNumber);
  var show32 = /* @__PURE__ */ show(showString);
  var toUnfoldable10 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var map35 = /* @__PURE__ */ map3(functorArray);
  var prettyString = {
    pretty: text
  };
  var prettyPrimOp = {
    pretty: function(v) {
      return text("<prim op>");
    }
  };
  var space2 = /* @__PURE__ */ text(" ");
  var pretty = function(dict) {
    return dict.pretty;
  };
  var pretty1 = /* @__PURE__ */ pretty(prettyString);
  var pretty2 = /* @__PURE__ */ pretty(prettyPrimOp);
  var prettyP = function(dictPretty) {
    var $324 = pretty(dictPretty);
    return function($325) {
      return render($324($325));
    };
  };
  var prettyCtr = {
    pretty: function($328) {
      return pretty1(show6($328));
    }
  };
  var pretty3 = /* @__PURE__ */ pretty(prettyCtr);
  var $$null4 = /* @__PURE__ */ empty8(0)(0);
  var vert = function(dictFoldable) {
    var fromFoldable11 = fromFoldable2(dictFoldable);
    return function(delim) {
      var vert$prime = function(v) {
        if (v instanceof Nil) {
          return $$null4;
        }
        ;
        if (v instanceof Cons && v.value1 instanceof Nil) {
          return v.value0;
        }
        ;
        if (v instanceof Cons && v.value1 instanceof Cons) {
          return atop(beside(v.value0)(delim))(vert$prime(new Cons(v.value1.value0, v.value1.value1)));
        }
        ;
        throw new Error("Failed pattern match at Pretty (line 88, column 10 - line 88, column 34): " + [v.constructor.name]);
      };
      return function($329) {
        return vert$prime(fromFoldable11($329));
      };
    };
  };
  var vert2 = /* @__PURE__ */ vert(foldableArray);
  var nil2 = /* @__PURE__ */ function() {
    return text(str.lBracket + str.rBracket);
  }();
  var hspace = function(dictFoldable) {
    var $330 = intersperse(space2);
    var $331 = fromFoldable2(dictFoldable);
    return function($332) {
      return hcat2($330($331($332)));
    };
  };
  var hspace1 = /* @__PURE__ */ hspace(foldableArray);
  var hspace2 = /* @__PURE__ */ hspace(foldableList);
  var comma = /* @__PURE__ */ text(",");
  var hcomma = function(dictFoldable) {
    var $333 = intersperse(beside(comma)(space2));
    var $334 = fromFoldable2(dictFoldable);
    return function($335) {
      return hcat2($333($334($335)));
    };
  };
  var hcomma1 = /* @__PURE__ */ hcomma(foldableArray);
  var hcomma2 = /* @__PURE__ */ hcomma(foldableList);
  var colon = /* @__PURE__ */ function() {
    return text(str.colon);
  }();
  var between2 = function(l) {
    return function(r) {
      return function(doc) {
        return beside(beside(l)(doc))(r);
      };
    };
  };
  var highlightIf = function(v) {
    if (!v) {
      return identity18;
    }
    ;
    if (v) {
      return between2(text("_"))(text("_"));
    }
    ;
    throw new Error("Failed pattern match at Pretty (line 35, column 1 - line 35, column 35): " + [v.constructor.name]);
  };
  var parens = /* @__PURE__ */ between2(/* @__PURE__ */ text("("))(/* @__PURE__ */ text(")"));
  var prettyParensOpt = function(dictPretty) {
    var pretty6 = pretty(dictPretty);
    return function(x2) {
      var doc = pretty6(x2);
      var $148 = contains(" ")(render(doc));
      if ($148) {
        return parens(doc);
      }
      ;
      return doc;
    };
  };
  var prettyConstr = function(dictPretty) {
    var pretty6 = pretty(dictPretty);
    var prettyParensOpt1 = prettyParensOpt(dictPretty);
    return function(\u03B1) {
      return function(c) {
        return function(v) {
          if (v instanceof Cons && (v.value1 instanceof Cons && (v.value1.value1 instanceof Nil && eq6(c)(cPair)))) {
            return highlightIf(\u03B1)(parens(hcomma1([pretty6(v.value0), pretty6(v.value1.value0)])));
          }
          ;
          if (v instanceof Nil && eq6(c)(cNil)) {
            return highlightIf(\u03B1)(nil2);
          }
          ;
          if (v instanceof Cons && (v.value1 instanceof Cons && (v.value1.value1 instanceof Nil && eq6(c)(cCons)))) {
            return parens(hspace1([pretty6(v.value0), highlightIf(\u03B1)(text(":")), pretty6(v.value1.value0)]));
          }
          ;
          return hspace2(new Cons(highlightIf(\u03B1)(pretty3(c)), map28(prettyParensOpt1)(v)));
        };
      };
    };
  };
  var prettyRecord = function(dictPretty) {
    var pretty6 = pretty(dictPretty);
    return function(\u03B1) {
      return function(xvs) {
        return function() {
          var $336 = highlightIf(\u03B1);
          var $337 = between2(text("{"))(text("}"));
          return function($338) {
            return $336($337($338));
          };
        }()(hcomma2(mapFlipped5(xvs)(function(v) {
          return hspace1([beside(text(v.value0))(colon), pretty6(v.value1)]);
        })));
      };
    };
  };
  var prettyValBoolean = {
    pretty: function(v) {
      if (v instanceof Int3) {
        return highlightIf(v.value0)(text(show14(v.value1)));
      }
      ;
      if (v instanceof Float3) {
        return highlightIf(v.value0)(text(show23(v.value1)));
      }
      ;
      if (v instanceof Str3) {
        return highlightIf(v.value0)(text(show32(v.value1)));
      }
      ;
      if (v instanceof Record3) {
        return prettyRecord(prettyValBoolean)(v.value0)(toUnfoldable10(v.value1));
      }
      ;
      if (v instanceof Constr3) {
        return prettyConstr(prettyValBoolean)(v.value0)(v.value1)(v.value2);
      }
      ;
      if (v instanceof Matrix3) {
        return vert2(comma)(map35(function() {
          var $339 = map35(pretty(prettyValBoolean));
          return function($340) {
            return hcomma1($339($340));
          };
        }())(v.value1.value0.value0));
      }
      ;
      if (v instanceof Closure) {
        return text("<closure>");
      }
      ;
      if (v instanceof Primitive) {
        return parens(pretty2(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Pretty (line 156, column 1 - line 164, column 59): " + [v.constructor.name]);
    }
  };

  // output/Primitive/index.js
  var identity19 = /* @__PURE__ */ identity4(categoryFn);
  var prettyP2 = /* @__PURE__ */ prettyP(prettyValBoolean);
  var eq7 = /* @__PURE__ */ eq(eqCtr);
  var fanin3 = /* @__PURE__ */ fanin(categoryFn)(choiceFn);
  var toFromValBoolean = {
    constr: fst,
    constr_bwd: function(v) {
      return new Tuple(v, false);
    },
    match: function(v) {
      return new Tuple(v, true);
    }
  };
  var isZeroNumber = {
    isZero: /* @__PURE__ */ eq(eqNumber)(0)
  };
  var isZeroInt = {
    isZero: /* @__PURE__ */ eq(eqInt)(0)
  };
  var withInverse2 = function(fwd) {
    return {
      fwd,
      bwd: $$const(identity19)
    };
  };
  var withInverse1 = function(fwd) {
    return {
      fwd,
      bwd: $$const(identity19)
    };
  };
  var union1 = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Primitive (line 222, column 1 - line 222, column 71): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var match3 = function(dict) {
    return dict.match;
  };
  var match_fwd = function(dictToFrom) {
    return match3(dictToFrom);
  };
  var toFromArrayArrayValBoolea = {
    match: function(v) {
      if (v instanceof Matrix3) {
        return new Tuple(v.value1, v.value0);
      }
      ;
      return error2("Matrix expected; got " + prettyP2(v));
    },
    constr: function(v) {
      return new Matrix3(v.value1, v.value0);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromArrayArrayValBoolea)(v);
    }
  };
  var toFromBoolean = {
    match: function(v) {
      if (v instanceof Constr3 && v.value2 instanceof Nil) {
        if (eq7(v.value1)(cTrue)) {
          return new Tuple(true, v.value0);
        }
        ;
        if (eq7(v.value1)(cFalse)) {
          return new Tuple(false, v.value0);
        }
        ;
      }
      ;
      return error2("Boolean expected; got " + prettyP2(v));
    },
    constr: function(v) {
      if (v.value0) {
        return new Constr3(v.value1, cTrue, Nil.value);
      }
      ;
      if (!v.value0) {
        return new Constr3(v.value1, cFalse, Nil.value);
      }
      ;
      throw new Error("Failed pattern match at Primitive (line 111, column 1 - line 120, column 30): " + [v.constructor.name]);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromBoolean)(v);
    }
  };
  var toFromEitherEitherIntNumb = {
    constr: function(v) {
      if (v.value0 instanceof Left && v.value0.value0 instanceof Left) {
        return new Int3(v.value1, v.value0.value0.value0);
      }
      ;
      if (v.value0 instanceof Left && v.value0.value0 instanceof Right) {
        return new Float3(v.value1, v.value0.value0.value0);
      }
      ;
      if (v.value0 instanceof Right) {
        return new Str3(v.value1, v.value0.value0);
      }
      ;
      throw new Error("Failed pattern match at Primitive (line 71, column 1 - line 81, column 79): " + [v.constructor.name]);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromEitherEitherIntNumb)(v);
    },
    match: function(v) {
      if (v instanceof Int3) {
        return new Tuple(new Left(new Left(v.value1)), v.value0);
      }
      ;
      if (v instanceof Float3) {
        return new Tuple(new Left(new Right(v.value1)), v.value0);
      }
      ;
      if (v instanceof Str3) {
        return new Tuple(new Right(v.value1), v.value0);
      }
      ;
      return error2("Int, Float or Str expected; got " + prettyP2(v));
    }
  };
  var toFromInt = {
    match: function(v) {
      if (v instanceof Int3) {
        return new Tuple(v.value1, v.value0);
      }
      ;
      return error2("Int expected; got " + prettyP2(v));
    },
    constr: function(v) {
      return new Int3(v.value1, v.value0);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromInt)(v);
    }
  };
  var match1 = /* @__PURE__ */ match3(toFromInt);
  var toFromInt$plusNumber = {
    constr: function(v) {
      if (v.value0 instanceof Left) {
        return new Int3(v.value1, v.value0.value0);
      }
      ;
      if (v.value0 instanceof Right) {
        return new Float3(v.value1, v.value0.value0);
      }
      ;
      throw new Error("Failed pattern match at Primitive (line 61, column 1 - line 69, column 75): " + [v.constructor.name]);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromInt$plusNumber)(v);
    },
    match: function(v) {
      if (v instanceof Int3) {
        return new Tuple(new Left(v.value1), v.value0);
      }
      ;
      if (v instanceof Float3) {
        return new Tuple(new Right(v.value1), v.value0);
      }
      ;
      return error2("Int or Float expected; got " + prettyP2(v));
    }
  };
  var toFromMapVarValBoolean = {
    match: function(v) {
      if (v instanceof Record3) {
        return new Tuple(v.value1, v.value0);
      }
      ;
      return error2("Record expected; got " + prettyP2(v));
    },
    constr: function(v) {
      return new Record3(v.value1, v.value0);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromMapVarValBoolean)(v);
    }
  };
  var toFromNumber = {
    match: function(v) {
      if (v instanceof Float3) {
        return new Tuple(v.value1, v.value0);
      }
      ;
      return error2("Float expected; got " + prettyP2(v));
    },
    constr: function(v) {
      return new Float3(v.value1, v.value0);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromNumber)(v);
    }
  };
  var toFromString = {
    match: function(v) {
      if (v instanceof Str3) {
        return new Tuple(v.value1, v.value0);
      }
      ;
      return error2("Str expected; got " + prettyP2(v));
    },
    constr: function(v) {
      return new Str3(v.value1, v.value0);
    },
    constr_bwd: function(v) {
      return match_fwd(toFromString)(v);
    }
  };
  var unwrap6 = function(dictToFrom) {
    var $354 = match3(dictToFrom);
    return function($355) {
      return fst($354($355));
    };
  };
  var isZero = function(dict) {
    return dict.isZero;
  };
  var isZero$plus = function(dictIsZero) {
    var isZero1 = isZero(dictIsZero);
    return function(dictIsZero1) {
      return {
        isZero: fanin3(isZero1)(isZero(dictIsZero1))
      };
    };
  };
  var constr_bwd = function(dict) {
    return dict.constr_bwd;
  };
  var constr = function(dict) {
    return dict.constr;
  };
  var constr1 = /* @__PURE__ */ constr(toFromInt);
  var match_bwd = function(dictToFrom) {
    return constr(dictToFrom);
  };
  var unary_ = function(dictToFrom) {
    var match_fwd1 = match_fwd(dictToFrom);
    var match_bwd1 = match_bwd(dictToFrom);
    var unwrap1 = unwrap6(dictToFrom);
    var match22 = match3(dictToFrom);
    return function(dictToFrom1) {
      var constr2 = constr(dictToFrom1);
      var constr_bwd1 = constr_bwd(dictToFrom1);
      return function(v) {
        var apply_fwd = function() {
          return function(v1) {
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return constr2(v.fwd(match_fwd1(v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Primitive (line 165, column 4 - line 165, column 62): " + [v1.constructor.name]);
          };
        };
        var apply_fwd1 = apply_fwd();
        var apply_bwd = function() {
          return function(v1) {
            return function(v2) {
              if (v2 instanceof Cons && v2.value1 instanceof Nil) {
                return new Cons(match_bwd1(v.bwd(constr_bwd1(v1))(unwrap1(v2.value0))), Nil.value);
              }
              ;
              throw new Error("Failed pattern match at Primitive (line 168, column 4 - line 168, column 84): " + [v1.constructor.name, v2.constructor.name]);
            };
          };
        };
        var apply_bwd1 = apply_bwd();
        var apply10 = function() {
          return function(v1) {
            if (v1 instanceof Cons && v1.value1 instanceof Nil) {
              return constr2(v.fwd(match22(v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Primitive (line 162, column 4 - line 162, column 53): " + [v1.constructor.name]);
          };
        };
        var apply12 = apply10();
        return flip(Primitive.create)(Nil.value)({
          arity: 1,
          op: apply12,
          op_fwd: apply_fwd1,
          op_bwd: apply_bwd1
        });
      };
    };
  };
  var unary = function(dictToFrom) {
    var unary_1 = unary_(dictToFrom);
    return function(dictToFrom1) {
      var unary_2 = unary_1(dictToFrom1);
      return function(v) {
        var fwd$prime = function(v1) {
          return new Tuple(v.fwd(v1.value0), v1.value1);
        };
        var bwd$prime = function(v1) {
          return function(x2) {
            return new Tuple(v.bwd(v1.value0)(x2), v1.value1);
          };
        };
        return unary_2({
          fwd: fwd$prime,
          bwd: bwd$prime
        });
      };
    };
  };
  var toFromInt$215Boolean$215Int$215Boo = {
    constr: function(v) {
      return new Constr3(v.value1, cPair, new Cons(constr1(v.value0.value0), new Cons(constr1(v.value0.value1), Nil.value)));
    },
    constr_bwd: function(v) {
      return match_fwd(toFromInt$215Boolean$215Int$215Boo)(v);
    },
    match: function(v) {
      if (v instanceof Constr3 && (v.value2 instanceof Cons && (v.value2.value1 instanceof Cons && (v.value2.value1.value1 instanceof Nil && eq7(v.value1)(cPair))))) {
        return new Tuple(new Tuple(match1(v.value2.value0), match1(v.value2.value1.value0)), v.value0);
      }
      ;
      return error2("Pair expected; got " + prettyP2(v));
    }
  };
  var binary_ = function(dictToFrom) {
    var match_fwd1 = match_fwd(dictToFrom);
    var unwrap1 = unwrap6(dictToFrom);
    var match_bwd1 = match_bwd(dictToFrom);
    var match22 = match3(dictToFrom);
    return function(dictToFrom1) {
      var match_fwd22 = match_fwd(dictToFrom1);
      var unwrap22 = unwrap6(dictToFrom1);
      var match_bwd2 = match_bwd(dictToFrom1);
      var match32 = match3(dictToFrom1);
      return function(dictToFrom2) {
        var constr2 = constr(dictToFrom2);
        var constr_bwd1 = constr_bwd(dictToFrom2);
        return function(v) {
          var apply_fwd = function() {
            return function(v1) {
              if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
                return constr2(v.fwd(match_fwd1(v1.value0))(match_fwd22(v1.value1.value0)));
              }
              ;
              throw new Error("Failed pattern match at Primitive (line 182, column 4 - line 182, column 70): " + [v1.constructor.name]);
            };
          };
          var apply_fwd1 = apply_fwd();
          var apply_bwd = function() {
            return function(v1) {
              return function(v2) {
                if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
                  var v4 = v.bwd(constr_bwd1(v1))(new Tuple(unwrap1(v2.value0), unwrap22(v2.value1.value0)));
                  return new Cons(match_bwd1(v4.value0), new Cons(match_bwd2(v4.value1), Nil.value));
                }
                ;
                throw new Error("Failed pattern match at Primitive (line 185, column 4 - line 185, column 87): " + [v1.constructor.name, v2.constructor.name]);
              };
            };
          };
          var apply_bwd1 = apply_bwd();
          var apply10 = function() {
            return function(v1) {
              if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Nil)) {
                return constr2(v.fwd(match22(v1.value0))(match32(v1.value1.value0)));
              }
              ;
              throw new Error("Failed pattern match at Primitive (line 179, column 4 - line 179, column 56): " + [v1.constructor.name]);
            };
          };
          var apply12 = apply10();
          return flip(Primitive.create)(Nil.value)({
            arity: 2,
            op: apply12,
            op_fwd: apply_fwd1,
            op_bwd: apply_bwd1
          });
        };
      };
    };
  };
  var binaryZero = function(dictIsZero) {
    var isZero1 = isZero(dictIsZero);
    return function(dictToFrom) {
      var binary_1 = binary_(dictToFrom)(dictToFrom);
      return function(dictToFrom1) {
        var binary_2 = binary_1(dictToFrom1);
        return function(v) {
          var fwd$prime = function(v1) {
            return function(v2) {
              return new Tuple(v.fwd(v1.value0)(v2.value0), function() {
                var $301 = isZero1(v1.value0);
                if ($301) {
                  return v1.value1;
                }
                ;
                var $302 = isZero1(v2.value0);
                if ($302) {
                  return v2.value1;
                }
                ;
                return meet(v1.value1)(v2.value1);
              }());
            };
          };
          var bwd$prime = function(v1) {
            return function(v2) {
              var v3 = v.bwd(v1.value0)(new Tuple(v2.value0, v2.value1));
              var $310 = isZero1(v2.value0);
              if ($310) {
                return new Tuple(new Tuple(v3.value0, v1.value1), new Tuple(v3.value1, false));
              }
              ;
              var $311 = isZero1(v2.value1);
              if ($311) {
                return new Tuple(new Tuple(v3.value0, false), new Tuple(v3.value1, v1.value1));
              }
              ;
              return new Tuple(new Tuple(v3.value0, v1.value1), new Tuple(v3.value1, v1.value1));
            };
          };
          return binary_2({
            fwd: fwd$prime,
            bwd: bwd$prime
          });
        };
      };
    };
  };
  var binary = function(dictToFrom) {
    var binary_1 = binary_(dictToFrom);
    return function(dictToFrom1) {
      var binary_2 = binary_1(dictToFrom1);
      return function(dictToFrom2) {
        var binary_3 = binary_2(dictToFrom2);
        return function(v) {
          var fwd$prime = function(v1) {
            return function(v2) {
              return new Tuple(v.fwd(v1.value0)(v2.value0), meet(v1.value1)(v2.value1));
            };
          };
          var bwd$prime = function(v1) {
            return function(v2) {
              var v3 = v.bwd(v1.value0)(new Tuple(v2.value0, v2.value1));
              return new Tuple(new Tuple(v3.value0, v1.value1), new Tuple(v3.value1, v1.value1));
            };
          };
          return binary_3({
            fwd: fwd$prime,
            bwd: bwd$prime
          });
        };
      };
    };
  };
  var asNumberIntOrNumber = /* @__PURE__ */ function() {
    return {
      as: Right.create
    };
  }();
  var asIntOrNumberString = {
    as: function(v) {
      return error2("Non-uniform argument types");
    }
  };
  var asIntNumber = {
    as: toNumber
  };
  var asIntIntOrNumber = /* @__PURE__ */ function() {
    return {
      as: Left.create
    };
  }();
  var asBooleanBoolean = {
    as: identity19
  };
  var as = function(dict) {
    return dict.as;
  };
  var as1 = /* @__PURE__ */ as(asIntNumber);
  var asEither = {
    as: function(v) {
      if (v instanceof Left) {
        return as1(v.value0);
      }
      ;
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Primitive (line 253, column 1 - line 255, column 22): " + [v.constructor.name]);
    }
  };
  var union7 = function(dictAs) {
    var as22 = as(dictAs);
    return function(dictAs1) {
      var as3 = as(dictAs1);
      return function(dictAs2) {
        var as4 = as(dictAs2);
        return function(dictAs3) {
          var as5 = as(dictAs3);
          return function(v) {
            return function(v1) {
              return function(v2) {
                return function(v3) {
                  if (v2 instanceof Left && v3 instanceof Left) {
                    return as22(v(v2.value0)(v3.value0));
                  }
                  ;
                  if (v2 instanceof Left && v3 instanceof Right) {
                    return as3(v1(as4(v2.value0))(v3.value0));
                  }
                  ;
                  if (v2 instanceof Right && v3 instanceof Right) {
                    return as3(v1(v2.value0)(v3.value0));
                  }
                  ;
                  if (v2 instanceof Right && v3 instanceof Left) {
                    return as3(v1(v2.value0)(as5(v3.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Primitive (line 227, column 1 - line 228, column 73): " + [v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name]);
                };
              };
            };
          };
        };
      };
    };
  };
  var unionStr = function(dictAs) {
    var union23 = union7(dictAs)(dictAs);
    return function(dictAs1) {
      return union23(dictAs1)(dictAs1);
    };
  };

  // output/App.Util/index.js
  var update3 = /* @__PURE__ */ update(ordString);
  var eq8 = /* @__PURE__ */ eq(eqCtr);
  var bind10 = /* @__PURE__ */ bind(bindMaybe);
  var pure16 = /* @__PURE__ */ pure(applicativeMaybe);
  var neg3 = /* @__PURE__ */ neg(joinSemilatticeValBoolean);
  var match_fwd2 = /* @__PURE__ */ match_fwd(toFromMapVarValBoolean);
  var first2 = /* @__PURE__ */ first(strongFn);
  var as2 = /* @__PURE__ */ as(asEither);
  var toggleField = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Record3) {
          return new Record3(v2.value0, update3(function($156) {
            return Just.create(v1($156));
          })(v)(v2.value1));
        }
        ;
        return error2(absurd2);
      };
    };
  };
  var toggleConstrArg = function(v) {
    return function(v1) {
      return function(v2) {
        return function(v3) {
          if (v3 instanceof Constr3 && eq8(v)(v3.value1)) {
            return definitely$prime(bind10(index2(v3.value2)(v1))(function(u1) {
              return bind10(updateAt2(v1)(v2(u1))(v3.value2))(function(us$prime) {
                return pure16(new Constr3(v3.value0, v, us$prime));
              });
            }));
          }
          ;
          return error2(absurd2);
        };
      };
    };
  };
  var toggleCell = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Matrix3) {
          return new Matrix3(v2.value0, updateMatrix(v)(v1)(neg3)(new Tuple(new Tuple(v2.value1.value0.value0, new Tuple(v2.value1.value0.value1.value0, v2.value1.value0.value1.value1)), new Tuple(v2.value1.value1.value0, v2.value1.value1.value1))));
        }
        ;
        return error2(absurd2);
      };
    };
  };
  var selectNth = function(v) {
    return function(v1) {
      return function(v2) {
        if (v === 0 && (v2 instanceof Constr3 && (v2.value2 instanceof Cons && (v2.value2.value1 instanceof Cons && (v2.value2.value1.value1 instanceof Nil && eq8(v2.value1)(cCons)))))) {
          return new Constr3(v2.value0, v2.value1, new Cons(v1(v2.value2.value0), new Cons(v2.value2.value1.value0, Nil.value)));
        }
        ;
        if (v2 instanceof Constr3 && (v2.value2 instanceof Cons && (v2.value2.value1 instanceof Cons && (v2.value2.value1.value1 instanceof Nil && eq8(v2.value1)(cCons))))) {
          return new Constr3(v2.value0, v2.value1, new Cons(v2.value2.value0, new Cons(selectNth(v - 1 | 0)(v1)(v2.value2.value1.value0), Nil.value)));
        }
        ;
        return error2(absurd2);
      };
    };
  };
  var record = function(toRecord) {
    return function(u) {
      return toRecord(fst(match_fwd2(u)));
    };
  };
  var get3 = /* @__PURE__ */ mustLookup(ordString);
  var get_prim = function(dictToFrom) {
    var match_fwd1 = match_fwd(dictToFrom);
    return function(x2) {
      var $159 = get3(x2);
      return function($160) {
        return match_fwd1($159($160));
      };
    };
  };
  var get_prim1 = /* @__PURE__ */ get_prim(toFromInt$plusNumber);
  var get_intOrNumber = function(x2) {
    return function(r) {
      return first2(as2)(get_prim1(x2)(r));
    };
  };
  var from2 = function(dict) {
    return dict.from;
  };
  var reflectArray = {
    from: function() {
      return function(v) {
        if (v instanceof Constr3 && (v.value2 instanceof Nil && eq8(v.value1)(cNil))) {
          return [];
        }
        ;
        if (v instanceof Constr3 && (v.value2 instanceof Cons && (v.value2.value1 instanceof Cons && (v.value2.value1.value1 instanceof Nil && eq8(v.value1)(cCons))))) {
          return cons2(v.value2.value0)(from2(reflectArray)()(v.value2.value1.value0));
        }
        ;
        throw new Error("Failed pattern match at App.Util (line 47, column 1 - line 49, column 67): " + [v.constructor.name]);
      };
    }
  };
  var doNothing = /* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit2));

  // output/Web.Event.Event/foreign.js
  function _target(e) {
    return e.target;
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x2) {
    return x2;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.Event.Event/index.js
  var target = function($3) {
    return toMaybe(_target($3));
  };

  // output/App.BarChart/index.js
  var get_prim2 = /* @__PURE__ */ get_prim(toFromString);
  var map29 = /* @__PURE__ */ map3(functorArray);
  var from3 = /* @__PURE__ */ from2(reflectArray)();
  var neg4 = /* @__PURE__ */ neg(joinSemilatticeValBoolean);
  var reflectMapVarValBooleanBa = {
    from: function() {
      return function(r) {
        return {
          x: get_prim2(f_x)(r),
          y: get_intOrNumber(f_y)(r)
        };
      };
    }
  };
  var from1 = /* @__PURE__ */ from2(reflectMapVarValBooleanBa)();
  var reflectMapVarValBooleanBa1 = {
    from: function() {
      return function(r) {
        return {
          caption: get_prim2(f_caption)(r),
          data: map29(record(from1))(from3(get3(f_data)(r)))
        };
      };
    }
  };
  var barChartHandler = function(ev) {
    var unsafeBarIndex = function(tgt_opt) {
      var tgt = definitely$prime(tgt_opt);
      return unsafeIndex4(tgt["__data__"])(0);
    };
    var toggleBar = function(i) {
      return toggleConstrArg(cBarChart)(0)(toggleField(f_data)(selectNth(i)(neg4)));
    };
    return toggleBar(unsafeBarIndex(target(ev)));
  };

  // output/App.LineChart/foreign.js
  function curry42(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade2(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function max_y(linePlot) {
    return Math.max(...linePlot.data.map((point2) => point2.y.value0));
  }
  function min_x(linePlot) {
    return Math.min(...linePlot.data.map((point2) => point2.x.value0));
  }
  function max_x(linePlot) {
    return Math.max(...linePlot.data.map((point2) => point2.x.value0));
  }
  function drawLineChart_(id3, childIndex, {
    caption,
    plots
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const margin = { top: 15, right: 65, bottom: 40, left: 30 }, width = 230 - margin.left - margin.right, height = 185 - margin.top - margin.bottom, y_max = Math.max(...plots.map(max_y)), x_min = Math.min(...plots.map(min_x)), x_max = Math.max(...plots.map(max_x)), names = plots.map((plot) => plot.name.value0);
      const div4 = select_default2("#" + id3);
      div4.selectAll("#" + childId).remove();
      const svg = div4.append("svg").attr("width", width + margin.left + margin.right).attr("height", height + margin.top + margin.bottom).attr("id", childId).append("g").attr("transform", `translate(${margin.left}, ${margin.top})`);
      const x2 = linear2().domain([x_min, x_max]).range([0, width]), y2 = linear2().domain([0, y_max]).range([height, 0]);
      const line1 = line_default().x((d) => x2(d.x.value0)).y((d) => y2(d.y.value0));
      const color2 = ordinal(Pastel1_default);
      svg.selectAll("lines").data([...plots.entries()]).enter().append("g").append("path").attr("fill", "none").attr("stroke", ([, d]) => color2(names.indexOf(d.name.value0))).attr("stroke-width", 1).attr("class", "line").attr("d", ([_, d]) => line1(d.data));
      const smallRadius = 2;
      for (const n_plot of plots.entries()) {
        const [i, plot] = n_plot, col = color2(names.indexOf(plot.name.value0));
        svg.selectAll("markers").data([...plot.data.entries()].map(([j, ns]) => [[i, j], ns])).enter().append("g").append("circle").attr("r", ([, d]) => d.y.value1 ? smallRadius * 2 : smallRadius).attr("cx", ([, d]) => x2(d.x.value0)).attr("cy", ([, d]) => y2(d.y.value0)).attr("fill", col).attr("stroke", ([, d]) => d.y.value1 ? colorShade2(col, -30) : col).on("mousedown", (e, d) => {
          console.log(`mousedown ${d[0]}`);
          listener(e);
        });
      }
      svg.append("g").attr("transform", `translate(0, ${height})`).call(axisBottom(x2).ticks(x_max - x_min).tickFormat(format("d")));
      svg.append("g").call(axisLeft(y2).tickSizeOuter(0).ticks(3).tickFormat(format(".1f")));
      const legendLineHeight = 15, legendStart = width + margin.left / 2;
      svg.append("rect").attr("transform", `translate(${legendStart}, ${legendLineHeight * (names.length - 1) + 2})`).attr("x", 0).attr("y", 0).attr("stroke", "lightgray").attr("fill", "none").attr("height", legendLineHeight * names.length).attr("width", margin.right - 16);
      const legend = svg.selectAll("legend").data(names).enter().append("g").attr("class", "legend").attr(
        "transform",
        (d, i) => `translate(${legendStart}, ${height / 2 - margin.top + i * legendLineHeight})`
      );
      legend.append("text").text((d) => d).attr("font-size", 11).attr("transform", "translate(15, 9)");
      legend.append("circle").attr("fill", (d) => color2(names.indexOf(d))).attr("r", smallRadius).attr("cx", legendLineHeight / 2 - smallRadius / 2).attr("cy", legendLineHeight / 2 - smallRadius / 2);
      svg.append("text").text(caption.value0).attr("x", width / 2).attr("y", height + 35).attr("class", "title-text").attr("dominant-baseline", "bottom").attr("text-anchor", "middle");
    };
  }
  var drawLineChart = curry42(drawLineChart_);

  // output/App.LineChart/index.js
  var get_prim3 = /* @__PURE__ */ get_prim(toFromString);
  var map30 = /* @__PURE__ */ map3(functorArray);
  var from4 = /* @__PURE__ */ from2(reflectArray)();
  var eq9 = /* @__PURE__ */ eq(eqCtr);
  var neg5 = /* @__PURE__ */ neg(joinSemilatticeValBoolean);
  var reflectMapVarValBooleanPo = {
    from: function() {
      return function(r) {
        return {
          x: get_intOrNumber(f_x)(r),
          y: get_intOrNumber(f_y)(r)
        };
      };
    }
  };
  var from12 = /* @__PURE__ */ from2(reflectMapVarValBooleanPo)();
  var reflectMapVarValBooleanLi = {
    from: function() {
      return function(r) {
        return {
          name: get_prim3(f_name)(r),
          data: map30(record(from12))(from4(get3(f_data)(r)))
        };
      };
    }
  };
  var from22 = /* @__PURE__ */ from2(reflectMapVarValBooleanLi)();
  var reflectValBooleanLinePlot = {
    from: function() {
      return function(v) {
        if (v instanceof Constr3 && (v.value2 instanceof Cons && (v.value2.value1 instanceof Nil && eq9(v.value1)(cLinePlot)))) {
          return record(from22)(v.value2.value0);
        }
        ;
        throw new Error("Failed pattern match at App.LineChart (line 45, column 1 - line 46, column 66): " + [v.constructor.name]);
      };
    }
  };
  var from32 = /* @__PURE__ */ from2(reflectValBooleanLinePlot)();
  var reflectMapVarValBooleanLi1 = {
    from: function() {
      return function(r) {
        return {
          caption: get_prim3(f_caption)(r),
          plots: map30(from32)(from4(get3(f_plots)(r)))
        };
      };
    }
  };
  var lineChartHandler = function(ev) {
    var unsafePos = function(tgt_opt) {
      var tgt = definitely$prime(tgt_opt);
      var xy = unsafeIndex4(tgt["__data__"])(0);
      return new Tuple(unsafeIndex4(xy)(0), unsafeIndex4(xy)(1));
    };
    var togglePoint = function(v) {
      return toggleConstrArg(cLineChart)(0)(toggleField(f_plots)(selectNth(v.value0)(toggleConstrArg(cLinePlot)(0)(toggleField(f_data)(selectNth(v.value1)(neg5))))));
    };
    return togglePoint(unsafePos(target(ev)));
  };

  // output/App.MatrixView/foreign.js
  function curry43(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function drawMatrix_(id3, childIndex, {
    title: title2,
    matrix: { value0: { value0: nss, value1: i_max }, value1: j_max }
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const strokeWidth = 0.5;
      const w = 30, h = 30;
      const div4 = select_default2("#" + id3);
      const [width, height] = [w * j_max + strokeWidth, h * i_max + strokeWidth];
      const hMargin = w / 2;
      const vMargin = h / 2;
      div4.selectAll("#" + childId).remove();
      const svg = div4.append("svg").attr("id", childId).attr("width", width + hMargin).attr("height", height + vMargin);
      const grp = svg.selectAll("g").data([...nss.entries()].map(([i, ns]) => [i + 1, ns])).enter().append("g").attr(
        "transform",
        (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`
      );
      const rect = grp.selectAll("rect").data(([i, ns]) => [...ns.entries()].map(([j, n]) => [[i, j + 1], n])).enter();
      rect.append("rect").attr("x", (_, j) => w * j).attr("width", w).attr("height", h).attr("class", ([, n]) => n.value1 ? "matrix-cell-selected" : "matrix-cell-unselected").attr("stroke-width", strokeWidth);
      rect.append("text").text(([, n]) => n.value0).attr("x", (_, j) => w * (j + 0.5)).attr("y", 0.5 * h).attr("class", "matrix-cell-text").attr("text-anchor", "middle").attr("dominant-baseline", "middle").attr("pointer-events", "none");
      svg.append("text").text(title2).attr("x", hMargin / 2).attr("y", vMargin / 2).attr("class", "title-text").attr("dominant-baseline", "middle").attr("text-anchor", "left");
      svg.selectAll("rect").on("mousedown", (e, d) => {
        console.log(`mousedown ${d[0]}`);
        listener(e);
      });
    };
  }
  var drawMatrix = curry43(drawMatrix_);

  // output/App.MatrixView/index.js
  var map31 = /* @__PURE__ */ map3(functorArray);
  var match_fwd3 = /* @__PURE__ */ match_fwd(toFromInt);
  var matrixViewHandler = function(ev) {
    var unsafePos = function(tgt_opt) {
      var tgt = definitely$prime(tgt_opt);
      var xy = unsafeIndex4(tgt["__data__"])(0);
      return new Tuple(unsafeIndex4(xy)(0), unsafeIndex4(xy)(1));
    };
    return uncurry(toggleCell)(unsafePos(target(ev)));
  };
  var matrixRep = function(v) {
    return new Tuple(new Tuple(map31(map31(match_fwd3))(v.value0.value0), v.value0.value1.value0), v.value1.value0);
  };

  // output/App.TableView/foreign.js
  function curry44(f) {
    return (x1) => (x2) => (x3) => (x4) => f(x1, x2, x3, x4);
  }
  function colorShade3(col, amt) {
    col = col.replace(/^#/, "");
    if (col.length === 3)
      col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2];
    let [r, g, b] = col.match(/.{2}/g);
    [r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt];
    r = Math.max(Math.min(255, r), 0).toString(16);
    g = Math.max(Math.min(255, g), 0).toString(16);
    b = Math.max(Math.min(255, b), 0).toString(16);
    const rr = (r.length < 2 ? "0" : "") + r;
    const gg = (g.length < 2 ? "0" : "") + g;
    const bb = (b.length < 2 ? "0" : "") + b;
    return `#${rr}${gg}${bb}`;
  }
  function isUsed(r) {
    return Object.keys(r).some((k) => r[k].value1);
  }
  function drawTable_(id3, childIndex, {
    title: title2,
    table
  }, listener) {
    return () => {
      const childId = id3 + "-" + childIndex;
      const cellFill = "#ffffff";
      const div4 = select_default2("#" + id3);
      div4.selectAll("#" + childId).remove();
      table = table.filter((r) => isUsed(r));
      if (table.length > 0) {
        const HTMLtable = div4.append("table").attr("id", childId);
        const colNames = Object.keys(table[0]);
        HTMLtable.append("thead").append("tr").selectAll("th").data(colNames).enter().append("th").text((d) => d);
        const rows = HTMLtable.append("tbody").selectAll("tr").data(table).enter().append("tr");
        rows.selectAll("td").data((d) => colNames.map((k) => {
          return { "value": d[k], "name": k };
        })).enter().append("td").attr("data-th", (d) => d.name).attr("class", (d) => d.value.value1 ? "cell-selected" : null).attr("bgcolor", (d) => d.value.value1 ? colorShade3(cellFill, -40) : cellFill).text((d) => d.value.value0).on(
          "mouseover",
          (e, d) => listener(e)
        );
      }
    };
  }
  var drawTable = curry44(drawTable_);

  // output/App.TableView/index.js
  var get_prim4 = /* @__PURE__ */ get_prim(toFromInt);
  var get_prim12 = /* @__PURE__ */ get_prim(toFromString);
  var tableViewHandler = /* @__PURE__ */ $$const(/* @__PURE__ */ identity4(categoryFn));
  var energyRecord = function(r) {
    return {
      year: get_prim4("year")(r),
      country: get_prim12("country")(r),
      energyType: get_prim12("energyType")(r),
      output: get_intOrNumber("output")(r)
    };
  };

  // output/DesugarFwd/index.js
  var map36 = /* @__PURE__ */ map3(functorList);
  var difference7 = /* @__PURE__ */ difference(eqCtr);
  var toUnfoldable11 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var dataTypeFor3 = /* @__PURE__ */ dataTypeFor(dataTypeForCtr);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable3(ordCtr);
  var fromFoldable14 = /* @__PURE__ */ fromFoldable7(foldableList);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable7(foldableArray);
  var pure17 = /* @__PURE__ */ pure(applicativeEither);
  var bind11 = /* @__PURE__ */ bind(bindEither);
  var applySecond6 = /* @__PURE__ */ applySecond(applyEither);
  var map113 = /* @__PURE__ */ map3(functorEither);
  var map210 = /* @__PURE__ */ map3(functorFn);
  var compare5 = /* @__PURE__ */ compare(ordString);
  var apply7 = /* @__PURE__ */ apply(applyEither);
  var eq10 = /* @__PURE__ */ eq(eqString);
  var traverse2 = /* @__PURE__ */ traverse(traversableNonEmptyList)(applicativeEither);
  var map37 = /* @__PURE__ */ map3(functorNonEmptyList);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEither);
  var traverse12 = /* @__PURE__ */ traverse(traversableList)(applicativeEither);
  var fromFoldable32 = /* @__PURE__ */ fromFoldable3(ordString)(foldableList);
  var traverse22 = /* @__PURE__ */ traverse(traversableTuple)(applicativeEither);
  var flap4 = /* @__PURE__ */ flap(functorEither);
  var eq12 = /* @__PURE__ */ eq(eqCtr);
  var foldM5 = /* @__PURE__ */ foldM(foldableList)(monadEither);
  var maybeJoin3 = /* @__PURE__ */ maybeJoin(slicesElimBoolean);
  var pure18 = /* @__PURE__ */ pure(applicativeList);
  var join5 = /* @__PURE__ */ join(bindList);
  var enil = function(\u03B1) {
    return new Constr(\u03B1, cNil, Nil.value);
  };
  var totaliseConstrFwd = function(v) {
    return function(\u03B1) {
      var defaultBranch = function(c$prime) {
        return new Tuple(c$prime, applyN(function() {
          var $225 = ElimVar.create(varAnon);
          return function($226) {
            return ContElim.create($225($226));
          };
        }())(successful(arity(c$prime)))(new ContExpr(enil(\u03B1))));
      };
      var c\u03BAs = map36(defaultBranch)(difference7(toUnfoldable11(ctrs(successful(dataTypeFor3(v.value0)))))(singleton3(v.value0)));
      return fromFoldable14(new Cons(new Tuple(v.value0, v.value1), c\u03BAs));
    };
  };
  var totaliseFwd = function(v) {
    return function(v1) {
      if (v instanceof ContNone) {
        return error2(absurd2);
      }
      ;
      if (v instanceof ContExpr) {
        return new ContExpr(v.value0);
      }
      ;
      if (v instanceof ContElim && v.value0 instanceof ElimConstr) {
        var v2 = asSingletonMap(v.value0.value0);
        return new ContElim(new ElimConstr(totaliseConstrFwd(new Tuple(v2.value0, totaliseFwd(v2.value1)(v1)))(v1)));
      }
      ;
      if (v instanceof ContElim && v.value0 instanceof ElimRecord) {
        return new ContElim(new ElimRecord(v.value0.value0, totaliseFwd(v.value0.value1)(v1)));
      }
      ;
      if (v instanceof ContElim && v.value0 instanceof ElimVar) {
        return new ContElim(new ElimVar(v.value0.value0, totaliseFwd(v.value0.value1)(v1)));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 161, column 1 - line 161, column 37): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var elimBool = function(\u03BA) {
    return function(\u03BA$prime) {
      return new ElimConstr(fromFoldable23([new Tuple(cTrue, \u03BA), new Tuple(cFalse, \u03BA$prime)]));
    };
  };
  var econs = function(\u03B1) {
    return function(e) {
      return function(e$prime) {
        return new Constr(\u03B1, cCons, new Cons(e, new Cons(e$prime, Nil.value)));
      };
    };
  };
  var recordPatternFwd = function(v) {
    return function(\u03BA) {
      if (v instanceof Nil) {
        return pure17(\u03BA);
      }
      ;
      if (v instanceof Cons) {
        return bind11(patternFwd(v.value0.value1)(\u03BA))(function() {
          var $227 = recordPatternFwd(v.value1);
          return function($228) {
            return $227(ContElim.create($228));
          };
        }());
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 144, column 1 - line 144, column 70): " + [v.constructor.name, \u03BA.constructor.name]);
    };
  };
  var patternFwd = function(v) {
    return function(\u03BA) {
      if (v instanceof PVar) {
        return pure17(new ElimVar(v.value0, \u03BA));
      }
      ;
      if (v instanceof PConstr) {
        return applySecond6(checkArity(v.value0)(length3(v.value1)))(map113(map210(ElimConstr.create)(singleton4(v.value0)))(argPatternFwd(map36(Left.create)(v.value1))(\u03BA)));
      }
      ;
      if (v instanceof PRecord) {
        return map113(ElimRecord.create(dom(v.value0)))(recordPatternFwd(sortBy2(on(flip(compare5))(fst))(v.value0))(\u03BA));
      }
      ;
      if (v instanceof PListEmpty) {
        return pure17(new ElimConstr(singleton4(cNil)(\u03BA)));
      }
      ;
      if (v instanceof PListNonEmpty) {
        return map113(map210(ElimConstr.create)(singleton4(cCons)))(argPatternFwd(new Cons(new Left(v.value0), new Cons(new Right(v.value1), Nil.value)))(\u03BA));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 126, column 1 - line 126, column 52): " + [v.constructor.name, \u03BA.constructor.name]);
    };
  };
  var listRestPatternFwd = function(v) {
    return function(\u03BA) {
      if (v instanceof PEnd) {
        return pure17(new ElimConstr(singleton4(cNil)(\u03BA)));
      }
      ;
      if (v instanceof PNext) {
        return map113(map210(ElimConstr.create)(singleton4(cCons)))(argPatternFwd(new Cons(new Left(v.value0), new Cons(new Right(v.value1), Nil.value)))(\u03BA));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 135, column 1 - line 135, column 68): " + [v.constructor.name, \u03BA.constructor.name]);
    };
  };
  var argPatternFwd = function(v) {
    return function(\u03BA) {
      if (v instanceof Nil) {
        return pure17(\u03BA);
      }
      ;
      if (v instanceof Cons && v.value0 instanceof Left) {
        return map113(ContElim.create)(bind11(argPatternFwd(v.value1)(\u03BA))(patternFwd(v.value0.value0)));
      }
      ;
      if (v instanceof Cons && v.value0 instanceof Right) {
        return map113(ContElim.create)(bind11(argPatternFwd(v.value1)(\u03BA))(listRestPatternFwd(v.value0.value0)));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 139, column 1 - line 139, column 80): " + [v.constructor.name, \u03BA.constructor.name]);
    };
  };
  var varDefsFwd = function(v) {
    if (v.value0.value1 instanceof Nil) {
      return apply7(map113(Let.create)(varDefFwd(v.value0.value0)))(exprFwd(v.value1));
    }
    ;
    if (v.value0.value1 instanceof Cons) {
      return apply7(map113(Let.create)(varDefFwd(v.value0.value0)))(varDefsFwd(new Tuple(new NonEmpty(v.value0.value1.value0, v.value0.value1.value1), v.value1)));
    }
    ;
    throw new Error("Failed pattern match at DesugarFwd (line 54, column 1 - line 54, column 55): " + [v.constructor.name]);
  };
  var varDefFwd = function(v) {
    return apply7(map113(VarDef.create)(patternFwd(v.value0)(ContNone.value)))(exprFwd(v.value1));
  };
  var recDefsFwd = function(xcs) {
    var xcss = groupBy2(on(eq10)(fst))(xcs);
    return map113(toList3)(traverse2(recDefFwd)(xcss));
  };
  var recDefFwd = function(xcs) {
    return map113(function(v) {
      return new Tuple(fst(head3(xcs)), v);
    })(branchesFwd_curried(map37(snd)(xcs)));
  };
  var patternsFwd = function(v) {
    if (v.value0.value1 instanceof Nil) {
      return branchFwd_uncurried(v.value0.value0)(v.value1);
    }
    ;
    if (v.value0.value1 instanceof Cons) {
      return bindFlipped4(patternFwd(v.value0.value0))(map113(map210(ContExpr.create)(Lambda.create))(patternsFwd(new Tuple(new NonEmpty(v.value0.value1.value0, v.value0.value1.value1), v.value1))));
    }
    ;
    throw new Error("Failed pattern match at DesugarFwd (line 121, column 1 - line 121, column 65): " + [v.constructor.name]);
  };
  var listRestFwd = function(v) {
    if (v instanceof End) {
      return pure17(enil(v.value0));
    }
    ;
    if (v instanceof Next) {
      return apply7(map113(econs(v.value0))(exprFwd(v.value1)))(listRestFwd(v.value2));
    }
    ;
    throw new Error("Failed pattern match at DesugarFwd (line 116, column 1 - line 116, column 48): " + [v.constructor.name]);
  };
  var exprFwd = function(v) {
    if (v instanceof Var2) {
      return pure17(new Var(v.value0));
    }
    ;
    if (v instanceof Op2) {
      return pure17(new Op(v.value0));
    }
    ;
    if (v instanceof Int2) {
      return pure17(new Int(v.value0, v.value1));
    }
    ;
    if (v instanceof Float2) {
      return pure17(new Float(v.value0, v.value1));
    }
    ;
    if (v instanceof Str2) {
      return pure17(new Str(v.value0, v.value1));
    }
    ;
    if (v instanceof Constr2) {
      return map113(Constr.create(v.value0)(v.value1))(traverse12(exprFwd)(v.value2));
    }
    ;
    if (v instanceof Record2) {
      return map113(map210(Record.create(v.value0))(fromFoldable32))(traverse12(traverse22(exprFwd))(v.value1));
    }
    ;
    if (v instanceof Matrix2) {
      return apply7(flap4(map113(Matrix.create(v.value0))(exprFwd(v.value1)))(new Tuple(v.value2.value0, v.value2.value1)))(exprFwd(v.value3));
    }
    ;
    if (v instanceof Lambda2) {
      return map113(Lambda.create)(branchesFwd_curried(v.value0));
    }
    ;
    if (v instanceof Project2) {
      return flap4(map113(Project.create)(exprFwd(v.value0)))(v.value1);
    }
    ;
    if (v instanceof App3) {
      return apply7(map113(App2.create)(exprFwd(v.value0)))(exprFwd(v.value1));
    }
    ;
    if (v instanceof BinaryApp) {
      return apply7(map113(App2.create)(map113(App2.create(new Op(v.value1)))(exprFwd(v.value0))))(exprFwd(v.value2));
    }
    ;
    if (v instanceof MatchAs) {
      return apply7(map113(App2.create)(map113(Lambda.create)(branchesFwd_uncurried(v.value1))))(exprFwd(v.value0));
    }
    ;
    if (v instanceof IfElse) {
      return bind11(exprFwd(v.value1))(function(e22) {
        return bind11(exprFwd(v.value2))(function(e3) {
          return map113(App2.create(new Lambda(elimBool(new ContExpr(e22))(new ContExpr(e3)))))(exprFwd(v.value0));
        });
      });
    }
    ;
    if (v instanceof ListEmpty) {
      return pure17(enil(v.value0));
    }
    ;
    if (v instanceof ListNonEmpty) {
      return apply7(map113(econs(v.value0))(exprFwd(v.value1)))(listRestFwd(v.value2));
    }
    ;
    if (v instanceof ListEnum) {
      return apply7(map113(App2.create)(map113(App2.create(new Var("enumFromTo")))(exprFwd(v.value0))))(exprFwd(v.value1));
    }
    ;
    if (v instanceof ListComp && (v.value2.value0 instanceof Guard && (v.value2.value0.value0 instanceof Constr2 && (v.value2.value0.value0.value2 instanceof Nil && (v.value2.value1 instanceof Nil && eq12(v.value2.value0.value0.value1)(cTrue)))))) {
      return flap4(map113(econs(v.value2.value0.value0.value0))(exprFwd(v.value1)))(enil(v.value2.value0.value0.value0));
    }
    ;
    if (v instanceof ListComp && v.value2.value1 instanceof Nil) {
      return exprFwd(new ListComp(v.value0, v.value1, new NonEmpty(v.value2.value0, new Cons(new Guard(new Constr2(v.value0, cTrue, Nil.value)), Nil.value))));
    }
    ;
    if (v instanceof ListComp && (v.value2.value0 instanceof Guard && v.value2.value1 instanceof Cons)) {
      return bind11(exprFwd(new ListComp(v.value0, v.value1, new NonEmpty(v.value2.value1.value0, v.value2.value1.value1))))(function(e) {
        return map113(App2.create(new Lambda(elimBool(new ContExpr(e))(new ContExpr(enil(v.value0))))))(exprFwd(v.value2.value0.value0));
      });
    }
    ;
    if (v instanceof ListComp && (v.value2.value0 instanceof Declaration && v.value2.value1 instanceof Cons)) {
      return bind11(exprFwd(new ListComp(v.value0, v.value1, new NonEmpty(v.value2.value1.value0, v.value2.value1.value1))))(function(e) {
        return bind11(patternFwd(v.value2.value0.value0.value0)(new ContExpr(e)))(function(\u03C3) {
          return map113(App2.create(new Lambda(\u03C3)))(exprFwd(v.value2.value0.value0.value1));
        });
      });
    }
    ;
    if (v instanceof ListComp && (v.value2.value0 instanceof Generator && v.value2.value1 instanceof Cons)) {
      return bind11(exprFwd(new ListComp(v.value0, v.value1, new NonEmpty(v.value2.value1.value0, v.value2.value1.value1))))(function(e) {
        return bind11(patternFwd(v.value2.value0.value0)(new ContExpr(e)))(function(\u03C3) {
          return map113(App2.create(new App2(new Var("concatMap"), new Lambda(asElim(totaliseFwd(new ContElim(\u03C3))(v.value0))))))(exprFwd(v.value2.value0.value1));
        });
      });
    }
    ;
    if (v instanceof Let2) {
      return varDefsFwd(new Tuple(v.value0, v.value1));
    }
    ;
    if (v instanceof LetRec2) {
      return apply7(map113(LetRec.create)(recDefsFwd(v.value0)))(exprFwd(v.value1));
    }
    ;
    throw new Error("Failed pattern match at DesugarFwd (line 71, column 1 - line 71, column 40): " + [v.constructor.name]);
  };
  var branchesFwd_uncurried = function(bs) {
    return bind11(traverse2(uncurry(branchFwd_uncurried))(bs))(function(v) {
      return foldM5(maybeJoin3)(v.value0)(v.value1);
    });
  };
  var branchesFwd_curried = function(bs) {
    return bind11(traverse2(patternsFwd)(bs))(function(v) {
      return foldM5(maybeJoin3)(v.value0)(v.value1);
    });
  };
  var branchFwd_uncurried = function(p) {
    return function(s) {
      return bind11(map113(ContExpr.create)(exprFwd(s)))(patternFwd(p));
    };
  };
  var moduleFwd = function(v) {
    var varDefOrRecDefsFwd = function(v1) {
      if (v1 instanceof Left) {
        return map113(Left.create)(varDefFwd(v1.value0));
      }
      ;
      if (v1 instanceof Right) {
        return map113(Right.create)(recDefsFwd(v1.value0));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 43, column 4 - line 43, column 84): " + [v1.constructor.name]);
    };
    var desugarDefs = function(v1) {
      if (v1 instanceof Left) {
        return map36(Left.create)(toList3(v1.value0));
      }
      ;
      if (v1 instanceof Right) {
        return pure18(new Right(v1.value0));
      }
      ;
      throw new Error("Failed pattern match at DesugarFwd (line 47, column 4 - line 47, column 71): " + [v1.constructor.name]);
    };
    return map113(Module.create)(traverse12(varDefOrRecDefsFwd)(join5(map36(desugarDefs)(v.value0))));
  };
  var desugarModuleFwd = moduleFwd;
  var desugarFwd = exprFwd;

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left3, right3, eff) {
      try {
        return right3(eff());
      } catch (error4) {
        return left3(error4);
      }
    }
    function runAsync(left3, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left3(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size3 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size3 !== 0) {
          size3--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size3 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size3) % limit] = cb;
          size3++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util2) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util2.isLeft(result) && util2.fromLeft(result)) {
                    setTimeout(function() {
                      throw util2.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util2, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step2 = aff;
      var fail4 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step2 = bhead(step2);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail4 = util2.left(e);
                step2 = null;
              }
              break;
            case STEP_RESULT:
              if (util2.isLeft(step2)) {
                status = RETURN;
                fail4 = step2;
                step2 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step2 = util2.fromRight(step2);
              }
              break;
            case CONTINUE:
              switch (step2.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step2._2;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step2 = util2.right(step2._1);
                  } else {
                    status = STEP_BIND;
                    step2 = step2._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step2 = runSync(util2.left, util2.right, step2._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step2 = runAsync(util2.left, step2._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step2 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail4 = util2.left(step2._1);
                  step2 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step2, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step2, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step2 = step2._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util2, supervisor, step2._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step2._1) {
                    tmp.run();
                  }
                  step2 = util2.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step2 = sequential2(util2, supervisor, step2._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step2 = interrupt || fail4 || step2;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail4) {
                      status = CONTINUE;
                      step2 = attempt._2(util2.fromLeft(fail4));
                      fail4 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail4) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step2 = util2.fromRight(step2);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail4 === null) {
                      result = util2.fromRight(step2);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step2 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail4), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step2 = attempt._1.killed(util2.fromLeft(interrupt))(attempt._2);
                    } else if (fail4) {
                      step2 = attempt._1.failed(util2.fromLeft(fail4))(attempt._2);
                    } else {
                      step2 = attempt._1.completed(util2.fromRight(step2))(attempt._2);
                    }
                    fail4 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step2, fail4), attempts, interrupt);
                    status = CONTINUE;
                    step2 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step2 = attempt._1;
                    fail4 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step2));
                }
              }
              joins = null;
              if (interrupt && fail4) {
                setTimeout(function() {
                  throw util2.fromLeft(fail4);
                }, 0);
              } else if (util2.isLeft(step2) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util2.fromLeft(step2);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join8) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join8.rethrow;
            join8.handler(step2)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join8;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util2.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util2.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util2.left(error4);
              status = COMPLETED;
              step2 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step2(error4)), attempts, interrupt);
                }
                status = RETURN;
                step2 = null;
                fail4 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util2.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step2 = null;
                fail4 = null;
              }
          }
          return canceler;
        };
      }
      function join7(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join: join7,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util2, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root3 = EMPTY;
      function kill(error4, par2, cb2) {
        var step2 = par2;
        var head6 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step2.tag) {
              case FORKED:
                if (step2._3 === EMPTY) {
                  tmp = fibers[step2._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head6 === null) {
                  break loop;
                }
                step2 = head6._2;
                if (tail2 === null) {
                  head6 = null;
                } else {
                  head6 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step2 = step2._2;
                break;
              case APPLY:
              case ALT:
                if (head6) {
                  tail2 = new Aff2(CONS, head6, tail2);
                }
                head6 = step2;
                step2 = step2._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util2.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join7(result, head6, tail2) {
        var fail4, step2, lhs, rhs, tmp, kid;
        if (util2.isLeft(result)) {
          fail4 = result;
          step2 = null;
        } else {
          step2 = result;
          fail4 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head6 === null) {
              cb(fail4 || step2)();
              return;
            }
            if (head6._3 !== EMPTY) {
              return;
            }
            switch (head6.tag) {
              case MAP:
                if (fail4 === null) {
                  head6._3 = util2.right(head6._1(util2.fromRight(step2)));
                  step2 = head6._3;
                } else {
                  head6._3 = fail4;
                }
                break;
              case APPLY:
                lhs = head6._1._3;
                rhs = head6._2._3;
                if (fail4) {
                  head6._3 = fail4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, fail4 === lhs ? head6._2 : head6._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join7(fail4, null, null);
                      } else {
                        join7(fail4, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step2 = util2.right(util2.fromRight(lhs)(util2.fromRight(rhs)));
                  head6._3 = step2;
                }
                break;
              case ALT:
                lhs = head6._1._3;
                rhs = head6._2._3;
                if (lhs === EMPTY && util2.isLeft(rhs) || rhs === EMPTY && util2.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util2.isLeft(lhs) && rhs !== EMPTY && util2.isLeft(rhs)) {
                  fail4 = step2 === lhs ? rhs : lhs;
                  step2 = null;
                  head6._3 = fail4;
                } else {
                  head6._3 = step2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, step2 === lhs ? head6._2 : head6._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join7(step2, null, null);
                      } else {
                        join7(step2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head6 = null;
            } else {
              head6 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join7(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step2 = par;
        var head6 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step2.tag) {
                  case MAP:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(MAP, step2._1, EMPTY, EMPTY);
                    step2 = step2._2;
                    break;
                  case APPLY:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(APPLY, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  case ALT:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(ALT, EMPTY, step2._2, EMPTY);
                    step2 = step2._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step2;
                    step2 = new Aff2(FORKED, fid, new Aff2(CONS, head6, tail2), EMPTY);
                    tmp = Fiber(util2, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step2)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head6 === null) {
                  break loop;
                }
                if (head6._1 === EMPTY) {
                  head6._1 = step2;
                  status = CONTINUE;
                  step2 = head6._2;
                  head6._2 = EMPTY;
                } else {
                  head6._2 = step2;
                  step2 = head6;
                  if (tail2 === null) {
                    head6 = null;
                  } else {
                    head6 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root3 = step2;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util2.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill(error4, root3, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential2(util2, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util2, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value2) {
          return Aff.Pure(f(value2));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  var _liftEffect = Aff.Sync;
  var makeAff = Aff.Async;
  function _makeFiber(util2, aff) {
    return function() {
      return Aff.Fiber(util2, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right3, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer2 = setDelay(ms, cb(right3()));
          return function() {
            return Aff.Sync(function() {
              return right3(clearDelay(ms, timer2));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Class/index.js
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map38 = /* @__PURE__ */ map3(functorEither);
  var ExceptT = function(x2) {
    return x2;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map118 = map3(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map118(map38(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind20 = bind(dictMonad.Bind1());
    var pure24 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind20(v)(either(function($187) {
            return pure24(Left.create($187));
          })(function(a) {
            var v1 = k(a);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append10 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind20 = bind(Bind1);
      var pure24 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind20(v)(function(rm) {
              if (rm instanceof Right) {
                return pure24(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind20(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure24(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure24(new Left(append10(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy8 = function(name3, moduleName, init4) {
    var state2 = 0;
    var val2;
    return function(lineNumber) {
      if (state2 === 2)
        return val2;
      if (state2 === 1)
        throw new ReferenceError(name3 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val2 = init4();
      state2 = 2;
      return val2;
    };
  };
  var $$void6 = /* @__PURE__ */ $$void(functorEffect);
  var functorAff = {
    map: _map
  };
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 21 - line 409, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 20 - line 404, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 397, column 12 - line 399, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy8("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(71);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindAff);
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped5(function($77) {
        return liftEffect2(k($77));
      })($$try3(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void6(runAff(k)(aff));
    };
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit2));

  // output/Effect.Console/foreign.js
  var log2 = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Trace/index.js
  var unions4 = /* @__PURE__ */ unions(foldableList)(ordString);
  var map39 = /* @__PURE__ */ map3(functorList);
  var unions12 = /* @__PURE__ */ unions(foldableMap)(ordString);
  var map114 = /* @__PURE__ */ map3(functorMap);
  var MatchVar = /* @__PURE__ */ function() {
    function MatchVar2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MatchVar2.create = function(value0) {
      return function(value1) {
        return new MatchVar2(value0, value1);
      };
    };
    return MatchVar2;
  }();
  var MatchVarAnon = /* @__PURE__ */ function() {
    function MatchVarAnon2(value0) {
      this.value0 = value0;
    }
    ;
    MatchVarAnon2.create = function(value0) {
      return new MatchVarAnon2(value0);
    };
    return MatchVarAnon2;
  }();
  var MatchConstr = /* @__PURE__ */ function() {
    function MatchConstr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MatchConstr2.create = function(value0) {
      return function(value1) {
        return new MatchConstr2(value0, value1);
      };
    };
    return MatchConstr2;
  }();
  var MatchRecord = /* @__PURE__ */ function() {
    function MatchRecord2(value0) {
      this.value0 = value0;
    }
    ;
    MatchRecord2.create = function(value0) {
      return new MatchRecord2(value0);
    };
    return MatchRecord2;
  }();
  var VarDef3 = /* @__PURE__ */ function() {
    function VarDef4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    VarDef4.create = function(value0) {
      return function(value1) {
        return new VarDef4(value0, value1);
      };
    };
    return VarDef4;
  }();
  var Var3 = /* @__PURE__ */ function() {
    function Var4(value0) {
      this.value0 = value0;
    }
    ;
    Var4.create = function(value0) {
      return new Var4(value0);
    };
    return Var4;
  }();
  var Op3 = /* @__PURE__ */ function() {
    function Op4(value0) {
      this.value0 = value0;
    }
    ;
    Op4.create = function(value0) {
      return new Op4(value0);
    };
    return Op4;
  }();
  var Int4 = /* @__PURE__ */ function() {
    function Int5(value0) {
      this.value0 = value0;
    }
    ;
    Int5.create = function(value0) {
      return new Int5(value0);
    };
    return Int5;
  }();
  var Float4 = /* @__PURE__ */ function() {
    function Float5(value0) {
      this.value0 = value0;
    }
    ;
    Float5.create = function(value0) {
      return new Float5(value0);
    };
    return Float5;
  }();
  var Str4 = /* @__PURE__ */ function() {
    function Str5(value0) {
      this.value0 = value0;
    }
    ;
    Str5.create = function(value0) {
      return new Str5(value0);
    };
    return Str5;
  }();
  var Record4 = /* @__PURE__ */ function() {
    function Record5(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Record5.create = function(value0) {
      return function(value1) {
        return new Record5(value0, value1);
      };
    };
    return Record5;
  }();
  var Constr4 = /* @__PURE__ */ function() {
    function Constr5(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Constr5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Constr5(value0, value1, value2);
        };
      };
    };
    return Constr5;
  }();
  var Matrix4 = /* @__PURE__ */ function() {
    function Matrix5(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    Matrix5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new Matrix5(value0, value1, value2, value3);
          };
        };
      };
    };
    return Matrix5;
  }();
  var Lambda3 = /* @__PURE__ */ function() {
    function Lambda4(value0) {
      this.value0 = value0;
    }
    ;
    Lambda4.create = function(value0) {
      return new Lambda4(value0);
    };
    return Lambda4;
  }();
  var Project3 = /* @__PURE__ */ function() {
    function Project4(value0, value1, value2) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
    }
    ;
    Project4.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return new Project4(value0, value1, value2);
        };
      };
    };
    return Project4;
  }();
  var App4 = /* @__PURE__ */ function() {
    function App5(value0, value1, value2, value3) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value2;
      this.value3 = value3;
    }
    ;
    App5.create = function(value0) {
      return function(value1) {
        return function(value2) {
          return function(value3) {
            return new App5(value0, value1, value2, value3);
          };
        };
      };
    };
    return App5;
  }();
  var AppPrim = /* @__PURE__ */ function() {
    function AppPrim2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AppPrim2.create = function(value0) {
      return function(value1) {
        return new AppPrim2(value0, value1);
      };
    };
    return AppPrim2;
  }();
  var AppConstr = /* @__PURE__ */ function() {
    function AppConstr2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    AppConstr2.create = function(value0) {
      return function(value1) {
        return new AppConstr2(value0, value1);
      };
    };
    return AppConstr2;
  }();
  var Let3 = /* @__PURE__ */ function() {
    function Let4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Let4.create = function(value0) {
      return function(value1) {
        return new Let4(value0, value1);
      };
    };
    return Let4;
  }();
  var LetRec3 = /* @__PURE__ */ function() {
    function LetRec4(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    LetRec4.create = function(value0) {
      return function(value1) {
        return new LetRec4(value0, value1);
      };
    };
    return LetRec4;
  }();
  var bVMatch = {
    bv: function(v) {
      if (v instanceof MatchVar) {
        return singleton6(v.value0);
      }
      ;
      if (v instanceof MatchVarAnon) {
        return empty5;
      }
      ;
      if (v instanceof MatchConstr) {
        return unions4(map39(bv(bVMatch))(v.value1));
      }
      ;
      if (v instanceof MatchRecord) {
        return unions12(map114(bv(bVMatch))(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Trace (line 39, column 1 - line 43, column 49): " + [v.constructor.name]);
    }
  };

  // output/Eval/index.js
  var pure19 = /* @__PURE__ */ pure(applicativeEither);
  var bind12 = /* @__PURE__ */ bind(bindEither);
  var disjUnion3 = /* @__PURE__ */ disjUnion(ordString);
  var show7 = /* @__PURE__ */ show(showInt);
  var discard4 = /* @__PURE__ */ discard(discardUnit)(bindEither);
  var checkConsistent2 = /* @__PURE__ */ checkConsistent(keyCtr);
  var show15 = /* @__PURE__ */ show(showCtr);
  var lookup4 = /* @__PURE__ */ lookup(ordCtr);
  var map40 = /* @__PURE__ */ map3(functorEither);
  var second3 = /* @__PURE__ */ second(strongFn);
  var dataTypeFor4 = /* @__PURE__ */ dataTypeFor(dataTypeForSetCtr);
  var prettyP3 = /* @__PURE__ */ prettyP(prettyValBoolean);
  var show24 = /* @__PURE__ */ show(showDataType$primeInt);
  var subset2 = /* @__PURE__ */ subset(ordString);
  var show33 = /* @__PURE__ */ show(/* @__PURE__ */ showSet(showString));
  var toUnfoldable13 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable3(ordString)(foldableList);
  var mapFlipped6 = /* @__PURE__ */ mapFlipped(functorMap);
  var union8 = /* @__PURE__ */ union2(ordString);
  var fv3 = /* @__PURE__ */ fv(/* @__PURE__ */ fVMapVar(fVElim));
  var fv1 = /* @__PURE__ */ fv(fVElim);
  var traverse3 = /* @__PURE__ */ traverse(traversableMap)(applicativeEither);
  var mapFlipped12 = /* @__PURE__ */ mapFlipped(functorEither);
  var traverse13 = /* @__PURE__ */ traverse(traversableList)(applicativeEither);
  var bimap5 = /* @__PURE__ */ bimap2(bifunctorTuple);
  var fromFoldable15 = /* @__PURE__ */ fromFoldable(foldableList);
  var eq13 = /* @__PURE__ */ eq(eqCtr);
  var match12 = /* @__PURE__ */ match3(toFromInt);
  var greaterThanOrEq1 = /* @__PURE__ */ greaterThanOrEq(/* @__PURE__ */ ordTuple(ordInt)(ordInt));
  var show42 = /* @__PURE__ */ show(/* @__PURE__ */ showTuple(showInt)(showInt));
  var map115 = /* @__PURE__ */ map3(functorFn);
  var map211 = /* @__PURE__ */ map3(functorList);
  var sequence3 = /* @__PURE__ */ sequence(traversableList)(applicativeEither);
  var bind13 = /* @__PURE__ */ bind(bindList);
  var append14 = /* @__PURE__ */ append(semigroupList);
  var patternMismatch = function(s) {
    return function(s$prime) {
      return "Pattern mismatch: found " + (s + (", expected " + s$prime));
    };
  };
  var matchMany = function(v) {
    return function(v1) {
      if (v instanceof Nil) {
        return pure19(new Tuple(new Tuple(empty4, v1), Nil.value));
      }
      ;
      if (v instanceof Cons && v1 instanceof ContElim) {
        return bind12(match4(v.value0)(v1.value0))(function(v3) {
          return bind12(matchMany(v.value1)(v3.value0.value1))(function(v4) {
            return pure19(new Tuple(new Tuple(disjUnion3(v3.value0.value0)(v4.value0.value0), v4.value0.value1), new Cons(v3.value1, v4.value1)));
          });
        });
      }
      ;
      if (v instanceof Cons && v1 instanceof ContExpr) {
        return report(show7(length3(v.value1) + 1 | 0) + " extra argument(s) to constructor/record; did you forget parentheses in lambda pattern?");
      }
      ;
      return error2(absurd2);
    };
  };
  var match4 = function(v) {
    return function(v1) {
      var v2 = v;
      if (v1 instanceof ElimVar) {
        if (v1.value0 === varAnon) {
          return pure19(new Tuple(new Tuple(empty4, v1.value1), new MatchVarAnon(v2)));
        }
        ;
        if (otherwise) {
          return pure19(new Tuple(new Tuple(singleton4(v1.value0)(v2), v1.value1), new MatchVar(v1.value0, v2)));
        }
        ;
      }
      ;
      if (v instanceof Constr3 && v1 instanceof ElimConstr) {
        return discard4(checkConsistent2("Pattern mismatch: ")(v.value1)(keys2(v1.value0)))(function() {
          return bind12(note("Incomplete patterns: no branch for " + show15(v.value1))(lookup4(v.value1)(v1.value0)))(function(\u03BA) {
            return map40(second3(MatchConstr.create(v.value1)))(matchMany(v.value2)(\u03BA));
          });
        });
      }
      ;
      var v2 = v;
      if (v1 instanceof ElimConstr) {
        return bind12(dataTypeFor4(keys2(v1.value0)))(function(d) {
          return report(patternMismatch(prettyP3(v2))(show24(d)));
        });
      }
      ;
      if (v instanceof Record3 && v1 instanceof ElimRecord) {
        return discard4(check(subset2(v1.value0)(keys2(v.value1)))(patternMismatch(show33(keys2(v.value1)))(show33(v1.value0))))(function() {
          var v22 = unzip(toUnfoldable13(v.value1));
          return map40(second3(function() {
            var $239 = zip2(v22.value0);
            return function($240) {
              return MatchRecord.create(fromFoldable8($239($240)));
            };
          }()))(matchMany(v22.value1)(v1.value1));
        });
      }
      ;
      if (v1 instanceof ElimRecord) {
        return report(patternMismatch(prettyP3(v))(show33(v1.value0)));
      }
      ;
      throw new Error("Failed pattern match at Eval (line 30, column 1 - line 30, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var closeDefs = function(\u03B3) {
    return function(\u03C1) {
      return mapFlipped6(\u03C1)(function(\u03C3) {
        var xs = union8(fv3($$for(\u03C1)(\u03C3)))(fv1(\u03C3));
        return new Closure(false, restrict(\u03B3)(xs), \u03C1, \u03C3);
      });
    };
  };
  var checkArity2 = function(c) {
    return function(n) {
      return bind12(arity(c))(function(n$prime) {
        return check(n$prime >= n)(show15(c) + (" got " + (show7(n) + (" argument(s), expects at most " + show7(n$prime)))));
      });
    };
  };
  var $$eval = function(v) {
    return function(v1) {
      if (v1 instanceof Var) {
        return map40(function(v2) {
          return new Tuple(new Var3(v1.value0), v2);
        })(lookup$prime(v1.value0)(v));
      }
      ;
      if (v1 instanceof Op) {
        return map40(function(v2) {
          return new Tuple(new Op3(v1.value0), v2);
        })(lookup$prime(v1.value0)(v));
      }
      ;
      if (v1 instanceof Int) {
        return pure19(new Tuple(new Int4(v1.value1), new Int3(false, v1.value1)));
      }
      ;
      if (v1 instanceof Float) {
        return pure19(new Tuple(new Float4(v1.value1), new Float3(false, v1.value1)));
      }
      ;
      if (v1 instanceof Str) {
        return pure19(new Tuple(new Str4(v1.value1), new Str3(false, v1.value1)));
      }
      ;
      if (v1 instanceof Record) {
        return bind12(traverse3($$eval(v))(v1.value1))(function(xtvs) {
          return pure19(new Tuple(new Record4(v, mapFlipped6(xtvs)(fst)), new Record3(false, mapFlipped6(xtvs)(snd))));
        });
      }
      ;
      if (v1 instanceof Constr) {
        return discard4(checkArity2(v1.value1)(length3(v1.value2)))(function() {
          return bind12(mapFlipped12(traverse13($$eval(v))(v1.value2))(unzip))(function(v2) {
            return pure19(new Tuple(new Constr4(v, v1.value1, v2.value0), new Constr3(false, v1.value1, v2.value1)));
          });
        });
      }
      ;
      if (v1 instanceof Matrix) {
        var unzipToArray = function() {
          var $241 = bimap5(fromFoldable15)(fromFoldable15);
          return function($242) {
            return $241(unzip($242));
          };
        }();
        return bind12($$eval(v)(v1.value3))(function(v2) {
          if (v2.value1 instanceof Constr3 && (v2.value1.value2 instanceof Cons && (v2.value1.value2.value1 instanceof Cons && (v2.value1.value2.value1.value1 instanceof Nil && eq13(v2.value1.value1)(cPair))))) {
            var v4 = new Tuple(match12(v2.value1.value2.value0), match12(v2.value1.value2.value1.value0));
            return discard4(check(greaterThanOrEq1(new Tuple(v4.value0.value0, v4.value1.value0))(new Tuple(1, 1)))("array must be at least (" + (show42(new Tuple(1, 1)) + ("); got (" + (show42(new Tuple(v4.value0.value0, v4.value1.value0)) + ")")))))(function() {
              return bind12(map40(map115(unzipToArray)(map211(unzipToArray)))(sequence3(bind13(range3(1)(v4.value0.value0))(function(i) {
                return singleton3(sequence3(bind13(range3(1)(v4.value1.value0))(function(j) {
                  var \u03B3$prime2 = disjUnion3(singleton4(v1.value2.value0)(new Int3(false, i)))(singleton4(v1.value2.value1)(new Int3(false, j)));
                  return singleton3($$eval(append6(v)(\u03B3$prime2))(v1.value1));
                })));
              }))))(function(v5) {
                return pure19(new Tuple(new Matrix4(v5.value0, new Tuple(v1.value2.value0, v1.value2.value1), new Tuple(v4.value0.value0, v4.value1.value0), v2.value0), new Matrix3(false, new Tuple(new Tuple(v5.value1, new Tuple(v4.value0.value0, false)), new Tuple(v4.value1.value0, false)))));
              });
            });
          }
          ;
          return report("Array dimensions must be pair of ints; got " + prettyP3(v2.value1));
        });
      }
      ;
      if (v1 instanceof Lambda) {
        return pure19(new Tuple(new Lambda3(v1.value0), new Closure(false, restrict(v)(fv1(v1.value0)), empty4, v1.value0)));
      }
      ;
      if (v1 instanceof Project) {
        return bind12($$eval(v)(v1.value0))(function(v2) {
          if (v2.value1 instanceof Record3) {
            return map40(function(v4) {
              return new Tuple(new Project3(v2.value0, toUnfoldable13(v2.value1.value1), v1.value1), v4);
            })(find3(v1.value1)(toUnfoldable13(v2.value1.value1)));
          }
          ;
          return report("Expected record");
        });
      }
      ;
      if (v1 instanceof App2) {
        return bind12($$eval(v)(v1.value0))(function(v2) {
          return bind12($$eval(v)(v1.value1))(function(v4) {
            if (v2.value1 instanceof Closure) {
              var \u03B32 = closeDefs(v2.value1.value1)(v2.value1.value2);
              return bind12(match4(v4.value1)(v2.value1.value3))(function(v5) {
                return bind12($$eval(append6(append6(v2.value1.value1)(\u03B32))(v5.value0.value0))(asExpr(v5.value0.value1)))(function(v6) {
                  return pure19(new Tuple(new App4(new Tuple(new Tuple(v2.value0, dom2(v2.value1.value2)), v2.value1.value3), v4.value0, v5.value1, v6.value0), v6.value1));
                });
              });
            }
            ;
            if (v2.value1 instanceof Primitive) {
              var vs$prime = append14(v2.value1.value1)(singleton3(v4.value1));
              var v$prime$prime = function() {
                var $186 = v2.value1.value0.arity > length3(vs$prime);
                if ($186) {
                  return new Primitive(v2.value1.value0, vs$prime);
                }
                ;
                return v2.value1.value0.op(vs$prime);
              }();
              return pure19(new Tuple(new AppPrim(new Tuple(new Tuple(v2.value0, v2.value1.value0), v2.value1.value1), new Tuple(v4.value0, v4.value1)), v$prime$prime));
            }
            ;
            if (v2.value1 instanceof Constr3) {
              return discard4(check(successful(arity(v2.value1.value1)) > length3(v2.value1.value2))("Too many arguments to " + show15(v2.value1.value1)))(function() {
                return pure19(new Tuple(new AppConstr(new Tuple(new Tuple(v2.value0, v2.value1.value1), length3(v2.value1.value2)), v4.value0), new Constr3(false, v2.value1.value1, append14(v2.value1.value2)(singleton3(v4.value1)))));
              });
            }
            ;
            return report("Expected closure, operator or unsaturated constructor");
          });
        });
      }
      ;
      if (v1 instanceof Let) {
        return bind12($$eval(v)(v1.value0.value1))(function(v2) {
          return bind12(match4(v2.value1)(v1.value0.value0))(function(v4) {
            return bind12($$eval(append6(v)(v4.value0.value0))(v1.value1))(function(v5) {
              return pure19(new Tuple(new Let3(new VarDef3(v4.value1, v2.value0), v5.value0), v5.value1));
            });
          });
        });
      }
      ;
      if (v1 instanceof LetRec) {
        var \u03B3$prime = closeDefs(v)(fromFoldable8(v1.value0));
        return bind12($$eval(append6(v)(\u03B3$prime))(v1.value1))(function(v2) {
          return pure19(new Tuple(new LetRec3(v1.value0, v2.value0), v2.value1));
        });
      }
      ;
      throw new Error("Failed pattern match at Eval (line 66, column 1 - line 66, column 53): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var eval_module = function(\u03B3) {
    var go = function(v) {
      return function(v1) {
        if (v1.value0 instanceof Nil) {
          return pure19(v);
        }
        ;
        if (v1.value0 instanceof Cons && v1.value0.value0 instanceof Left) {
          return bind12($$eval(append6(\u03B3)(v))(v1.value0.value0.value0.value1))(function(v2) {
            return bind12(match4(v2.value1)(v1.value0.value0.value0.value0))(function(v4) {
              return go(append6(v)(v4.value0.value0))(new Module(v1.value0.value1));
            });
          });
        }
        ;
        if (v1.value0 instanceof Cons && v1.value0.value0 instanceof Right) {
          return go(append6(v)(closeDefs(append6(\u03B3)(v))(fromFoldable8(v1.value0.value0.value0))))(new Module(v1.value0.value1));
        }
        ;
        throw new Error("Failed pattern match at Eval (line 133, column 4 - line 133, column 46): " + [v.constructor.name, v1.constructor.name]);
      };
    };
    return go(empty4);
  };

  // output/EvalBwd/index.js
  var disjUnion_inv2 = /* @__PURE__ */ disjUnion_inv(ordString);
  var bv3 = /* @__PURE__ */ bv(bVMatch);
  var append7 = /* @__PURE__ */ append(semigroupList);
  var eq11 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqString));
  var mustLookup3 = /* @__PURE__ */ mustLookup(ordString);
  var botOf3 = /* @__PURE__ */ botOf(boundedSlicesValBoolean);
  var toUnfoldable14 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var fromFoldable9 = /* @__PURE__ */ fromFoldable3(ordString)(foldableList);
  var insert4 = /* @__PURE__ */ insert(ordString);
  var weakJoin2 = /* @__PURE__ */ weakJoin(slicesValBoolean);
  var weakJoin1 = /* @__PURE__ */ weakJoin(slicesElimBoolean);
  var join6 = /* @__PURE__ */ join2(joinSemilatticeBoolean);
  var foldrWithIndex3 = /* @__PURE__ */ foldrWithIndex(foldableWithIndexMap);
  var joinSemilatticeMap3 = /* @__PURE__ */ joinSemilatticeMap(keyString);
  var join1 = /* @__PURE__ */ join2(/* @__PURE__ */ joinSemilatticeMap3(slicesElimBoolean));
  var intersectionWith2 = /* @__PURE__ */ intersectionWith(ordString);
  var mapFlipped7 = /* @__PURE__ */ mapFlipped(functorMap);
  var foldr7 = /* @__PURE__ */ foldr(foldableMap);
  var join22 = /* @__PURE__ */ join2(/* @__PURE__ */ joinSemilatticeMap3(slicesValBoolean));
  var botOf1 = /* @__PURE__ */ botOf(/* @__PURE__ */ boundedSlicesMap(keyString)(boundedSlicesValBoolean));
  var foldr13 = /* @__PURE__ */ foldr(foldableList);
  var bind14 = /* @__PURE__ */ bind(bindList);
  var union9 = /* @__PURE__ */ union2(ordString);
  var disjUnion4 = /* @__PURE__ */ disjUnion(ordString);
  var bot4 = /* @__PURE__ */ bot(boundedJoinSemilatticeBoo);
  var foldl13 = /* @__PURE__ */ foldl12(foldableList);
  var join32 = /* @__PURE__ */ join2(joinSemilatticeExprBoolea);
  var map41 = /* @__PURE__ */ map3(/* @__PURE__ */ functorNonEmpty(functorList));
  var length7 = /* @__PURE__ */ length2(foldableList)(semiringInt);
  var botOf22 = /* @__PURE__ */ botOf(/* @__PURE__ */ boundedSlicesList(/* @__PURE__ */ boundedSlicesTuple(eqString)(showString)(boundedSlicesElimBoolean)));
  var matchManyBwd = function(v) {
    return function(\u03BA) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nil) {
            if (isEmpty(v)) {
              return new Tuple(Nil.value, \u03BA);
            }
            ;
            if (otherwise) {
              return error2(absurd2);
            }
            ;
          }
          ;
          if (v2 instanceof Cons) {
            var v3 = disjUnion_inv2(bv3(v2.value0))(v);
            var v4 = matchBwd(v3.value0)(\u03BA)(v1)(v2.value0);
            var v6 = matchManyBwd(v3.value1)(new ContElim(v4.value1))(v1)(v2.value1);
            return new Tuple(append7(v6.value0)(new Cons(v4.value0, Nil.value)), v6.value1);
          }
          ;
          throw new Error("Failed pattern match at EvalBwd (line 51, column 1 - line 51, column 80): " + [v.constructor.name, \u03BA.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    };
  };
  var matchBwd = function(v) {
    return function(\u03BA) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof MatchVar) {
            if (eq11(dom2(v))(singleton6(v2.value0))) {
              return new Tuple(mustLookup3(v2.value0)(v), new ElimVar(v2.value0, \u03BA));
            }
            ;
            if (otherwise) {
              return new Tuple(botOf3(v2.value1), new ElimVar(v2.value0, \u03BA));
            }
            ;
          }
          ;
          if (v2 instanceof MatchVarAnon) {
            if (isEmpty(v)) {
              return new Tuple(botOf3(v2.value0), new ElimVar(varAnon, \u03BA));
            }
            ;
            if (otherwise) {
              return error2(absurd2);
            }
            ;
          }
          ;
          if (v2 instanceof MatchConstr) {
            var v3 = matchManyBwd(v)(\u03BA)(v1)(reverse2(v2.value1));
            return new Tuple(new Constr3(v1, v2.value0, v3.value0), new ElimConstr(singleton4(v2.value0)(v3.value1)));
          }
          ;
          if (v2 instanceof MatchRecord) {
            var v3 = unzip(toUnfoldable14(v2.value0));
            var v4 = matchManyBwd(v)(\u03BA)(v1)(reverse2(v3.value1));
            return new Tuple(new Record3(v1, fromFoldable9(zip2(v3.value0)(v4.value0))), new ElimRecord(keys2(v2.value0), v4.value1));
          }
          ;
          throw new Error("Failed pattern match at EvalBwd (line 38, column 1 - line 38, column 62): " + [v.constructor.name, \u03BA.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    };
  };
  var closeDefsBwd = function(\u03B3) {
    var joinDefs = function(f) {
      return function(v2) {
        return function(v1) {
          var v22 = mustLookup3(f)(\u03B3);
          if (v22 instanceof Closure) {
            return new Tuple(new Tuple(new Tuple(insert4(f)(v22.value3)(v1.value0.value0.value0), weakJoin2(v1.value0.value0.value1)(v22.value1)), weakJoin1(v1.value0.value1)(v22.value2)), join6(v1.value1)(v22.value0));
          }
          ;
          return error2(absurd2);
        };
      };
    };
    var v = foldrWithIndex3(joinDefs)(new Tuple(new Tuple(new Tuple(empty4, empty4), empty4), false))(\u03B3);
    return new Tuple(new Tuple(v.value0.value0.value1, join1(v.value0.value1)(v.value0.value0.value0)), v.value1);
  };
  var evalBwd = function(v) {
    return function(v1) {
      var v2 = v;
      if (v1 instanceof Var3) {
        return new Tuple(new Tuple(singleton4(v1.value0)(v2), new Var(v1.value0)), false);
      }
      ;
      var v2 = v;
      if (v1 instanceof Op3) {
        return new Tuple(new Tuple(singleton4(v1.value0)(v2), new Op(v1.value0)), false);
      }
      ;
      if (v instanceof Str3 && v1 instanceof Str4) {
        return new Tuple(new Tuple(empty4, new Str(v.value0, v1.value0)), v.value0);
      }
      ;
      if (v instanceof Int3 && v1 instanceof Int4) {
        return new Tuple(new Tuple(empty4, new Int(v.value0, v1.value0)), v.value0);
      }
      ;
      if (v instanceof Float3 && v1 instanceof Float4) {
        return new Tuple(new Tuple(empty4, new Float(v.value0, v1.value0)), v.value0);
      }
      ;
      if (v instanceof Closure && v1 instanceof Lambda3) {
        return new Tuple(new Tuple(v.value1, new Lambda(v.value3)), v.value0);
      }
      ;
      if (v instanceof Record3 && v1 instanceof Record4) {
        var xvts = intersectionWith2(Tuple.create)(v.value1)(v1.value1);
        var x\u03B3e\u03B1s = mapFlipped7(xvts)(uncurry(evalBwd));
        var \u03B3$prime = foldr7(join22)(botOf1(v1.value0))(mapFlipped7(x\u03B3e\u03B1s)(function($394) {
          return fst(fst($394));
        }));
        return new Tuple(new Tuple(\u03B3$prime, new Record(v.value0, mapFlipped7(x\u03B3e\u03B1s)(function($395) {
          return snd(fst($395));
        }))), foldr7(join6)(v.value0)(mapFlipped7(x\u03B3e\u03B1s)(snd)));
      }
      ;
      if (v instanceof Constr3 && v1 instanceof Constr4) {
        var evalArg_bwd = function(v22) {
          return function(v32) {
            var v42 = evalBwd(v22.value0)(v22.value1);
            return new Tuple(new Tuple(join22(v32.value0.value0)(v42.value0.value0), new Cons(v42.value0.value1, v32.value0.value1)), join6(v32.value1)(v42.value1));
          };
        };
        var v2 = foldr13(evalArg_bwd)(new Tuple(new Tuple(botOf1(v1.value0), Nil.value), v.value0))(zip2(v.value2)(v1.value2));
        return new Tuple(new Tuple(v2.value0.value0, new Constr(v.value0, v1.value1, v2.value0.value1)), v2.value1);
      }
      ;
      if (v instanceof Matrix3 && v1 instanceof Matrix4) {
        var v2 = nonEmpty(bind14(range3(1)(v1.value2.value0))(function(i) {
          return bind14(range3(1)(v1.value2.value1))(function(j) {
            return singleton3(new Tuple(i, j));
          });
        }));
        var evalBwd_elem = function(v32) {
          var v42 = evalBwd(unsafeIndex4(unsafeIndex4(v.value1.value0.value0)(v32.value0 - 1 | 0))(v32.value1 - 1 | 0))(unsafeIndex4(unsafeIndex4(v1.value0)(v32.value0 - 1 | 0))(v32.value1 - 1 | 0));
          var v52 = append_inv(union9(singleton6(v1.value1.value0))(singleton6(v1.value1.value1)))(v42.value0.value0);
          var \u03B30 = append6(disjUnion4(singleton4(v1.value1.value0)(new Int3(bot4, v1.value2.value0)))(singleton4(v1.value1.value1)(new Int3(bot4, v1.value2.value1))))(v52.value1);
          var v62 = new Tuple(mustLookup3(v1.value1.value0)(\u03B30), mustLookup3(v1.value1.value0)(\u03B30));
          if (v62.value0 instanceof Int3 && v62.value1 instanceof Int3) {
            return new Tuple(new Tuple(new Tuple(new Tuple(v52.value0, v42.value0.value1), v42.value1), v62.value0.value0), v62.value1.value0);
          }
          ;
          throw new Error("Failed pattern match at EvalBwd (line 89, column 39 - line 89, column 97): " + [v62.constructor.name]);
        };
        var v3 = foldl13(function(v42) {
          return function(v52) {
            return new Tuple(new Tuple(new Tuple(new Tuple(join22(v42.value0.value0.value0.value0)(v52.value0.value0.value0.value0), join32(v42.value0.value0.value0.value1)(v52.value0.value0.value0.value1)), join6(v42.value0.value0.value1)(v52.value0.value0.value1)), join6(v42.value0.value1)(v52.value0.value1)), join6(v42.value1)(v52.value1));
          };
        })(map41(evalBwd_elem)(v2));
        var v4 = evalBwd(new Constr3(false, cPair, new Cons(new Int3(join6(v3.value0.value1)(v.value1.value0.value1.value1), v1.value2.value0), new Cons(new Int3(join6(v3.value1)(v.value1.value1.value1), v1.value2.value1), Nil.value))))(v1.value3);
        return new Tuple(new Tuple(join22(v3.value0.value0.value0.value0)(v4.value0.value0), new Matrix(v.value0, v3.value0.value0.value0.value1, new Tuple(v1.value1.value0, v1.value1.value1), v4.value0.value1)), join6(join6(v.value0)(v3.value0.value0.value1))(v4.value1));
      }
      ;
      var v2 = v;
      if (v1 instanceof Project3) {
        var v$prime = new Record3(false, insert4(v1.value2)(v2)(mapFlipped7(fromFoldable9(v1.value1))(botOf3)));
        var v3 = evalBwd(v$prime)(v1.value0);
        return new Tuple(new Tuple(v3.value0.value0, new Project(v3.value0.value1, v1.value2)), v3.value1);
      }
      ;
      var v2 = v;
      if (v1 instanceof App4) {
        var v3 = evalBwd(v2)(v1.value3);
        var v4 = append_inv(bv3(v1.value2))(v3.value0.value0);
        var v5 = matchBwd(v4.value1)(new ContExpr(v3.value0.value1))(v3.value1)(v1.value2);
        var v6 = append_inv(v1.value0.value0.value1)(v4.value0);
        var v7 = evalBwd(v5.value0)(v1.value1);
        var v8 = closeDefsBwd(v6.value1);
        var v9 = evalBwd(new Closure(join6(v3.value1)(v8.value1), join22(v6.value0)(v8.value0.value0), v8.value0.value1, v5.value1))(v1.value0.value0.value0);
        return new Tuple(new Tuple(join22(v7.value0.value0)(v9.value0.value0), new App2(v9.value0.value1, v7.value0.value1)), join6(v7.value1)(v9.value1));
      }
      ;
      var v2 = v;
      if (v1 instanceof AppPrim) {
        var vs$prime = append7(v1.value0.value1)(singleton3(v1.value1.value1));
        var v3 = definitely$prime(unsnoc(function() {
          var $312 = v1.value0.value0.value1.arity > length7(vs$prime);
          if ($312) {
            if (v2 instanceof Primitive) {
              return v2.value1;
            }
            ;
            throw new Error("Failed pattern match at EvalBwd (line 114, column 35 - line 114, column 57): " + [v2.constructor.name]);
          }
          ;
          return v1.value0.value0.value1.op_bwd(v2)(vs$prime);
        }()));
        var v4 = evalBwd(new Primitive(v1.value0.value0.value1, v3.init))(v1.value0.value0.value0);
        var v5 = evalBwd(v3.last)(v1.value1.value0);
        return new Tuple(new Tuple(join22(v4.value0.value0)(v5.value0.value0), new App2(v4.value0.value1, v5.value0.value1)), join6(v4.value1)(v5.value1));
      }
      ;
      if (v instanceof Constr3 && v1 instanceof AppConstr) {
        var v2 = definitely$prime(unsnoc(v.value2));
        var v3 = evalBwd(new Constr3(v.value0, v1.value0.value0.value1, v2.init))(v1.value0.value0.value0);
        var v4 = evalBwd(v2.last)(v1.value1);
        return new Tuple(new Tuple(join22(v3.value0.value0)(v4.value0.value0), new App2(v3.value0.value1, v4.value0.value1)), join6(v3.value1)(v4.value1));
      }
      ;
      if (v1 instanceof Let3) {
        var v3 = evalBwd(v)(v1.value1);
        var v4 = append_inv(bv3(v1.value0.value0))(v3.value0.value0);
        var v5 = matchBwd(v4.value1)(ContNone.value)(v3.value1)(v1.value0.value0);
        var v6 = evalBwd(v5.value0)(v1.value0.value1);
        return new Tuple(new Tuple(join22(v4.value0)(v6.value0.value0), new Let(new VarDef(v5.value1, v6.value0.value1), v3.value0.value1)), join6(v6.value1)(v3.value1));
      }
      ;
      if (v1 instanceof LetRec3) {
        var v3 = evalBwd(v)(v1.value1);
        var v4 = append_inv(dom(v1.value0))(v3.value0.value0);
        var v5 = closeDefsBwd(v4.value1);
        return new Tuple(new Tuple(join22(v4.value0)(v5.value0.value0), new LetRec(update2(botOf22(v1.value0))(v5.value0.value1), v3.value0.value1)), join6(v3.value1)(v5.value1));
      }
      ;
      return error2(absurd2);
    };
  };

  // output/EvalFwd/index.js
  var splitStrong2 = /* @__PURE__ */ splitStrong(categoryFn)(strongFn);
  var first3 = /* @__PURE__ */ first(strongFn);
  var disjUnion5 = /* @__PURE__ */ disjUnion(ordString);
  var second4 = /* @__PURE__ */ second(strongFn);
  var mustLookup4 = /* @__PURE__ */ mustLookup(ordCtr);
  var mapFlipped8 = /* @__PURE__ */ mapFlipped(functorList);
  var toUnfoldable15 = /* @__PURE__ */ toUnfoldable2(unfoldableList);
  var mapFlipped13 = /* @__PURE__ */ mapFlipped(functorMap);
  var union10 = /* @__PURE__ */ union2(ordString);
  var fv4 = /* @__PURE__ */ fv(/* @__PURE__ */ fVMapVar(fVElim));
  var fv12 = /* @__PURE__ */ fv(fVElim);
  var intersectionWith3 = /* @__PURE__ */ intersectionWith(ordString);
  var map45 = /* @__PURE__ */ map3(functorList);
  var match_fwd4 = /* @__PURE__ */ match_fwd(toFromInt);
  var fromFoldable10 = /* @__PURE__ */ fromFoldable(foldableList);
  var bind15 = /* @__PURE__ */ bind(bindList);
  var eq14 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqString));
  var fromFoldable16 = /* @__PURE__ */ fromFoldable3(ordString)(foldableList);
  var mustLookup1 = /* @__PURE__ */ mustLookup(ordString);
  var append8 = /* @__PURE__ */ append(semigroupList);
  var matchManyFwd = function(v) {
    return function(v1) {
      return function(v2) {
        if (v instanceof Nil && v2 instanceof Nil) {
          return new Tuple(new Tuple(empty4, v1), true);
        }
        ;
        if (v instanceof Cons && v2 instanceof Cons) {
          var v4 = matchFwd(v.value0)(asElim(v1))(v2.value0);
          return splitStrong2(first3(function(v5) {
            return disjUnion5(v4.value0.value0)(v5);
          }))(function(v5) {
            return meet(v5)(v4.value1);
          })(matchManyFwd(v.value1)(v4.value0.value1)(v2.value1));
        }
        ;
        return error2(absurd2);
      };
    };
  };
  var matchFwd = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof ElimVar && v2 instanceof MatchVarAnon) {
          return new Tuple(new Tuple(empty4, v1.value1), true);
        }
        ;
        var v3 = v;
        if (v1 instanceof ElimVar && v2 instanceof MatchVar) {
          return new Tuple(new Tuple(singleton4(v2.value0)(v3), v1.value1), true);
        }
        ;
        if (v instanceof Constr3 && (v1 instanceof ElimConstr && v2 instanceof MatchConstr)) {
          return second4(function(v32) {
            return meet(v32)(v.value0);
          })(matchManyFwd(v.value2)(mustLookup4(v2.value0)(v1.value0))(v2.value1));
        }
        ;
        if (v instanceof Record3 && (v1 instanceof ElimRecord && v2 instanceof MatchRecord)) {
          return second4(function(v32) {
            return meet(v32)(v.value0);
          })(matchManyFwd(mapFlipped8(toUnfoldable15(v.value1))(snd))(v1.value1)(mapFlipped8(toUnfoldable15(v2.value0))(snd)));
        }
        ;
        return error2(absurd2);
      };
    };
  };
  var closeDefsFwd = function(\u03B3) {
    return function(\u03C1) {
      return function(\u03B1) {
        return mapFlipped13(\u03C1)(function(\u03C3) {
          var xs = union10(fv4($$for(\u03C1)(\u03C3)))(fv12(\u03C3));
          return new Closure(\u03B1, restrict(\u03B3)(xs), \u03C1, \u03C3);
        });
      };
    };
  };
  var evalFwd = function(v) {
    return function(v1) {
      return function(v2) {
        return function(v3) {
          if (v1 instanceof Var && v3 instanceof Var3) {
            return successful(lookup$prime(v3.value0)(v));
          }
          ;
          if (v1 instanceof Op && v3 instanceof Op3) {
            return successful(lookup$prime(v3.value0)(v));
          }
          ;
          if (v1 instanceof Int && v3 instanceof Int4) {
            return new Int3(meet(v1.value0)(v2), v3.value0);
          }
          ;
          if (v1 instanceof Float && v3 instanceof Float4) {
            return new Float3(meet(v1.value0)(v2), v3.value0);
          }
          ;
          if (v1 instanceof Str && v3 instanceof Str4) {
            return new Str3(meet(v1.value0)(v2), v3.value0);
          }
          ;
          if (v1 instanceof Record && v3 instanceof Record4) {
            var xvs = mapFlipped13(intersectionWith3(Tuple.create)(v1.value1)(v3.value1))(function(v42) {
              return evalFwd(v)(v42.value0)(v2)(v42.value1);
            });
            return new Record3(meet(v1.value0)(v2), xvs);
          }
          ;
          if (v1 instanceof Constr && v3 instanceof Constr4) {
            return new Constr3(meet(v1.value0)(v2), v3.value1, map45(function(v42) {
              return evalFwd(v)(v42.value0)(v2)(v42.value1);
            })(zip2(v1.value2)(v3.value2)));
          }
          ;
          if (v1 instanceof Matrix && v3 instanceof Matrix4) {
            var v4 = evalFwd(v)(v1.value3)(v2)(v3.value3);
            if (v4 instanceof Constr3 && (v4.value2 instanceof Cons && (v4.value2.value1 instanceof Cons && v4.value2.value1.value1 instanceof Nil))) {
              var v5 = new Tuple(match_fwd4(v4.value2.value0), match_fwd4(v4.value2.value1.value0));
              var vss = assert(v5.value0.value0 === v3.value2.value0 && v5.value1.value0 === v3.value2.value1)(fromFoldable10(bind15(range3(1)(v3.value2.value0))(function(i) {
                return singleton3(fromFoldable10(bind15(range3(1)(v3.value2.value1))(function(j) {
                  var \u03B3$prime2 = disjUnion5(singleton4(v3.value1.value0)(new Int3(v5.value0.value1, i)))(singleton4(v3.value1.value1)(new Int3(v5.value1.value1, j)));
                  return singleton3(evalFwd(append6(v)(\u03B3$prime2))(v1.value1)(v2)(unsafeIndex4(unsafeIndex4(v3.value0)(i - 1 | 0))(j - 1 | 0)));
                })));
              })));
              return new Matrix3(meet(v1.value0)(v2), new Tuple(new Tuple(vss, new Tuple(v3.value2.value0, v5.value0.value1)), new Tuple(v3.value2.value1, v5.value1.value1)));
            }
            ;
            return error2(absurd2);
          }
          ;
          if (v1 instanceof Lambda && v3 instanceof Lambda3) {
            return new Closure(v2, restrict(v)(fv12(v1.value0)), empty4, v1.value0);
          }
          ;
          if (v1 instanceof Project && v3 instanceof Project3) {
            var v4 = evalFwd(v)(v1.value0)(v2)(v3.value0);
            if (v4 instanceof Record3) {
              return assert(eq14(keys2(v4.value1))(keys2(fromFoldable16(v3.value1))))(mustLookup1(v3.value2)(v4.value1));
            }
            ;
            return error2(absurd2);
          }
          ;
          if (v1 instanceof App2 && v3 instanceof App4) {
            var v4 = evalFwd(v)(v1.value0)(v2)(v3.value0.value0.value0);
            if (v4 instanceof Closure) {
              var \u03B32 = closeDefsFwd(v4.value1)(v4.value2)(v4.value0);
              var v5 = evalFwd(v)(v1.value1)(v2)(v3.value1);
              var v6 = matchFwd(v5)(v4.value3)(v3.value2);
              return evalFwd(append6(append6(v4.value1)(\u03B32))(v6.value0.value0))(asExpr(v6.value0.value1))(meet(v4.value0)(v6.value1))(v3.value3);
            }
            ;
            return error2(absurd2);
          }
          ;
          if (v1 instanceof App2 && v3 instanceof AppPrim) {
            var v4 = evalFwd(v)(v1.value0)(v2)(v3.value0.value0.value0);
            if (v4 instanceof Primitive) {
              var v2$prime = evalFwd(v)(v1.value1)(v2)(v3.value1.value0);
              var vs$prime$prime = append8(v4.value1)(singleton3(v2$prime));
              var $177 = v3.value0.value0.value1.arity > length3(vs$prime$prime);
              if ($177) {
                return new Primitive(v3.value0.value0.value1, vs$prime$prime);
              }
              ;
              return v3.value0.value0.value1.op_fwd(vs$prime$prime);
            }
            ;
            return error2(absurd2);
          }
          ;
          if (v1 instanceof App2 && v3 instanceof AppConstr) {
            var v4 = evalFwd(v)(v1.value0)(v2)(v3.value0.value0.value0);
            if (v4 instanceof Constr3) {
              var v5 = evalFwd(v)(v1.value1)(v2)(v3.value1);
              return new Constr3(meet(v2)(v4.value0), v3.value0.value0.value1, append8(v4.value2)(singleton3(v5)));
            }
            ;
            return error2(absurd2);
          }
          ;
          if (v1 instanceof Let && v3 instanceof Let3) {
            var v4 = evalFwd(v)(v1.value0.value1)(v2)(v3.value0.value1);
            var v5 = matchFwd(v4)(v1.value0.value0)(v3.value0.value0);
            return evalFwd(append6(v)(v5.value0.value0))(v1.value1)(v5.value1)(v3.value1);
          }
          ;
          if (v1 instanceof LetRec && v3 instanceof LetRec3) {
            var \u03B3$prime = closeDefsFwd(v)(fromFoldable16(v1.value0))(v2);
            return evalFwd(append6(v)(\u03B3$prime))(v1.value1)(v2)(v3.value1);
          }
          ;
          return error2(absurd2);
        };
      };
    };
  };

  // output/Affjax/foreign.js
  function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options) {
    return function(errback, callback) {
      var xhr = platformSpecificDriver.newXHR();
      var fixedUrl = platformSpecificDriver.fixupUrl(options.url, xhr);
      xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
      if (options.headers) {
        try {
          for (var i = 0, header; (header = options.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function(msgIdent) {
        return function() {
          errback(new Error(msgIdent));
        };
      };
      xhr.onerror = onerror(requestFailedMessageIdent);
      xhr.ontimeout = onerror(timeoutErrorMessageIdent);
      xhr.onload = function() {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header2) {
            return header2.length > 0;
          }).map(function(header2) {
            var i2 = header2.indexOf(":");
            return mkHeader(header2.substring(0, i2))(header2.substring(i2 + 2));
          }),
          body: xhr.response
        });
      };
      xhr.responseType = options.responseType;
      xhr.withCredentials = options.withCredentials;
      xhr.timeout = options.timeout;
      xhr.send(options.content);
      return function(error4, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  }

  // output/Data.MediaType.Common/index.js
  var applicationJSON = "application/json";
  var applicationFormURLEncoded = "application/x-www-form-urlencoded";

  // output/Affjax.RequestBody/index.js
  var ArrayView = /* @__PURE__ */ function() {
    function ArrayView2(value0) {
      this.value0 = value0;
    }
    ;
    ArrayView2.create = function(value0) {
      return new ArrayView2(value0);
    };
    return ArrayView2;
  }();
  var Blob = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var $$String = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var FormData = /* @__PURE__ */ function() {
    function FormData2(value0) {
      this.value0 = value0;
    }
    ;
    FormData2.create = function(value0) {
      return new FormData2(value0);
    };
    return FormData2;
  }();
  var FormURLEncoded = /* @__PURE__ */ function() {
    function FormURLEncoded2(value0) {
      this.value0 = value0;
    }
    ;
    FormURLEncoded2.create = function(value0) {
      return new FormURLEncoded2(value0);
    };
    return FormURLEncoded2;
  }();
  var Json = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
  var toMediaType = function(v) {
    if (v instanceof FormURLEncoded) {
      return new Just(applicationFormURLEncoded);
    }
    ;
    if (v instanceof Json) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };

  // output/Affjax.RequestHeader/index.js
  var unwrap7 = /* @__PURE__ */ unwrap();
  var Accept = /* @__PURE__ */ function() {
    function Accept2(value0) {
      this.value0 = value0;
    }
    ;
    Accept2.create = function(value0) {
      return new Accept2(value0);
    };
    return Accept2;
  }();
  var ContentType = /* @__PURE__ */ function() {
    function ContentType2(value0) {
      this.value0 = value0;
    }
    ;
    ContentType2.create = function(value0) {
      return new ContentType2(value0);
    };
    return ContentType2;
  }();
  var RequestHeader = /* @__PURE__ */ function() {
    function RequestHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RequestHeader2.create = function(value0) {
      return function(value1) {
        return new RequestHeader2(value0, value1);
      };
    };
    return RequestHeader2;
  }();
  var value = function(v) {
    if (v instanceof Accept) {
      return unwrap7(v.value0);
    }
    ;
    if (v instanceof ContentType) {
      return unwrap7(v.value0);
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value1;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
  };
  var name2 = function(v) {
    if (v instanceof Accept) {
      return "Accept";
    }
    ;
    if (v instanceof ContentType) {
      return "Content-Type";
    }
    ;
    if (v instanceof RequestHeader) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
  };

  // output/Affjax.ResponseFormat/index.js
  var identity20 = /* @__PURE__ */ identity4(categoryFn);
  var $$ArrayBuffer = /* @__PURE__ */ function() {
    function $$ArrayBuffer2(value0) {
      this.value0 = value0;
    }
    ;
    $$ArrayBuffer2.create = function(value0) {
      return new $$ArrayBuffer2(value0);
    };
    return $$ArrayBuffer2;
  }();
  var Blob2 = /* @__PURE__ */ function() {
    function Blob3(value0) {
      this.value0 = value0;
    }
    ;
    Blob3.create = function(value0) {
      return new Blob3(value0);
    };
    return Blob3;
  }();
  var Document2 = /* @__PURE__ */ function() {
    function Document3(value0) {
      this.value0 = value0;
    }
    ;
    Document3.create = function(value0) {
      return new Document3(value0);
    };
    return Document3;
  }();
  var Json2 = /* @__PURE__ */ function() {
    function Json3(value0) {
      this.value0 = value0;
    }
    ;
    Json3.create = function(value0) {
      return new Json3(value0);
    };
    return Json3;
  }();
  var $$String2 = /* @__PURE__ */ function() {
    function $$String3(value0) {
      this.value0 = value0;
    }
    ;
    $$String3.create = function(value0) {
      return new $$String3(value0);
    };
    return $$String3;
  }();
  var Ignore = /* @__PURE__ */ function() {
    function Ignore2(value0) {
      this.value0 = value0;
    }
    ;
    Ignore2.create = function(value0) {
      return new Ignore2(value0);
    };
    return Ignore2;
  }();
  var toResponseType = function(v) {
    if (v instanceof $$ArrayBuffer) {
      return "arraybuffer";
    }
    ;
    if (v instanceof Blob2) {
      return "blob";
    }
    ;
    if (v instanceof Document2) {
      return "document";
    }
    ;
    if (v instanceof Json2) {
      return "text";
    }
    ;
    if (v instanceof $$String2) {
      return "text";
    }
    ;
    if (v instanceof Ignore) {
      return "";
    }
    ;
    throw new Error("Failed pattern match at Affjax.ResponseFormat (line 44, column 3 - line 50, column 19): " + [v.constructor.name]);
  };
  var toMediaType2 = function(v) {
    if (v instanceof Json2) {
      return new Just(applicationJSON);
    }
    ;
    return Nothing.value;
  };
  var string2 = /* @__PURE__ */ function() {
    return new $$String2(identity20);
  }();
  var ignore = /* @__PURE__ */ function() {
    return new Ignore(identity20);
  }();

  // output/Affjax.ResponseHeader/index.js
  var ResponseHeader = /* @__PURE__ */ function() {
    function ResponseHeader2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseHeader2.create = function(value0) {
      return function(value1) {
        return new ResponseHeader2(value0, value1);
      };
    };
    return ResponseHeader2;
  }();

  // output/Control.Monad.Except/index.js
  var unwrap8 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap8(runExceptT($3));
  };

  // output/Data.Argonaut.Core/foreign.js
  function id2(x2) {
    return x2;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }

  // output/Foreign.Object/foreign.js
  var empty9 = {};
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys3 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Argonaut.Core/index.js
  var jsonEmptyObject = /* @__PURE__ */ id2(empty9);

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail4, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail4(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/JSURI/foreign.js
  function toRFC3896(input) {
    return input.replace(/[!'()*]/g, function(c) {
      return "%" + c.charCodeAt(0).toString(16);
    });
  }
  var _encodeFormURLComponent = function encode(fail4, succeed, input) {
    try {
      return succeed(toRFC3896(encodeURIComponent(input)).replace(/%20/g, "+"));
    } catch (err) {
      return fail4(err);
    }
  };

  // output/JSURI/index.js
  var encodeFormURLComponent = /* @__PURE__ */ function() {
    return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
  }();

  // output/Data.FormURLEncoded/index.js
  var apply8 = /* @__PURE__ */ apply(applyMaybe);
  var map46 = /* @__PURE__ */ map3(functorMaybe);
  var traverse4 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
  var toArray = function(v) {
    return v;
  };
  var encode2 = /* @__PURE__ */ function() {
    var encodePart = function(v) {
      if (v.value1 instanceof Nothing) {
        return encodeFormURLComponent(v.value0);
      }
      ;
      if (v.value1 instanceof Just) {
        return apply8(map46(function(key) {
          return function(val2) {
            return key + ("=" + val2);
          };
        })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
    };
    var $37 = map46(joinWith("&"));
    var $38 = traverse4(encodePart);
    return function($39) {
      return $37($38(toArray($39)));
    };
  }();

  // output/Data.HTTP.Method/index.js
  var OPTIONS = /* @__PURE__ */ function() {
    function OPTIONS2() {
    }
    ;
    OPTIONS2.value = new OPTIONS2();
    return OPTIONS2;
  }();
  var GET = /* @__PURE__ */ function() {
    function GET2() {
    }
    ;
    GET2.value = new GET2();
    return GET2;
  }();
  var HEAD = /* @__PURE__ */ function() {
    function HEAD2() {
    }
    ;
    HEAD2.value = new HEAD2();
    return HEAD2;
  }();
  var POST = /* @__PURE__ */ function() {
    function POST2() {
    }
    ;
    POST2.value = new POST2();
    return POST2;
  }();
  var PUT = /* @__PURE__ */ function() {
    function PUT2() {
    }
    ;
    PUT2.value = new PUT2();
    return PUT2;
  }();
  var DELETE = /* @__PURE__ */ function() {
    function DELETE2() {
    }
    ;
    DELETE2.value = new DELETE2();
    return DELETE2;
  }();
  var TRACE = /* @__PURE__ */ function() {
    function TRACE2() {
    }
    ;
    TRACE2.value = new TRACE2();
    return TRACE2;
  }();
  var CONNECT = /* @__PURE__ */ function() {
    function CONNECT2() {
    }
    ;
    CONNECT2.value = new CONNECT2();
    return CONNECT2;
  }();
  var PROPFIND = /* @__PURE__ */ function() {
    function PROPFIND2() {
    }
    ;
    PROPFIND2.value = new PROPFIND2();
    return PROPFIND2;
  }();
  var PROPPATCH = /* @__PURE__ */ function() {
    function PROPPATCH2() {
    }
    ;
    PROPPATCH2.value = new PROPPATCH2();
    return PROPPATCH2;
  }();
  var MKCOL = /* @__PURE__ */ function() {
    function MKCOL2() {
    }
    ;
    MKCOL2.value = new MKCOL2();
    return MKCOL2;
  }();
  var COPY = /* @__PURE__ */ function() {
    function COPY2() {
    }
    ;
    COPY2.value = new COPY2();
    return COPY2;
  }();
  var MOVE = /* @__PURE__ */ function() {
    function MOVE2() {
    }
    ;
    MOVE2.value = new MOVE2();
    return MOVE2;
  }();
  var LOCK = /* @__PURE__ */ function() {
    function LOCK2() {
    }
    ;
    LOCK2.value = new LOCK2();
    return LOCK2;
  }();
  var UNLOCK = /* @__PURE__ */ function() {
    function UNLOCK2() {
    }
    ;
    UNLOCK2.value = new UNLOCK2();
    return UNLOCK2;
  }();
  var PATCH = /* @__PURE__ */ function() {
    function PATCH2() {
    }
    ;
    PATCH2.value = new PATCH2();
    return PATCH2;
  }();
  var unCustomMethod = function(v) {
    return v;
  };
  var showMethod = {
    show: function(v) {
      if (v instanceof OPTIONS) {
        return "OPTIONS";
      }
      ;
      if (v instanceof GET) {
        return "GET";
      }
      ;
      if (v instanceof HEAD) {
        return "HEAD";
      }
      ;
      if (v instanceof POST) {
        return "POST";
      }
      ;
      if (v instanceof PUT) {
        return "PUT";
      }
      ;
      if (v instanceof DELETE) {
        return "DELETE";
      }
      ;
      if (v instanceof TRACE) {
        return "TRACE";
      }
      ;
      if (v instanceof CONNECT) {
        return "CONNECT";
      }
      ;
      if (v instanceof PROPFIND) {
        return "PROPFIND";
      }
      ;
      if (v instanceof PROPPATCH) {
        return "PROPPATCH";
      }
      ;
      if (v instanceof MKCOL) {
        return "MKCOL";
      }
      ;
      if (v instanceof COPY) {
        return "COPY";
      }
      ;
      if (v instanceof MOVE) {
        return "MOVE";
      }
      ;
      if (v instanceof LOCK) {
        return "LOCK";
      }
      ;
      if (v instanceof UNLOCK) {
        return "UNLOCK";
      }
      ;
      if (v instanceof PATCH) {
        return "PATCH";
      }
      ;
      throw new Error("Failed pattern match at Data.HTTP.Method (line 43, column 1 - line 59, column 23): " + [v.constructor.name]);
    }
  };
  var print = /* @__PURE__ */ either(/* @__PURE__ */ show(showMethod))(unCustomMethod);

  // output/Effect.Aff.Compat/index.js
  var fromEffectFnAff = function(v) {
    return makeAff(function(k) {
      return function __do2() {
        var v1 = v(function($9) {
          return k(Left.create($9))();
        }, function($10) {
          return k(Right.create($10))();
        });
        return function(e) {
          return makeAff(function(k2) {
            return function __do3() {
              v1(e, function($11) {
                return k2(Left.create($11))();
              }, function($12) {
                return k2(Right.create($12))();
              });
              return nonCanceler;
            };
          });
        };
      };
    });
  };

  // output/Foreign/foreign.js
  function tagOf(value2) {
    return Object.prototype.toString.call(value2).slice(8, -1);
  }
  var isArray = Array.isArray || function(value2) {
    return Object.prototype.toString.call(value2) === "[object Array]";
  };

  // output/Foreign/index.js
  var show8 = /* @__PURE__ */ show(showString);
  var show16 = /* @__PURE__ */ show(showInt);
  var ForeignError = /* @__PURE__ */ function() {
    function ForeignError2(value0) {
      this.value0 = value0;
    }
    ;
    ForeignError2.create = function(value0) {
      return new ForeignError2(value0);
    };
    return ForeignError2;
  }();
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var ErrorAtIndex = /* @__PURE__ */ function() {
    function ErrorAtIndex2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtIndex2.create = function(value0) {
      return function(value1) {
        return new ErrorAtIndex2(value0, value1);
      };
    };
    return ErrorAtIndex2;
  }();
  var ErrorAtProperty = /* @__PURE__ */ function() {
    function ErrorAtProperty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ErrorAtProperty2.create = function(value0) {
      return function(value1) {
        return new ErrorAtProperty2(value0, value1);
      };
    };
    return ErrorAtProperty2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var renderForeignError = function(v) {
    if (v instanceof ForeignError) {
      return v.value0;
    }
    ;
    if (v instanceof ErrorAtIndex) {
      return "Error at array index " + (show16(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof ErrorAtProperty) {
      return "Error at property " + (show8(v.value0) + (": " + renderForeignError(v.value1)));
    }
    ;
    if (v instanceof TypeMismatch) {
      return "Type mismatch: expected " + (v.value0 + (", found " + v.value1));
    }
    ;
    throw new Error("Failed pattern match at Foreign (line 78, column 1 - line 78, column 45): " + [v.constructor.name]);
  };
  var fail2 = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton7($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure112 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail2(dictMonad);
    return function(tag) {
      return function(value2) {
        if (tagOf(value2) === tag) {
          return pure112(unsafeFromForeign(value2));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value2)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value2.constructor.name]);
      };
    };
  };

  // output/Affjax/index.js
  var pure20 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
  var fail3 = /* @__PURE__ */ fail2(monadIdentity);
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var alt8 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var map47 = /* @__PURE__ */ map3(functorMaybe);
  var any3 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
  var eq15 = /* @__PURE__ */ eq(eqString);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var map116 = /* @__PURE__ */ map3(functorArray);
  var mapFlipped9 = /* @__PURE__ */ mapFlipped(functorAff);
  var $$try4 = /* @__PURE__ */ $$try(monadErrorAff);
  var pure110 = /* @__PURE__ */ pure(applicativeAff);
  var RequestContentError = /* @__PURE__ */ function() {
    function RequestContentError2(value0) {
      this.value0 = value0;
    }
    ;
    RequestContentError2.create = function(value0) {
      return new RequestContentError2(value0);
    };
    return RequestContentError2;
  }();
  var ResponseBodyError = /* @__PURE__ */ function() {
    function ResponseBodyError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ResponseBodyError2.create = function(value0) {
      return function(value1) {
        return new ResponseBodyError2(value0, value1);
      };
    };
    return ResponseBodyError2;
  }();
  var TimeoutError = /* @__PURE__ */ function() {
    function TimeoutError2() {
    }
    ;
    TimeoutError2.value = new TimeoutError2();
    return TimeoutError2;
  }();
  var RequestFailedError = /* @__PURE__ */ function() {
    function RequestFailedError2() {
    }
    ;
    RequestFailedError2.value = new RequestFailedError2();
    return RequestFailedError2;
  }();
  var XHROtherError = /* @__PURE__ */ function() {
    function XHROtherError2(value0) {
      this.value0 = value0;
    }
    ;
    XHROtherError2.create = function(value0) {
      return new XHROtherError2(value0);
    };
    return XHROtherError2;
  }();
  var request = function(driver2) {
    return function(req2) {
      var parseJSON = function(v2) {
        if (v2 === "") {
          return pure20(jsonEmptyObject);
        }
        ;
        return either(function($74) {
          return fail3(ForeignError.create($74));
        })(pure20)(jsonParser(v2));
      };
      var fromResponse = function() {
        if (req2.responseFormat instanceof $$ArrayBuffer) {
          return unsafeReadTagged2("ArrayBuffer");
        }
        ;
        if (req2.responseFormat instanceof Blob2) {
          return unsafeReadTagged2("Blob");
        }
        ;
        if (req2.responseFormat instanceof Document2) {
          return function(x2) {
            return alt8(unsafeReadTagged2("Document")(x2))(alt8(unsafeReadTagged2("XMLDocument")(x2))(unsafeReadTagged2("HTMLDocument")(x2)));
          };
        }
        ;
        if (req2.responseFormat instanceof Json2) {
          return composeKleisliFlipped2(function($75) {
            return req2.responseFormat.value0(parseJSON($75));
          })(unsafeReadTagged2("String"));
        }
        ;
        if (req2.responseFormat instanceof $$String2) {
          return unsafeReadTagged2("String");
        }
        ;
        if (req2.responseFormat instanceof Ignore) {
          return $$const(req2.responseFormat.value0(pure20(unit2)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req2.responseFormat.constructor.name]);
      }();
      var extractContent = function(v2) {
        if (v2 instanceof ArrayView) {
          return new Right(v2.value0(unsafeToForeign));
        }
        ;
        if (v2 instanceof Blob) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof Document) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof $$String) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormData) {
          return new Right(unsafeToForeign(v2.value0));
        }
        ;
        if (v2 instanceof FormURLEncoded) {
          return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map47(unsafeToForeign)(encode2(v2.value0)));
        }
        ;
        if (v2 instanceof Json) {
          return new Right(unsafeToForeign(stringify(v2.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 235, column 20 - line 250, column 69): " + [v2.constructor.name]);
      };
      var addHeader = function(mh) {
        return function(hs) {
          if (mh instanceof Just && !any3(on(eq15)(name2)(mh.value0))(hs)) {
            return snoc(hs)(mh.value0);
          }
          ;
          return hs;
        };
      };
      var headers = function(reqContent) {
        return addHeader(map47(ContentType.create)(bindFlipped6(toMediaType)(reqContent)))(addHeader(map47(Accept.create)(toMediaType2(req2.responseFormat)))(req2.headers));
      };
      var ajaxRequest = function(v2) {
        return {
          method: print(req2.method),
          url: req2.url,
          headers: map116(function(h) {
            return {
              field: name2(h),
              value: value(h)
            };
          })(headers(req2.content)),
          content: v2,
          responseType: toResponseType(req2.responseFormat),
          username: toNullable(req2.username),
          password: toNullable(req2.password),
          withCredentials: req2.withCredentials,
          timeout: fromMaybe(0)(map47(function(v1) {
            return v1;
          })(req2.timeout))
        };
      };
      var send = function(content) {
        return mapFlipped9($$try4(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content)))))(function(v2) {
          if (v2 instanceof Right) {
            var v1 = runExcept(fromResponse(v2.value0.body));
            if (v1 instanceof Left) {
              return new Left(new ResponseBodyError(head3(v1.value0), v2.value0));
            }
            ;
            if (v1 instanceof Right) {
              return new Right({
                body: v1.value0,
                headers: v2.value0.headers,
                status: v2.value0.status,
                statusText: v2.value0.statusText
              });
            }
            ;
            throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
          }
          ;
          if (v2 instanceof Left) {
            return new Left(function() {
              var message2 = message(v2.value0);
              var $61 = message2 === "AffjaxTimeoutErrorMessageIdent";
              if ($61) {
                return TimeoutError.value;
              }
              ;
              var $62 = message2 === "AffjaxRequestFailedMessageIdent";
              if ($62) {
                return RequestFailedError.value;
              }
              ;
              return new XHROtherError(v2.value0);
            }());
          }
          ;
          throw new Error("Failed pattern match at Affjax (line 207, column 144 - line 219, column 28): " + [v2.constructor.name]);
        });
      };
      if (req2.content instanceof Nothing) {
        return send(toNullable(Nothing.value));
      }
      ;
      if (req2.content instanceof Just) {
        var v = extractContent(req2.content.value0);
        if (v instanceof Right) {
          return send(toNullable(new Just(v.value0)));
        }
        ;
        if (v instanceof Left) {
          return pure110(new Left(new RequestContentError(v.value0)));
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 199, column 7 - line 203, column 48): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 195, column 3 - line 203, column 48): " + [req2.content.constructor.name]);
    };
  };
  var printError = function(v) {
    if (v instanceof RequestContentError) {
      return "There was a problem with the request content: " + v.value0;
    }
    ;
    if (v instanceof ResponseBodyError) {
      return "There was a problem with the response body: " + renderForeignError(v.value0);
    }
    ;
    if (v instanceof TimeoutError) {
      return "There was a problem making the request: timeout";
    }
    ;
    if (v instanceof RequestFailedError) {
      return "There was a problem making the request: request failed";
    }
    ;
    if (v instanceof XHROtherError) {
      return "There was a problem making the request: " + message(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Affjax (line 113, column 14 - line 123, column 66): " + [v.constructor.name]);
  };
  var defaultRequest = /* @__PURE__ */ function() {
    return {
      method: new Left(GET.value),
      url: "/",
      headers: [],
      content: Nothing.value,
      username: Nothing.value,
      password: Nothing.value,
      withCredentials: false,
      responseFormat: ignore,
      timeout: Nothing.value
    };
  }();

  // output/Affjax.Web/foreign.js
  var driver = {
    newXHR: function() {
      return new XMLHttpRequest();
    },
    fixupUrl: function(url) {
      return url || "/";
    }
  };

  // output/Affjax.Web/index.js
  var request2 = /* @__PURE__ */ request(driver);

  // output/Debug/foreign.js
  var req = typeof module === "undefined" ? void 0 : module.require;
  var util = function() {
    try {
      return req === void 0 ? void 0 : req("util");
    } catch (e) {
      return void 0;
    }
  }();
  function _trace(x2, k) {
    if (util !== void 0) {
      console.log(util.inspect(x2, { depth: null, colors: true }));
    } else {
      console.log(x2);
    }
    return k({});
  }
  var now2 = function() {
    var perf;
    if (typeof performance !== "undefined") {
      perf = performance;
    } else if (req) {
      try {
        perf = req("perf_hooks").performance;
      } catch (e) {
      }
    }
    return function() {
      return (perf || Date).now();
    };
  }();

  // output/Debug/index.js
  var trace = function() {
    return function(a) {
      return function(k) {
        return _trace(a, k);
      };
    };
  };

  // output/Primitive.Defs/index.js
  var union11 = /* @__PURE__ */ union7(asIntIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber);
  var union12 = /* @__PURE__ */ union7(asNumberIntOrNumber)(asNumberIntOrNumber)(asIntNumber)(asIntNumber);
  var unionStr2 = /* @__PURE__ */ unionStr(asBooleanBoolean)(asIntOrNumberString);
  var union22 = /* @__PURE__ */ union7(asBooleanBoolean)(asBooleanBoolean)(asIntNumber)(asIntNumber);
  var sub2 = /* @__PURE__ */ sub(ringInt);
  var div1 = /* @__PURE__ */ div(euclideanRingNumber);
  var trace2 = /* @__PURE__ */ trace();
  var binary2 = /* @__PURE__ */ binary(toFromInt$plusNumber)(toFromInt$plusNumber)(toFromInt$plusNumber);
  var binaryZero2 = /* @__PURE__ */ binaryZero(/* @__PURE__ */ isZero$plus(isZeroInt)(isZeroNumber))(toFromInt$plusNumber)(toFromInt$plusNumber);
  var binary1 = /* @__PURE__ */ binary(toFromEitherEitherIntNumb)(toFromEitherEitherIntNumb)(toFromBoolean);
  var binaryZero1 = /* @__PURE__ */ binaryZero(isZeroInt)(toFromInt)(toFromInt);
  var unary2 = /* @__PURE__ */ unary(toFromNumber)(toFromInt);
  var unary1 = /* @__PURE__ */ unary(toFromInt$plusNumber);
  var times = /* @__PURE__ */ union11(/* @__PURE__ */ mul(semiringInt))(/* @__PURE__ */ mul(semiringNumber));
  var rem2 = rem;
  var quot2 = quot;
  var pow3 = /* @__PURE__ */ union12(function(x2) {
    return function(y2) {
      return pow(toNumber(x2))(toNumber(y2));
    };
  })(pow);
  var plus = /* @__PURE__ */ union11(/* @__PURE__ */ add(semiringInt))(/* @__PURE__ */ add(semiringNumber));
  var numToStr = /* @__PURE__ */ union1(/* @__PURE__ */ show(showInt))(/* @__PURE__ */ show(showNumber));
  var notEquals = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ notEq(eqInt))(/* @__PURE__ */ notEq(eqNumber)))(/* @__PURE__ */ notEq(eqString));
  var mod4 = /* @__PURE__ */ mod(euclideanRingInt);
  var minus = /* @__PURE__ */ union11(sub2)(/* @__PURE__ */ sub(ringNumber));
  var matrixLookup = /* @__PURE__ */ function() {
    var fwd = function(v) {
      return function(v1) {
        return unsafeIndex4(unsafeIndex4(v.value0.value0)(v1.value0.value0 - 1 | 0))(v1.value1.value0 - 1 | 0);
      };
    };
    var bwd = function(v) {
      return function(v1) {
        return new Tuple(updateMatrix(v1.value1.value0.value0)(v1.value1.value1.value0)($$const(v))(new Tuple(new Tuple(v1.value0.value0.value0, new Tuple(v1.value0.value0.value1.value0, false)), new Tuple(v1.value0.value1.value0, false))), new Tuple(new Tuple(v1.value1.value0.value0, false), new Tuple(v1.value1.value1.value0, false)));
      };
    };
    return {
      fwd,
      bwd
    };
  }();
  var log3 = /* @__PURE__ */ union1(function($128) {
    return log(toNumber($128));
  })(log);
  var lessThanEquals = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ lessThanOrEq(ordInt))(/* @__PURE__ */ lessThanOrEq(ordNumber)))(/* @__PURE__ */ lessThanOrEq(ordString));
  var lessThan2 = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ lessThan(ordInt))(/* @__PURE__ */ lessThan(ordNumber)))(/* @__PURE__ */ lessThan(ordString));
  var greaterThanEquals = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ greaterThanOrEq(ordInt))(/* @__PURE__ */ greaterThanOrEq(ordNumber)))(/* @__PURE__ */ greaterThanOrEq(ordString));
  var greaterThan2 = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ greaterThan(ordInt))(/* @__PURE__ */ greaterThan(ordNumber)))(/* @__PURE__ */ greaterThan(ordString));
  var error_ = error2;
  var equals2 = /* @__PURE__ */ unionStr2(/* @__PURE__ */ union22(/* @__PURE__ */ eq(eqInt))(/* @__PURE__ */ eq(eqNumber)))(/* @__PURE__ */ eq(eqString));
  var divide = /* @__PURE__ */ union12(function(x2) {
    return function(y2) {
      return toNumber(x2) / toNumber(y2);
    };
  })(div1);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var dims = /* @__PURE__ */ function() {
    var fwd = function(v) {
      return new Tuple(v.value0.value1, v.value1);
    };
    var bwd = function(v) {
      return function(v1) {
        return new Tuple(new Tuple(v1.value0.value0, v.value0), v.value1);
      };
    };
    return {
      fwd,
      bwd
    };
  }();
  var debugLog = function(x2) {
    return trace2(x2)($$const(x2));
  };
  var concat3 = /* @__PURE__ */ append(semigroupString);
  var primitives = /* @__PURE__ */ function() {
    return fromFoldable3(ordString)(foldableArray)([new Tuple(":", new Constr3(false, cCons, Nil.value)), new Tuple("+", binary2(withInverse2(plus))), new Tuple("-", binary2(withInverse2(minus))), new Tuple("*", binaryZero2(withInverse2(times))), new Tuple("**", binaryZero2(withInverse2(pow3))), new Tuple("/", binaryZero2(withInverse2(divide))), new Tuple("==", binary1(withInverse2(equals2))), new Tuple("/=", binary1(withInverse2(notEquals))), new Tuple("<", binary1(withInverse2(lessThan2))), new Tuple(">", binary1(withInverse2(greaterThan2))), new Tuple("<=", binary1(withInverse2(lessThanEquals))), new Tuple(">=", binary1(withInverse2(greaterThanEquals))), new Tuple("++", binary(toFromString)(toFromString)(toFromString)(withInverse2(concat3))), new Tuple("!", binary(toFromArrayArrayValBoolea)(toFromInt$215Boolean$215Int$215Boo)(toFromValBoolean)(matrixLookup)), new Tuple("div", binaryZero1(withInverse2(div3))), new Tuple("mod", binaryZero1(withInverse2(mod4))), new Tuple("quot", binaryZero1(withInverse2(quot2))), new Tuple("rem", binaryZero1(withInverse2(rem2))), new Tuple("ceiling", unary2(withInverse1(ceil2))), new Tuple("debugLog", unary(toFromValBoolean)(toFromValBoolean)(withInverse1(debugLog))), new Tuple("dims", unary(toFromArrayArrayValBoolea)(toFromInt$215Boolean$215Int$215Boo)(dims)), new Tuple("error", unary(toFromString)(toFromValBoolean)(withInverse1(error_))), new Tuple("floor", unary2(withInverse1(floor2))), new Tuple("log", unary1(toFromNumber)(withInverse1(log3))), new Tuple("numToStr", unary1(toFromString)(withInverse1(numToStr)))]);
  }();

  // output/Module/index.js
  var bimap6 = /* @__PURE__ */ bimap2(bifunctorEither);
  var show9 = /* @__PURE__ */ show(showParseError);
  var identity21 = /* @__PURE__ */ identity4(categoryFn);
  var bind16 = /* @__PURE__ */ bind(bindAff);
  var pure21 = /* @__PURE__ */ pure(applicativeAff);
  var mapFlipped10 = /* @__PURE__ */ mapFlipped(functorEither);
  var bind17 = /* @__PURE__ */ bind(bindEither);
  var mapFlipped14 = /* @__PURE__ */ mapFlipped(functorAff);
  var semigroupFile = semigroupString;
  var resourceServerUrl = ".";
  var parse = function(src) {
    var $28 = bimap6(show9)(identity21);
    var $29 = runParser(src);
    return function($30) {
      return $28($29($30));
    };
  };
  var loadFile = function(v) {
    return function(v1) {
      var url = resourceServerUrl + ("/" + (v + ("/" + (v1 + ".fld"))));
      return bind16(request2({
        method: new Left(GET.value),
        url,
        headers: defaultRequest.headers,
        content: defaultRequest.content,
        username: defaultRequest.username,
        password: defaultRequest.password,
        withCredentials: defaultRequest.withCredentials,
        responseFormat: string2,
        timeout: defaultRequest.timeout
      }))(function(result) {
        if (result instanceof Left) {
          return error2(printError(result.value0));
        }
        ;
        if (result instanceof Right) {
          return pure21(result.value0.body);
        }
        ;
        throw new Error("Failed pattern match at Module (line 39, column 4 - line 41, column 43): " + [result.constructor.name]);
      });
    };
  };
  var loadModule = function(file) {
    return function(\u03B3) {
      return bind16(loadFile("fluid/lib")(file))(function(src) {
        return pure21(successful(mapFlipped10(bind17(bind17(parse(src)(module_))(desugarModuleFwd))(eval_module(\u03B3)))(function(v) {
          return append6(\u03B3)(v);
        })));
      });
    };
  };
  var parseProgram = function(folder) {
    return function(file) {
      return mapFlipped14(loadFile(folder)(file))(function() {
        var $31 = flip(parse)(program);
        return function($32) {
          return successful($31($32));
        };
      }());
    };
  };
  var open = /* @__PURE__ */ parseProgram("fluid/example");
  var defaultImports = /* @__PURE__ */ bind16(/* @__PURE__ */ bind16(/* @__PURE__ */ loadModule("prelude")(primitives))(/* @__PURE__ */ loadModule("graphics")))(/* @__PURE__ */ loadModule("convolution"));
  var openDatasetAs = function(file) {
    return function(x2) {
      return bind16(parseProgram("fluid")(file))(function(s) {
        return bind16(defaultImports)(function(\u03B3) {
          var v = successful(bind17(desugarFwd(s))($$eval(\u03B3)));
          return pure21(new Tuple(\u03B3, singleton4(x2)(v.value1)));
        });
      });
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event2) {
        return fn(event2)();
      };
    };
  }

  // output/App.Fig/index.js
  var eq16 = /* @__PURE__ */ eq(eqCtr);
  var from6 = /* @__PURE__ */ from2(reflectMapVarValBooleanBa1)();
  var from13 = /* @__PURE__ */ from2(reflectMapVarValBooleanLi1)();
  var map48 = /* @__PURE__ */ map3(functorArray);
  var from23 = /* @__PURE__ */ from2(reflectArray)();
  var match_fwd5 = /* @__PURE__ */ match_fwd(toFromArrayArrayValBoolea);
  var map117 = /* @__PURE__ */ map3(functorEither);
  var lookup5 = /* @__PURE__ */ lookup(ordString);
  var sequence4 = /* @__PURE__ */ sequence(traversableArray)(applicativeEither);
  var bind18 = /* @__PURE__ */ bind(bindEither);
  var pure23 = /* @__PURE__ */ pure(applicativeEither);
  var append9 = /* @__PURE__ */ append(semigroupFile);
  var bind19 = /* @__PURE__ */ bind(bindAff);
  var apply9 = /* @__PURE__ */ apply(applyAff);
  var map212 = /* @__PURE__ */ map3(functorAff);
  var pure111 = /* @__PURE__ */ pure(applicativeAff);
  var apply1 = /* @__PURE__ */ apply(applyEither);
  var mapFlipped11 = /* @__PURE__ */ mapFlipped(functorAff);
  var neg6 = /* @__PURE__ */ neg(joinSemilatticeValBoolean);
  var neg1 = /* @__PURE__ */ neg(/* @__PURE__ */ joinSemilatticeMap(keyString)(slicesValBoolean));
  var botOf4 = /* @__PURE__ */ botOf(/* @__PURE__ */ boundedSlicesMap(keyString)(boundedSlicesValBoolean));
  var map310 = /* @__PURE__ */ map3(functorExpr);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var identity22 = /* @__PURE__ */ identity4(categoryFn);
  var sequence_2 = /* @__PURE__ */ sequence_(applicativeEffect)(foldableArray);
  var length8 = /* @__PURE__ */ length2(foldableArray)(semiringInt);
  var MatrixFig = /* @__PURE__ */ function() {
    function MatrixFig2(value0) {
      this.value0 = value0;
    }
    ;
    MatrixFig2.create = function(value0) {
      return new MatrixFig2(value0);
    };
    return MatrixFig2;
  }();
  var EnergyTableView = /* @__PURE__ */ function() {
    function EnergyTableView2(value0) {
      this.value0 = value0;
    }
    ;
    EnergyTableView2.create = function(value0) {
      return new EnergyTableView2(value0);
    };
    return EnergyTableView2;
  }();
  var LineChartFig = /* @__PURE__ */ function() {
    function LineChartFig2(value0) {
      this.value0 = value0;
    }
    ;
    LineChartFig2.create = function(value0) {
      return new LineChartFig2(value0);
    };
    return LineChartFig2;
  }();
  var BarChartFig = /* @__PURE__ */ function() {
    function BarChartFig2(value0) {
      this.value0 = value0;
    }
    ;
    BarChartFig2.create = function(value0) {
      return new BarChartFig2(value0);
    };
    return BarChartFig2;
  }();
  var view = function(v) {
    return function(v1) {
      if (v1 instanceof Constr3 && (v1.value2 instanceof Cons && (v1.value2.value1 instanceof Nil && eq16(v1.value1)(cBarChart)))) {
        return new BarChartFig(record(from6)(v1.value2.value0));
      }
      ;
      if (v1 instanceof Constr3 && (v1.value2 instanceof Cons && (v1.value2.value1 instanceof Nil && eq16(v1.value1)(cLineChart)))) {
        return new LineChartFig(record(from13)(v1.value2.value0));
      }
      ;
      if (v1 instanceof Constr3 && (eq16(v1.value1)(cNil) || eq16(v1.value1)(cCons))) {
        return new EnergyTableView({
          title: v,
          table: map48(record(energyRecord))(from23(v1))
        });
      }
      ;
      if (v1 instanceof Matrix3) {
        return new MatrixFig({
          title: v,
          matrix: matrixRep(fst(match_fwd5(v1)))
        });
      }
      ;
      return error2(absurd2);
    };
  };
  var varView = function(x2) {
    return function(\u03B3) {
      return map117(view(x2))(orElse(absurd2)(lookup5(x2)(\u03B3)));
    };
  };
  var valViews = function(\u03B3) {
    return function(xs) {
      return sequence4(map48(flip(varView)(\u03B3))(xs));
    };
  };
  var splitDefs = function(\u03B30) {
    return function(s$prime) {
      var unpack = function() {
        return function(v2) {
          if (v2 instanceof LetRec2) {
            return new Tuple(new Right(v2.value0), v2.value1);
          }
          ;
          if (v2 instanceof Let2) {
            return new Tuple(new Left(v2.value0), v2.value1);
          }
          ;
          throw new Error("Failed pattern match at App.Fig (line 75, column 10 - line 75, column 81): " + [v2.constructor.name]);
        };
      };
      var unpack1 = unpack();
      var v = unpack1(s$prime);
      return bind18(bind18(desugarModuleFwd(new Module2(singleton3(v.value0))))(eval_module(\u03B30)))(function(\u03B3) {
        return pure23({
          \u03B3,
          s: v.value1
        });
      });
    };
  };
  var loadLinkFig = function(v) {
    var v1 = new Tuple(append9("linking/")(v.file1), append9("linking/")(v.file2));
    return bind19(openDatasetAs(append9("example/")(append9("linking/")(v.dataFile)))(v.x))(function(v2) {
      return bind19(apply9(map212(Tuple.create)(open(v1.value0)))(open(v1.value1)))(function(v3) {
        return pure111(successful(bind18(apply1(map117(Tuple.create)(desugarFwd(v3.value0)))(desugarFwd(v3.value1)))(function(v4) {
          return bind18($$eval(append6(v2.value0)(v2.value1))(v4.value0))(function(v5) {
            return bind18($$eval(append6(v2.value0)(v2.value1))(v4.value1))(function(v6) {
              return bind18(orElse(absurd2)(lookup5(v.x)(v2.value1)))(function(v0) {
                return pure23({
                  spec: v,
                  \u03B30: v2.value0,
                  \u03B3: v2.value1,
                  s1: v3.value0,
                  s2: v3.value1,
                  e1: v4.value0,
                  e2: v4.value1,
                  t1: v5.value0,
                  t2: v6.value0,
                  v1: v5.value1,
                  v2: v6.value1,
                  v0
                });
              });
            });
          });
        })));
      });
    });
  };
  var loadFig = function(v) {
    return bind19(openDatasetAs("example/linking/renewables")("data"))(function(v1) {
      return mapFlipped11(open(v.file))(function(s$prime) {
        return successful(bind18(splitDefs(append6(v1.value0)(v1.value1))(s$prime))(function(v2) {
          return bind18(desugarFwd(v2.s))(function(e) {
            var \u03B30\u03B3 = append6(append6(v1.value0)(v1.value1))(v2.\u03B3);
            return bind18($$eval(\u03B30\u03B3)(e))(function(v3) {
              return pure23({
                spec: v,
                \u03B30: v1.value0,
                \u03B3: append6(v1.value1)(v2.\u03B3),
                s: v2.s,
                e,
                t: v3.value0,
                v: v3.value1
              });
            });
          });
        }));
      });
    });
  };
  var linkResult = function(x2) {
    return function(\u03B30) {
      return function(e22) {
        return function(t1) {
          return function(t2) {
            return function(v1) {
              var v = evalBwd(v1)(t1);
              var v2 = append_inv(singleton6(x2))(v.value0.value0);
              return bind18(orElse(absurd2)(lookup5(x2)(v2.value1)))(function(v0$prime) {
                var v2$prime = neg6(evalFwd(neg1(append6(botOf4(\u03B30))(v2.value1)))(map310($$const(true))(e22))(true)(t2));
                return pure23({
                  "v'": v2$prime,
                  "v0'": v0$prime
                });
              });
            };
          };
        };
      };
    };
  };
  var figViews = function(v) {
    return function(\u03B4v) {
      var v2 = evalBwd(\u03B4v(v.v))(v.t);
      var v$prime = evalFwd(v2.value0.value0)(v2.value0.value1)(v2.value1)(v.t);
      return bind18(valViews(v2.value0.value0)(v.spec.xs))(function(views) {
        return pure23(new Tuple(view("output")(v$prime), views));
      });
    };
  };
  var drawView = function(divId) {
    return function(onSel) {
      return function(n) {
        return function(v) {
          if (v instanceof MatrixFig) {
            return bindFlipped7(drawMatrix(divId)(n)(v.value0))(eventListener(function($193) {
              return onSel(matrixViewHandler($193));
            }));
          }
          ;
          if (v instanceof EnergyTableView) {
            return bindFlipped7(drawTable(divId)(n)(v.value0))(eventListener(function($194) {
              return onSel(tableViewHandler($194));
            }));
          }
          ;
          if (v instanceof LineChartFig) {
            return bindFlipped7(drawLineChart(divId)(n)(v.value0))(eventListener(function($195) {
              return onSel(lineChartHandler($195));
            }));
          }
          ;
          if (v instanceof BarChartFig) {
            return bindFlipped7(drawBarChart(divId)(n)(v.value0))(eventListener(function($196) {
              return onSel(barChartHandler($196));
            }));
          }
          ;
          throw new Error("Failed pattern match at App.Fig (line 44, column 1 - line 44, column 58): " + [divId.constructor.name, onSel.constructor.name, n.constructor.name, v.constructor.name]);
        };
      };
    };
  };
  var drawLinkFig = function(v) {
    return function(\u03B4v) {
      return function __do2() {
        log2("Redrawing " + v.spec.divId)();
        var v3 = successful(function() {
          if (\u03B4v instanceof Left) {
            var v1$prime = \u03B4v.value0(v.v1);
            return bind18(linkResult(v.spec.x)(v.\u03B30)(v.e2)(v.t1)(v.t2)(v1$prime))(function(v4) {
              return pure23(new Tuple(new Tuple(new Tuple(new Tuple(v1$prime, v4["v'"]), $$const(v1$prime)), identity22), v4["v0'"]));
            });
          }
          ;
          if (\u03B4v instanceof Right) {
            var v2$prime = \u03B4v.value0(v.v2);
            return bind18(linkResult(v.spec.x)(v.\u03B30)(v.e1)(v.t2)(v.t1)(v2$prime))(function(v4) {
              return pure23(new Tuple(new Tuple(new Tuple(new Tuple(v4["v'"], v2$prime), identity22), $$const(v2$prime)), v4["v0'"]));
            });
          }
          ;
          throw new Error("Failed pattern match at App.Fig (line 126, column 48 - line 134, column 57): " + [\u03B4v.constructor.name]);
        }());
        drawView(v.spec.divId)(function(selector) {
          return drawLinkFig(v)(new Left(function($197) {
            return selector(v3.value0.value0.value1($197));
          }));
        })(2)(view("left view")(v3.value0.value0.value0.value0))();
        drawView(v.spec.divId)(function(selector) {
          return drawLinkFig(v)(new Right(function($198) {
            return selector(v3.value0.value1($198));
          }));
        })(0)(view("right view")(v3.value0.value0.value0.value1))();
        return drawView(v.spec.divId)(doNothing)(1)(view("common data")(v3.value1))();
      };
    };
  };
  var drawFig = function(v) {
    return function(\u03B4v) {
      return function __do2() {
        log2("Redrawing " + v.spec.divId)();
        var v1 = successful(figViews(v)(\u03B4v));
        sequence_2(map48(uncurry(drawView(v.spec.divId)(doNothing)))(zip(range(0)(length8(v1.value1) - 1 | 0))(v1.value1)))();
        return drawView(v.spec.divId)(function(selector) {
          return drawFig(v)(function($199) {
            return selector(\u03B4v($199));
          });
        })(length8(v1.value1))(v1.value0)();
      };
    };
  };

  // output/App.Main/index.js
  var sequence5 = /* @__PURE__ */ sequence(traversableArray)(applicativeAff);
  var show10 = /* @__PURE__ */ show(showError);
  var sequence_3 = /* @__PURE__ */ sequence_(applicativeEffect)(foldableArray);
  var map49 = /* @__PURE__ */ map3(functorArray);
  var botOf5 = /* @__PURE__ */ botOf(boundedSlicesValBoolean);
  var linkingFig1 = {
    divId: "fig-1",
    file1: "bar-chart",
    file2: "line-chart",
    dataFile: "renewables",
    x: "data"
  };
  var fig2 = {
    divId: "fig-conv-2",
    file: "slicing/conv-emboss-wrap",
    xs: ["image", "filter"]
  };
  var fig1 = {
    divId: "fig-conv-1",
    file: "slicing/conv-emboss",
    xs: ["image", "filter"]
  };
  var drawLinkFigs = function(loadFigs) {
    return flip(runAff_)(sequence5(loadFigs))(function(v) {
      if (v instanceof Left) {
        return log2(show10(v.value0));
      }
      ;
      if (v instanceof Right) {
        return sequence_3(map49(flip(drawLinkFig)(new Left(botOf5)))(v.value0));
      }
      ;
      throw new Error("Failed pattern match at App.Main (line 40, column 4 - line 42, column 73): " + [v.constructor.name]);
    });
  };
  var drawFigs = function(loadFigs) {
    return flip(runAff_)(sequence5(loadFigs))(function(v) {
      if (v instanceof Left) {
        return log2(show10(v.value0));
      }
      ;
      if (v instanceof Right) {
        return sequence_3(map49(flip(drawFig)(botOf5))(v.value0));
      }
      ;
      throw new Error("Failed pattern match at App.Main (line 47, column 4 - line 49, column 60): " + [v.constructor.name]);
    });
  };
  var main = function __do() {
    drawFigs([loadFig(fig1), loadFig(fig2)])();
    return drawLinkFigs([loadLinkFig(linkingFig1)])();
  };

  // <stdin>
  main();
})();

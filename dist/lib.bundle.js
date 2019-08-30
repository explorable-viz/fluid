var Flooid =
/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./src/Lib.ts");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./node_modules/moo/moo.js":
/*!*********************************!*\
  !*** ./node_modules/moo/moo.js ***!
  \*********************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

var __WEBPACK_AMD_DEFINE_FACTORY__, __WEBPACK_AMD_DEFINE_ARRAY__, __WEBPACK_AMD_DEFINE_RESULT__;(function(root, factory) {
  if (true) {
    !(__WEBPACK_AMD_DEFINE_ARRAY__ = [], __WEBPACK_AMD_DEFINE_FACTORY__ = (factory),
				__WEBPACK_AMD_DEFINE_RESULT__ = (typeof __WEBPACK_AMD_DEFINE_FACTORY__ === 'function' ?
				(__WEBPACK_AMD_DEFINE_FACTORY__.apply(exports, __WEBPACK_AMD_DEFINE_ARRAY__)) : __WEBPACK_AMD_DEFINE_FACTORY__),
				__WEBPACK_AMD_DEFINE_RESULT__ !== undefined && (module.exports = __WEBPACK_AMD_DEFINE_RESULT__)) /* global define */
  } else {}
}(this, function() {
  'use strict';

  var hasOwnProperty = Object.prototype.hasOwnProperty
  var toString = Object.prototype.toString
  var hasSticky = typeof new RegExp().sticky === 'boolean'

  /***************************************************************************/

  function isRegExp(o) { return o && toString.call(o) === '[object RegExp]' }
  function isObject(o) { return o && typeof o === 'object' && !isRegExp(o) && !Array.isArray(o) }

  function reEscape(s) {
    return s.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&')
  }
  function reGroups(s) {
    var re = new RegExp('|' + s)
    return re.exec('').length - 1
  }
  function reCapture(s) {
    return '(' + s + ')'
  }
  function reUnion(regexps) {
    if (!regexps.length) return '(?!)'
    var source =  regexps.map(function(s) {
      return "(?:" + s + ")"
    }).join('|')
    return "(?:" + source + ")"
  }

  function regexpOrLiteral(obj) {
    if (typeof obj === 'string') {
      return '(?:' + reEscape(obj) + ')'

    } else if (isRegExp(obj)) {
      // TODO: consider /u support
      if (obj.ignoreCase) throw new Error('RegExp /i flag not allowed')
      if (obj.global) throw new Error('RegExp /g flag is implied')
      if (obj.sticky) throw new Error('RegExp /y flag is implied')
      if (obj.multiline) throw new Error('RegExp /m flag is implied')
      if (obj.unicode) throw new Error('RegExp /u flag is not allowed')
      return obj.source

    } else {
      throw new Error('Not a pattern: ' + obj)
    }
  }

  function objectToRules(object) {
    var keys = Object.getOwnPropertyNames(object)
    var result = []
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      var thing = object[key]
      var rules = [].concat(thing)
      if (key === 'include') {
        for (var j = 0; j < rules.length; j++) {
          result.push({include: rules[j]})
        }
        continue
      }
      var match = []
      rules.forEach(function(rule) {
        if (isObject(rule)) {
          if (match.length) result.push(ruleOptions(key, match))
          result.push(ruleOptions(key, rule))
          match = []
        } else {
          match.push(rule)
        }
      })
      if (match.length) result.push(ruleOptions(key, match))
    }
    return result
  }

  function arrayToRules(array) {
    var result = []
    for (var i = 0; i < array.length; i++) {
      var obj = array[i]
      if (obj.include) {
        var include = [].concat(obj.include)
        for (var j = 0; j < include.length; j++) {
          result.push({include: include[j]})
        }
        continue
      }
      if (!obj.type) {
        throw new Error('Rule has no type: ' + JSON.stringify(obj))
      }
      result.push(ruleOptions(obj.type, obj))
    }
    return result
  }

  function ruleOptions(type, obj) {
    if (!isObject(obj)) {
      obj = { match: obj }
    }
    if (obj.include) {
      throw new Error('Matching rules cannot also include states')
    }

    // nb. error and fallback imply lineBreaks
    var options = {
      defaultType: type,
      lineBreaks: !!obj.error || !!obj.fallback,
      pop: false,
      next: null,
      push: null,
      error: false,
      fallback: false,
      value: null,
      type: null,
      shouldThrow: false,
    }

    // Avoid Object.assign(), so we support IE9+
    for (var key in obj) {
      if (hasOwnProperty.call(obj, key)) {
        options[key] = obj[key]
      }
    }

    // type transform cannot be a string
    if (typeof options.type === 'string' && type !== options.type) {
      throw new Error("Type transform cannot be a string (type '" + options.type + "' for token '" + type + "')")
    }

    // convert to array
    var match = options.match
    options.match = Array.isArray(match) ? match : match ? [match] : []
    options.match.sort(function(a, b) {
      return isRegExp(a) && isRegExp(b) ? 0
           : isRegExp(b) ? -1 : isRegExp(a) ? +1 : b.length - a.length
    })
    return options
  }

  function toRules(spec) {
    return Array.isArray(spec) ? arrayToRules(spec) : objectToRules(spec)
  }

  var defaultErrorRule = ruleOptions('error', {lineBreaks: true, shouldThrow: true})
  function compileRules(rules, hasStates) {
    var errorRule = null
    var fast = Object.create(null)
    var fastAllowed = true
    var groups = []
    var parts = []

    // If there is a fallback rule, then disable fast matching
    for (var i = 0; i < rules.length; i++) {
      if (rules[i].fallback) {
        fastAllowed = false
      }
    }

    for (var i = 0; i < rules.length; i++) {
      var options = rules[i]

      if (options.include) {
        // all valid inclusions are removed by states() preprocessor
        throw new Error('Inheritance is not allowed in stateless lexers')
      }

      if (options.error || options.fallback) {
        // errorRule can only be set once
        if (errorRule) {
          if (!options.fallback === !errorRule.fallback) {
            throw new Error("Multiple " + (options.fallback ? "fallback" : "error") + " rules not allowed (for token '" + options.defaultType + "')")
          } else {
            throw new Error("fallback and error are mutually exclusive (for token '" + options.defaultType + "')")
          }
        }
        errorRule = options
      }

      var match = options.match
      if (fastAllowed) {
        while (match.length && typeof match[0] === 'string' && match[0].length === 1) {
          var word = match.shift()
          fast[word.charCodeAt(0)] = options
        }
      }

      // Warn about inappropriate state-switching options
      if (options.pop || options.push || options.next) {
        if (!hasStates) {
          throw new Error("State-switching options are not allowed in stateless lexers (for token '" + options.defaultType + "')")
        }
        if (options.fallback) {
          throw new Error("State-switching options are not allowed on fallback tokens (for token '" + options.defaultType + "')")
        }
      }

      // Only rules with a .match are included in the RegExp
      if (match.length === 0) {
        continue
      }
      fastAllowed = false

      groups.push(options)

      // convert to RegExp
      var pat = reUnion(match.map(regexpOrLiteral))

      // validate
      var regexp = new RegExp(pat)
      if (regexp.test("")) {
        throw new Error("RegExp matches empty string: " + regexp)
      }
      var groupCount = reGroups(pat)
      if (groupCount > 0) {
        throw new Error("RegExp has capture groups: " + regexp + "\nUse (?: … ) instead")
      }

      // try and detect rules matching newlines
      if (!options.lineBreaks && regexp.test('\n')) {
        throw new Error('Rule should declare lineBreaks: ' + regexp)
      }

      // store regex
      parts.push(reCapture(pat))
    }


    // If there's no fallback rule, use the sticky flag so we only look for
    // matches at the current index.
    //
    // If we don't support the sticky flag, then fake it using an irrefutable
    // match (i.e. an empty pattern).
    var fallbackRule = errorRule && errorRule.fallback
    var flags = hasSticky && !fallbackRule ? 'ym' : 'gm'
    var suffix = hasSticky || fallbackRule ? '' : '|'
    var combined = new RegExp(reUnion(parts) + suffix, flags)

    return {regexp: combined, groups: groups, fast: fast, error: errorRule || defaultErrorRule}
  }

  function compile(rules) {
    var result = compileRules(toRules(rules))
    return new Lexer({start: result}, 'start')
  }

  function checkStateGroup(g, name, map) {
    var state = g && (g.push || g.next)
    if (state && !map[state]) {
      throw new Error("Missing state '" + state + "' (in token '" + g.defaultType + "' of state '" + name + "')")
    }
    if (g && g.pop && +g.pop !== 1) {
      throw new Error("pop must be 1 (in token '" + g.defaultType + "' of state '" + name + "')")
    }
  }
  function compileStates(states, start) {
    var all = states.$all ? toRules(states.$all) : []
    delete states.$all

    var keys = Object.getOwnPropertyNames(states)
    if (!start) start = keys[0]

    var ruleMap = Object.create(null)
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      ruleMap[key] = toRules(states[key]).concat(all)
    }
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      var rules = ruleMap[key]
      var included = Object.create(null)
      for (var j = 0; j < rules.length; j++) {
        var rule = rules[j]
        if (!rule.include) continue
        var splice = [j, 1]
        if (rule.include !== key && !included[rule.include]) {
          included[rule.include] = true
          var newRules = ruleMap[rule.include]
          if (!newRules) {
            throw new Error("Cannot include nonexistent state '" + rule.include + "' (in state '" + key + "')")
          }
          for (var k = 0; k < newRules.length; k++) {
            var newRule = newRules[k]
            if (rules.indexOf(newRule) !== -1) continue
            splice.push(newRule)
          }
        }
        rules.splice.apply(rules, splice)
        j--
      }
    }

    var map = Object.create(null)
    for (var i = 0; i < keys.length; i++) {
      var key = keys[i]
      map[key] = compileRules(ruleMap[key], true)
    }

    for (var i = 0; i < keys.length; i++) {
      var name = keys[i]
      var state = map[name]
      var groups = state.groups
      for (var j = 0; j < groups.length; j++) {
        checkStateGroup(groups[j], name, map)
      }
      var fastKeys = Object.getOwnPropertyNames(state.fast)
      for (var j = 0; j < fastKeys.length; j++) {
        checkStateGroup(state.fast[fastKeys[j]], name, map)
      }
    }

    return new Lexer(map, start)
  }

  function keywordTransform(map) {
    var reverseMap = Object.create(null)
    var byLength = Object.create(null)
    var types = Object.getOwnPropertyNames(map)
    for (var i = 0; i < types.length; i++) {
      var tokenType = types[i]
      var item = map[tokenType]
      var keywordList = Array.isArray(item) ? item : [item]
      keywordList.forEach(function(keyword) {
        (byLength[keyword.length] = byLength[keyword.length] || []).push(keyword)
        if (typeof keyword !== 'string') {
          throw new Error("keyword must be string (in keyword '" + tokenType + "')")
        }
        reverseMap[keyword] = tokenType
      })
    }

    // fast string lookup
    // https://jsperf.com/string-lookups
    function str(x) { return JSON.stringify(x) }
    var source = ''
    source += 'switch (value.length) {\n'
    for (var length in byLength) {
      var keywords = byLength[length]
      source += 'case ' + length + ':\n'
      source += 'switch (value) {\n'
      keywords.forEach(function(keyword) {
        var tokenType = reverseMap[keyword]
        source += 'case ' + str(keyword) + ': return ' + str(tokenType) + '\n'
      })
      source += '}\n'
    }
    source += '}\n'
    return Function('value', source) // type
  }

  /***************************************************************************/

  var Lexer = function(states, state) {
    this.startState = state
    this.states = states
    this.buffer = ''
    this.stack = []
    this.reset()
  }

  Lexer.prototype.reset = function(data, info) {
    this.buffer = data || ''
    this.index = 0
    this.line = info ? info.line : 1
    this.col = info ? info.col : 1
    this.queuedToken = info ? info.queuedToken : null
    this.queuedThrow = info ? info.queuedThrow : null
    this.setState(info ? info.state : this.startState)
    this.stack = info && info.stack ? info.stack.slice() : []
    return this
  }

  Lexer.prototype.save = function() {
    return {
      line: this.line,
      col: this.col,
      state: this.state,
      stack: this.stack.slice(),
      queuedToken: this.queuedToken,
      queuedThrow: this.queuedThrow,
    }
  }

  Lexer.prototype.setState = function(state) {
    if (!state || this.state === state) return
    this.state = state
    var info = this.states[state]
    this.groups = info.groups
    this.error = info.error
    this.re = info.regexp
    this.fast = info.fast
  }

  Lexer.prototype.popState = function() {
    this.setState(this.stack.pop())
  }

  Lexer.prototype.pushState = function(state) {
    this.stack.push(this.state)
    this.setState(state)
  }

  var eat = hasSticky ? function(re, buffer) { // assume re is /y
    return re.exec(buffer)
  } : function(re, buffer) { // assume re is /g
    var match = re.exec(buffer)
    // will always match, since we used the |(?:) trick
    if (match[0].length === 0) {
      return null
    }
    return match
  }

  Lexer.prototype._getGroup = function(match) {
    var groupCount = this.groups.length
    for (var i = 0; i < groupCount; i++) {
      if (match[i + 1] !== undefined) {
        return this.groups[i]
      }
    }
    throw new Error('Cannot find token type for matched text')
  }

  function tokenToString() {
    return this.value
  }

  Lexer.prototype.next = function() {
    var index = this.index

    // If a fallback token matched, we don't need to re-run the RegExp
    if (this.queuedGroup) {
      var token = this._token(this.queuedGroup, this.queuedText, index)
      this.queuedGroup = null
      this.queuedText = ""
      return token
    }

    var buffer = this.buffer
    if (index === buffer.length) {
      return // EOF
    }

    // Fast matching for single characters
    var group = this.fast[buffer.charCodeAt(index)]
    if (group) {
      return this._token(group, buffer.charAt(index), index)
    }

    // Execute RegExp
    var re = this.re
    re.lastIndex = index
    var match = eat(re, buffer)

    // Error tokens match the remaining buffer
    var error = this.error
    if (match == null) {
      return this._token(error, buffer.slice(index, buffer.length), index)
    }

    var group = this._getGroup(match)
    var text = match[0]

    if (error.fallback && match.index !== index) {
      this.queuedGroup = group
      this.queuedText = text

      // Fallback tokens contain the unmatched portion of the buffer
      return this._token(error, buffer.slice(index, match.index), index)
    }

    return this._token(group, text, index)
  }

  Lexer.prototype._token = function(group, text, offset) {
    // count line breaks
    var lineBreaks = 0
    if (group.lineBreaks) {
      var matchNL = /\n/g
      var nl = 1
      if (text === '\n') {
        lineBreaks = 1
      } else {
        while (matchNL.exec(text)) { lineBreaks++; nl = matchNL.lastIndex }
      }
    }

    var token = {
      type: (typeof group.type === 'function' && group.type(text)) || group.defaultType,
      value: typeof group.value === 'function' ? group.value(text) : text,
      text: text,
      toString: tokenToString,
      offset: offset,
      lineBreaks: lineBreaks,
      line: this.line,
      col: this.col,
    }
    // nb. adding more props to token object will make V8 sad!

    var size = text.length
    this.index += size
    this.line += lineBreaks
    if (lineBreaks !== 0) {
      this.col = size - nl + 1
    } else {
      this.col += size
    }

    // throw, if no rule with {error: true}
    if (group.shouldThrow) {
      throw new Error(this.formatError(token, "invalid syntax"))
    }

    if (group.pop) this.popState()
    else if (group.push) this.pushState(group.push)
    else if (group.next) this.setState(group.next)

    return token
  }

  if (typeof Symbol !== 'undefined' && Symbol.iterator) {
    var LexerIterator = function(lexer) {
      this.lexer = lexer
    }

    LexerIterator.prototype.next = function() {
      var token = this.lexer.next()
      return {value: token, done: !token}
    }

    LexerIterator.prototype[Symbol.iterator] = function() {
      return this
    }

    Lexer.prototype[Symbol.iterator] = function() {
      return new LexerIterator(this)
    }
  }

  Lexer.prototype.formatError = function(token, message) {
    var value = token.text
    var index = token.offset
    var eol = token.lineBreaks ? value.indexOf('\n') : value.length
    var start = Math.max(0, index - token.col + 1)
    var firstLine = this.buffer.substring(start, index + eol)
    message += " at line " + token.line + " col " + token.col + ":\n\n"
    message += "  " + firstLine + "\n"
    message += "  " + Array(token.col).join(" ") + "^"
    return message
  }

  Lexer.prototype.clone = function() {
    return new Lexer(this.states, this.state)
  }

  Lexer.prototype.has = function(tokenType) {
    return true
  }


  return {
    compile: compile,
    states: compileStates,
    error: Object.freeze({error: true}),
    fallback: Object.freeze({fallback: true}),
    keywords: keywordTransform,
  }

}));


/***/ }),

/***/ "./node_modules/nearley/lib/nearley.js":
/*!*********************************************!*\
  !*** ./node_modules/nearley/lib/nearley.js ***!
  \*********************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

(function(root, factory) {
    if ( true && module.exports) {
        module.exports = factory();
    } else {
        root.nearley = factory();
    }
}(this, function() {

    function Rule(name, symbols, postprocess) {
        this.id = ++Rule.highestId;
        this.name = name;
        this.symbols = symbols;        // a list of literal | regex class | nonterminal
        this.postprocess = postprocess;
        return this;
    }
    Rule.highestId = 0;

    Rule.prototype.toString = function(withCursorAt) {
        function stringifySymbolSequence (e) {
            return e.literal ? JSON.stringify(e.literal) :
                   e.type ? '%' + e.type : e.toString();
        }
        var symbolSequence = (typeof withCursorAt === "undefined")
                             ? this.symbols.map(stringifySymbolSequence).join(' ')
                             : (   this.symbols.slice(0, withCursorAt).map(stringifySymbolSequence).join(' ')
                                 + " ● "
                                 + this.symbols.slice(withCursorAt).map(stringifySymbolSequence).join(' ')     );
        return this.name + " → " + symbolSequence;
    }


    // a State is a rule at a position from a given starting point in the input stream (reference)
    function State(rule, dot, reference, wantedBy) {
        this.rule = rule;
        this.dot = dot;
        this.reference = reference;
        this.data = [];
        this.wantedBy = wantedBy;
        this.isComplete = this.dot === rule.symbols.length;
    }

    State.prototype.toString = function() {
        return "{" + this.rule.toString(this.dot) + "}, from: " + (this.reference || 0);
    };

    State.prototype.nextState = function(child) {
        var state = new State(this.rule, this.dot + 1, this.reference, this.wantedBy);
        state.left = this;
        state.right = child;
        if (state.isComplete) {
            state.data = state.build();
        }
        return state;
    };

    State.prototype.build = function() {
        var children = [];
        var node = this;
        do {
            children.push(node.right.data);
            node = node.left;
        } while (node.left);
        children.reverse();
        return children;
    };

    State.prototype.finish = function() {
        if (this.rule.postprocess) {
            this.data = this.rule.postprocess(this.data, this.reference, Parser.fail);
        }
    };


    function Column(grammar, index) {
        this.grammar = grammar;
        this.index = index;
        this.states = [];
        this.wants = {}; // states indexed by the non-terminal they expect
        this.scannable = []; // list of states that expect a token
        this.completed = {}; // states that are nullable
    }


    Column.prototype.process = function(nextColumn) {
        var states = this.states;
        var wants = this.wants;
        var completed = this.completed;

        for (var w = 0; w < states.length; w++) { // nb. we push() during iteration
            var state = states[w];

            if (state.isComplete) {
                state.finish();
                if (state.data !== Parser.fail) {
                    // complete
                    var wantedBy = state.wantedBy;
                    for (var i = wantedBy.length; i--; ) { // this line is hot
                        var left = wantedBy[i];
                        this.complete(left, state);
                    }

                    // special-case nullables
                    if (state.reference === this.index) {
                        // make sure future predictors of this rule get completed.
                        var exp = state.rule.name;
                        (this.completed[exp] = this.completed[exp] || []).push(state);
                    }
                }

            } else {
                // queue scannable states
                var exp = state.rule.symbols[state.dot];
                if (typeof exp !== 'string') {
                    this.scannable.push(state);
                    continue;
                }

                // predict
                if (wants[exp]) {
                    wants[exp].push(state);

                    if (completed.hasOwnProperty(exp)) {
                        var nulls = completed[exp];
                        for (var i = 0; i < nulls.length; i++) {
                            var right = nulls[i];
                            this.complete(state, right);
                        }
                    }
                } else {
                    wants[exp] = [state];
                    this.predict(exp);
                }
            }
        }
    }

    Column.prototype.predict = function(exp) {
        var rules = this.grammar.byName[exp] || [];

        for (var i = 0; i < rules.length; i++) {
            var r = rules[i];
            var wantedBy = this.wants[exp];
            var s = new State(r, 0, this.index, wantedBy);
            this.states.push(s);
        }
    }

    Column.prototype.complete = function(left, right) {
        var copy = left.nextState(right);
        this.states.push(copy);
    }


    function Grammar(rules, start) {
        this.rules = rules;
        this.start = start || this.rules[0].name;
        var byName = this.byName = {};
        this.rules.forEach(function(rule) {
            if (!byName.hasOwnProperty(rule.name)) {
                byName[rule.name] = [];
            }
            byName[rule.name].push(rule);
        });
    }

    // So we can allow passing (rules, start) directly to Parser for backwards compatibility
    Grammar.fromCompiled = function(rules, start) {
        var lexer = rules.Lexer;
        if (rules.ParserStart) {
          start = rules.ParserStart;
          rules = rules.ParserRules;
        }
        var rules = rules.map(function (r) { return (new Rule(r.name, r.symbols, r.postprocess)); });
        var g = new Grammar(rules, start);
        g.lexer = lexer; // nb. storing lexer on Grammar is iffy, but unavoidable
        return g;
    }


    function StreamLexer() {
      this.reset("");
    }

    StreamLexer.prototype.reset = function(data, state) {
        this.buffer = data;
        this.index = 0;
        this.line = state ? state.line : 1;
        this.lastLineBreak = state ? -state.col : 0;
    }

    StreamLexer.prototype.next = function() {
        if (this.index < this.buffer.length) {
            var ch = this.buffer[this.index++];
            if (ch === '\n') {
              this.line += 1;
              this.lastLineBreak = this.index;
            }
            return {value: ch};
        }
    }

    StreamLexer.prototype.save = function() {
      return {
        line: this.line,
        col: this.index - this.lastLineBreak,
      }
    }

    StreamLexer.prototype.formatError = function(token, message) {
        // nb. this gets called after consuming the offending token,
        // so the culprit is index-1
        var buffer = this.buffer;
        if (typeof buffer === 'string') {
            var nextLineBreak = buffer.indexOf('\n', this.index);
            if (nextLineBreak === -1) nextLineBreak = buffer.length;
            var line = buffer.substring(this.lastLineBreak, nextLineBreak)
            var col = this.index - this.lastLineBreak;
            message += " at line " + this.line + " col " + col + ":\n\n";
            message += "  " + line + "\n"
            message += "  " + Array(col).join(" ") + "^"
            return message;
        } else {
            return message + " at index " + (this.index - 1);
        }
    }


    function Parser(rules, start, options) {
        if (rules instanceof Grammar) {
            var grammar = rules;
            var options = start;
        } else {
            var grammar = Grammar.fromCompiled(rules, start);
        }
        this.grammar = grammar;

        // Read options
        this.options = {
            keepHistory: false,
            lexer: grammar.lexer || new StreamLexer,
        };
        for (var key in (options || {})) {
            this.options[key] = options[key];
        }

        // Setup lexer
        this.lexer = this.options.lexer;
        this.lexerState = undefined;

        // Setup a table
        var column = new Column(grammar, 0);
        var table = this.table = [column];

        // I could be expecting anything.
        column.wants[grammar.start] = [];
        column.predict(grammar.start);
        // TODO what if start rule is nullable?
        column.process();
        this.current = 0; // token index
    }

    // create a reserved token for indicating a parse fail
    Parser.fail = {};

    Parser.prototype.feed = function(chunk) {
        var lexer = this.lexer;
        lexer.reset(chunk, this.lexerState);

        var token;
        while (token = lexer.next()) {
            // We add new states to table[current+1]
            var column = this.table[this.current];

            // GC unused states
            if (!this.options.keepHistory) {
                delete this.table[this.current - 1];
            }

            var n = this.current + 1;
            var nextColumn = new Column(this.grammar, n);
            this.table.push(nextColumn);

            // Advance all tokens that expect the symbol
            var literal = token.text !== undefined ? token.text : token.value;
            var value = lexer.constructor === StreamLexer ? token.value : token;
            var scannable = column.scannable;
            for (var w = scannable.length; w--; ) {
                var state = scannable[w];
                var expect = state.rule.symbols[state.dot];
                // Try to consume the token
                // either regex or literal
                if (expect.test ? expect.test(value) :
                    expect.type ? expect.type === token.type
                                : expect.literal === literal) {
                    // Add it
                    var next = state.nextState({data: value, token: token, isToken: true, reference: n - 1});
                    nextColumn.states.push(next);
                }
            }

            // Next, for each of the rules, we either
            // (a) complete it, and try to see if the reference row expected that
            //     rule
            // (b) predict the next nonterminal it expects by adding that
            //     nonterminal's start state
            // To prevent duplication, we also keep track of rules we have already
            // added

            nextColumn.process();

            // If needed, throw an error:
            if (nextColumn.states.length === 0) {
                // No states at all! This is not good.
                var message = this.lexer.formatError(token, "invalid syntax") + "\n";
                message += "Unexpected " + (token.type ? token.type + " token: " : "");
                message += JSON.stringify(token.value !== undefined ? token.value : token) + "\n";
                var err = new Error(message);
                err.offset = this.current;
                err.token = token;
                throw err;
            }

            // maybe save lexer state
            if (this.options.keepHistory) {
              column.lexerState = lexer.save()
            }

            this.current++;
        }
        if (column) {
          this.lexerState = lexer.save()
        }

        // Incrementally keep track of results
        this.results = this.finish();

        // Allow chaining, for whatever it's worth
        return this;
    };

    Parser.prototype.save = function() {
        var column = this.table[this.current];
        column.lexerState = this.lexerState;
        return column;
    };

    Parser.prototype.restore = function(column) {
        var index = column.index;
        this.current = index;
        this.table[index] = column;
        this.table.splice(index + 1);
        this.lexerState = column.lexerState;

        // Incrementally keep track of results
        this.results = this.finish();
    };

    // nb. deprecated: use save/restore instead!
    Parser.prototype.rewind = function(index) {
        if (!this.options.keepHistory) {
            throw new Error('set option `keepHistory` to enable rewinding')
        }
        // nb. recall column (table) indicies fall between token indicies.
        //        col 0   --   token 0   --   col 1
        this.restore(this.table[index]);
    };

    Parser.prototype.finish = function() {
        // Return the possible parsings
        var considerations = [];
        var start = this.grammar.start;
        var column = this.table[this.table.length - 1]
        column.states.forEach(function (t) {
            if (t.rule.name === start
                    && t.dot === t.rule.symbols.length
                    && t.reference === 0
                    && t.data !== Parser.fail) {
                considerations.push(t);
            }
        });
        return considerations.map(function(c) {return c.data; });
    };

    return {
        Parser: Parser,
        Grammar: Grammar,
        Rule: Rule,
    };

}));


/***/ }),

/***/ "./src/Annotated.ts":
/*!**************************!*\
  !*** ./src/Annotated.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Lattice_1 = __webpack_require__(/*! ./util/Lattice */ "./src/util/Lattice.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
// For trait idiom see https://www.bryntum.com/blog/the-mixin-pattern-in-typescript-all-you-need-to-know/ and
// https://github.com/Microsoft/TypeScript/issues/21710.
function AnnotatedC(C) {
    // https://stackoverflow.com/questions/33605775
    return {
        [C.name]: class extends C {
            constructor() {
                super(...arguments);
                this.__α = Value_1._;
            }
        }
    }[C.name]; // give versioned class same name as C
}
exports.AnnotatedC = AnnotatedC;
function annotated(v) {
    return v.hasOwnProperty("__α");
}
exports.annotated = annotated;
function asAnnotated(v) {
    if (annotated(v)) {
        return v;
    }
    else {
        return Core_1.absurd(`Not an annotated value: ${Core_1.className(v)}`);
    }
}
exports.asAnnotated = asAnnotated;
function setα(α, v) {
    v.__α = α;
    return v;
}
exports.setα = setα;
function setallα(α, v) {
    if (annotated(v)) {
        setα(α, v);
    }
    v.fieldValues().forEach((v) => {
        if (v instanceof Value_1.Value) {
            setallα(α, v);
        }
    });
    return v;
}
exports.setallα = setallα;
function negateallα(v) {
    if (annotated(v)) {
        setα(Lattice_1.ann.negate(v.__α), v);
    }
    v.fieldValues().forEach((v) => {
        if (v instanceof Value_1.Value) {
            negateallα(v);
        }
    });
    return v;
}
exports.negateallα = negateallα;
function joinα(α, v) {
    v.__α = Lattice_1.ann.join(α, v.__α);
    return v;
}
exports.joinα = joinα;
function meetα(α, v) {
    v.__α = Lattice_1.ann.meet(α, v.__α);
    return v;
}
exports.meetα = meetα;
// Make an annotated node, for a class that doesn't already specify statically that its instances are annotated.
function annotatedAt(k, C, ...v̅) {
    const v = Versioned_1.at(k, C, ...v̅);
    v.__α = Value_1._;
    return v;
}
exports.annotatedAt = annotatedAt;
function num(val) {
    return annotatedAt(Versioned_1.ν(), Value_1.Num, val);
}
exports.num = num;
function str(val) {
    return annotatedAt(Versioned_1.ν(), Value_1.Str, val);
}
exports.str = str;


/***/ }),

/***/ "./src/BaseTypes.ts":
/*!**************************!*\
  !*** ./src/BaseTypes.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
// See Env for convention regarding instance members on reflected datatypes.
class Bool extends DataValue_1.DataValue {
}
exports.Bool = Bool;
class True extends Bool {
}
exports.True = True;
function true_() {
    return Annotated_1.annotatedAt(Versioned_1.ν(), True);
}
exports.true_ = true_;
class False extends Bool {
}
exports.False = False;
function false_() {
    return Annotated_1.annotatedAt(Versioned_1.ν(), False);
}
exports.false_ = false_;
class List extends DataValue_1.DataValue {
    static fromArray(x̅) {
        let x̅ʹ = nil();
        for (let n = x̅.length - 1; n >= 0; --n) {
            x̅ʹ = cons(x̅[n], x̅ʹ);
        }
        return x̅ʹ;
    }
    toArray() {
        const x̅ = [];
        this.toArray_(x̅);
        return x̅;
    }
    toArray_(x̅) {
        if (Cons.is(this)) {
            x̅.push(this.head);
            this.tail.toArray_(x̅);
        }
        else if (Nil.is(this)) {
        }
        else {
            return Core_1.absurd();
        }
    }
}
exports.List = List;
class Nil extends List {
    static is(xs) {
        return xs instanceof Nil;
    }
}
exports.Nil = Nil;
function nil() {
    return Value_1.make(Nil);
}
exports.nil = nil;
class Cons extends List {
    constructor() {
        super(...arguments);
        this.head = Value_1._;
        this.tail = Value_1._;
    }
    static is(xs) {
        return xs instanceof Cons;
    }
}
exports.Cons = Cons;
function cons(head, tail) {
    return Value_1.make(Cons, head, tail);
}
exports.cons = cons;
class Pair extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.fst = Value_1._;
        this.snd = Value_1._;
    }
}
exports.Pair = Pair;
function pair(fst, snd) {
    return Value_1.make(Pair, fst, snd);
}
exports.pair = pair;
class Tree extends DataValue_1.DataValue {
    toArray() {
        const x̅ = [];
        this.toArray_(x̅);
        return x̅;
    }
    toArray_(x̅) {
        if (NonEmpty.is(this)) {
            this.left.toArray_(x̅);
            x̅.push(this.t);
            this.right.toArray_(x̅);
        }
        else if (Empty.is(this)) {
        }
        else {
            return Core_1.absurd();
        }
    }
}
exports.Tree = Tree;
class Empty extends Tree {
    static is(t) {
        return t instanceof Empty;
    }
}
exports.Empty = Empty;
function empty() {
    return Value_1.make(Empty);
}
exports.empty = empty;
class NonEmpty extends Tree {
    constructor() {
        super(...arguments);
        this.left = Value_1._;
        this.t = Value_1._;
        this.right = Value_1._;
    }
    static is(t) {
        return t instanceof NonEmpty;
    }
}
exports.NonEmpty = NonEmpty;
function nonEmpty(left, t, right) {
    return Value_1.make(NonEmpty, left, t, right);
}
exports.nonEmpty = nonEmpty;
class Option extends DataValue_1.DataValue {
}
exports.Option = Option;
class None extends Option {
}
exports.None = None;
class Some extends Option {
    constructor() {
        super(...arguments);
        this.t = Value_1._;
    }
}
exports.Some = Some;
class Ordering extends DataValue_1.DataValue {
}
exports.Ordering = Ordering;
class LT extends Ordering {
}
exports.LT = LT;
class GT extends Ordering {
}
exports.GT = GT;
class EQ extends Ordering {
}
exports.EQ = EQ;
DataType_1.initDataType(Bool, [True, False]);
DataType_1.initDataType(List, [Nil, Cons]);
DataType_1.initDataType(Option, [Some, None]);
DataType_1.initDataType(Ordering, [LT, GT, EQ]);
DataType_1.initDataType(Pair, [Pair]);
DataType_1.initDataType(Tree, [Empty, NonEmpty]);


/***/ }),

/***/ "./src/DataType.ts":
/*!*************************!*\
  !*** ./src/DataType.ts ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Match_1 = __webpack_require__(/*! ./Match */ "./src/Match.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
class PrimType {
    constructor(name, C) {
        this.name = name;
        this.C = C;
    }
}
exports.PrimType = PrimType;
// Neither of these is currently reflective because of non-standard fields.
class DataType {
    constructor(name, elimC, ctrs, explC̅) {
        this.name = name;
        this.elimC = elimC;
        this.ctrs = ctrs;
        this.explC̅ = explC̅;
    }
}
exports.DataType = DataType;
// Constructor of a datatype, not to be confused with an instance of such a thing (Constr) or name of such a thing
// (Lex.Ctr). Fields have a total ordering given by the order of definition in the corresponding class.
class Ctr {
    constructor(C, f̅) {
        this.C = C;
        this.f̅ = f̅;
    }
}
exports.Ctr = Ctr;
function ctrFor(ctr) {
    return exports.ctrToDataType.get(ctr.val).ctrs.get(ctr.val);
}
exports.ctrFor = ctrFor;
function arity(ctr) {
    Core_1.assert(exports.ctrToDataType.has(ctr.val), `No such constructor: "${ctr.val}".`);
    return ctrFor(ctr).f̅.length;
}
exports.arity = arity;
// Populated by initDataTypes(). Constructors are not yet first-class.
exports.types = new Map;
exports.ctrToDataType = new Map;
exports.elimToDataType = new Map;
exports.elimSuffix = "Elim";
exports.explSuffix = "Expl";
// See https://stackoverflow.com/questions/33605775 for the dynamic class-naming idiom.
function initDataType(D, C̅) {
    C̅.sort((C, Cʹ) => C.name.localeCompare(Cʹ.name)); // consistent with Str.leq
    const ctrs = C̅.map((C) => [C.name, new Ctr(C, Value_1.fields(new C))]), elimC_name = D.name + exports.elimSuffix, elimC = {
        [elimC_name]: class extends Match_1.DataElim {
            constructor() {
                super();
                // lexicographical order hopefully preserved by getOwnPropertyNames()
                C̅.forEach((C) => {
                    this[C.name] = Value_1._;
                });
            }
        }
    }[elimC_name], explC_name = D.name + exports.explSuffix, explC̅ = ctrs.map(([cʹ, c]) => {
        return [cʹ, {
                [explC_name]: class extends DataValue_1.DataExpl {
                    constructor() {
                        super();
                        c.f̅.forEach((f) => {
                            this[f] = Value_1._;
                        });
                    }
                }
            }[explC_name]];
    }), d = new DataType(Annotated_1.str(D.name), elimC, new Map(ctrs), new Map(explC̅));
    C̅.forEach((C) => {
        exports.ctrToDataType.set(C.name, d);
    });
    exports.elimToDataType.set(elimC_name, d);
    exports.types.set(d.name.val, d);
}
exports.initDataType = initDataType;
exports.types.set(Value_1.Num.name, new PrimType(Annotated_1.str(Value_1.Num.name), Value_1.Num));
exports.types.set(Value_1.Str.name, new PrimType(Annotated_1.str(Value_1.Str.name), Value_1.Str));


/***/ }),

/***/ "./src/DataValue.ts":
/*!**************************!*\
  !*** ./src/DataValue.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(/*! ./util/Array */ "./src/util/Array.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
// Value of a datatype constructor; fields are always user-level values (i.e. not ES6 primitives).
class DataValue extends Value_1.Value {
    fieldValues() {
        return Value_1.fields(this).map(k => this[k]);
    }
    fieldExplValues() {
        return Array_1.zip(this.__expl.fieldValues(), this.fieldValues());
    }
}
exports.DataValue = DataValue;
class DataExpl extends DataValue {
    fieldValues() {
        return Value_1.fields(this).map(k => this[k]);
    }
}
exports.DataExpl = DataExpl;


/***/ }),

/***/ "./src/Env.ts":
/*!********************!*\
  !*** ./src/Env.ts ***!
  \********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
// Idiom is to permit instance methods on reflected datatypes, but not have them use polymorphism.
// Environments are snoc lists.
class Env extends DataValue_1.DataValue {
    get(k) {
        if (this instanceof EmptyEnv) {
            return undefined;
        }
        else if (this instanceof ExtendEnv) {
            if (this.k.val === k.val) {
                return this.v;
            }
            else {
                return this.ρ.get(k);
            }
        }
        else {
            return Core_1.absurd();
        }
    }
    has(k) {
        return this.get(k) !== undefined;
    }
    static singleton(k, v) {
        return extendEnv(emptyEnv(), k, v);
    }
    concat(ρ) {
        if (ρ instanceof EmptyEnv) {
            return this;
        }
        else if (ρ instanceof ExtendEnv) {
            return extendEnv(this.concat(ρ.ρ), ρ.k, ρ.v);
        }
        else {
            return Core_1.absurd();
        }
    }
}
exports.Env = Env;
class EmptyEnv extends Env {
}
exports.EmptyEnv = EmptyEnv;
function emptyEnv() {
    return Value_1.make(EmptyEnv);
}
exports.emptyEnv = emptyEnv;
class ExtendEnv extends Env {
    constructor() {
        super(...arguments);
        this.ρ = Value_1._;
        this.k = Value_1._;
        this.v = Value_1._;
    }
}
exports.ExtendEnv = ExtendEnv;
function extendEnv(ρ, k, v) {
    return Value_1.make(ExtendEnv, ρ, k, v);
}
exports.extendEnv = extendEnv;


/***/ }),

/***/ "./src/Eval.ts":
/*!*********************!*\
  !*** ./src/Eval.ts ***!
  \*********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Array_1 = __webpack_require__(/*! ./util/Array */ "./src/util/Array.ts");
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Lattice_1 = __webpack_require__(/*! ./util/Lattice */ "./src/util/Lattice.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const BaseTypes_1 = __webpack_require__(/*! ./BaseTypes */ "./src/BaseTypes.ts");
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Env_1 = __webpack_require__(/*! ./Env */ "./src/Env.ts");
const ExplValue_1 = __webpack_require__(/*! ./ExplValue */ "./src/ExplValue.ts");
const Expr_1 = __webpack_require__(/*! ./Expr */ "./src/Expr.ts");
const FiniteMap_1 = __webpack_require__(/*! ./FiniteMap */ "./src/FiniteMap.ts");
const Match_1 = __webpack_require__(/*! ./Match */ "./src/Match.ts");
const Primitive_1 = __webpack_require__(/*! ./Primitive */ "./src/Primitive.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
var Direction;
(function (Direction) {
    Direction[Direction["Fwd"] = 0] = "Fwd";
    Direction[Direction["Bwd"] = 1] = "Bwd";
})(Direction = exports.Direction || (exports.Direction = {}));
var Eval;
(function (Eval) {
    // ρ plus bindings in δ are closing for f.
    class Closure extends Annotated_1.AnnotatedC(DataValue_1.DataValue) {
        constructor() {
            super(...arguments);
            this.ρ = Value_1._;
            this.δ = Value_1._;
            this.f = Value_1._;
        }
    }
    Eval.Closure = Closure;
    function closure(k, ρ, δ, f) {
        return Versioned_1.at(k, Closure, ρ, δ, f);
    }
    // Environments are snoc-lists, so this (inconsequentially) reverses declaration order.
    function recDefs(δ_0, ρ, δ) {
        if (BaseTypes_1.Cons.is(δ)) {
            const def = δ.head, [δₜ, ρ_ext] = recDefs(δ_0, ρ, δ.tail), f = closure(Versioned_1.ν(), ρ, δ_0, Match_1.evalTrie(def.σ));
            return [BaseTypes_1.cons(ExplValue_1.Expl.recDef(def.x, f), δₜ), Env_1.extendEnv(ρ_ext, def.x, f)];
        }
        else if (BaseTypes_1.Nil.is(δ)) {
            return [BaseTypes_1.nil(), Env_1.emptyEnv()];
        }
        else {
            return Core_1.absurd();
        }
    }
    function recDefs_(dir, δ) {
        if (BaseTypes_1.Cons.is(δ)) {
            Array_1.zip(δ.head.f.δ.toArray(), δ.toArray()).map(([def, defₜ]) => {
                Core_1.assert(def.x.eq(defₜ.x));
                if (dir === Direction.Fwd) {
                    Annotated_1.setα(def.x.__α, defₜ.f);
                }
                else {
                    Annotated_1.joinα(defₜ.f.__α, def.x);
                }
            });
        }
        else if (BaseTypes_1.Nil.is(δ)) {
        }
        else {
            return Core_1.absurd();
        }
    }
    // Here we mustn't invert definition order.
    function defs(ρ, def̅, ρ_ext) {
        if (BaseTypes_1.Cons.is(def̅)) {
            const def = def̅.head;
            if (def instanceof Expr_1.Expr.Let) {
                const tv = eval_(ρ.concat(ρ_ext), def.e), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, Env_1.extendEnv(ρ_ext, def.x, tv.v));
                return [BaseTypes_1.cons(ExplValue_1.Expl.let_(def.x, tv), def̅ₜ), ρ_extʹ];
            }
            else if (def instanceof Expr_1.Expr.Prim) {
                // first-class primitives currently happen to be unary
                if (Primitive_1.unaryOps.has(def.x.val)) {
                    const op = Primitive_1.unaryOps.get(def.x.val), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, Env_1.extendEnv(ρ_ext, def.x, op));
                    return [BaseTypes_1.cons(ExplValue_1.Expl.prim(def.x, op), def̅ₜ), ρ_extʹ];
                }
                else {
                    return Core_1.error(`No implementation found for primitive "${def.x.val}".`);
                }
            }
            else if (def instanceof Expr_1.Expr.LetRec) {
                const [δ, ρᵟ] = recDefs(def.δ, ρ.concat(ρ_ext), def.δ), [def̅ₜ, ρ_extʹ] = defs(ρ, def̅.tail, ρ_ext.concat(ρᵟ));
                return [BaseTypes_1.cons(ExplValue_1.Expl.letRec(δ), def̅ₜ), ρ_extʹ];
            }
            else {
                return Core_1.absurd();
            }
        }
        else if (BaseTypes_1.Nil.is(def̅)) {
            return [BaseTypes_1.nil(), ρ_ext];
        }
        else {
            return Core_1.absurd();
        }
    }
    function defs_fwd(def̅, def̅ₜ) {
        Array_1.zip(def̅.toArray(), def̅ₜ.toArray()).forEach(([def, defₜ]) => {
            if (defₜ instanceof ExplValue_1.Expl.Let) {
                eval_fwd(Core_1.as(def, Expr_1.Expr.Let).e, defₜ.tv);
                Annotated_1.meetα(defₜ.x.__α, defₜ.tv.v);
            }
            else if (defₜ instanceof ExplValue_1.Expl.Prim) {
                Annotated_1.setα(defₜ.x.__α, defₜ.op);
            }
            else if (defₜ instanceof ExplValue_1.Expl.LetRec) {
                recDefs_(Direction.Fwd, defₜ.δ);
            }
            else {
                Core_1.absurd();
            }
        });
    }
    function defs_bwd(def̅, def̅ₜ) {
        Array_1.zip(def̅.toArray(), def̅ₜ.toArray()).reverse().forEach(([def, defₜ]) => {
            if (defₜ instanceof ExplValue_1.Expl.Let) {
                Annotated_1.joinα(defₜ.tv.v.__α, defₜ.x);
                eval_bwd(Core_1.as(def, Expr_1.Expr.Let).e, defₜ.tv);
            }
            else if (defₜ instanceof ExplValue_1.Expl.Prim) {
                Annotated_1.joinα(defₜ.op.__α, defₜ.x);
            }
            else if (defₜ instanceof ExplValue_1.Expl.LetRec) {
                recDefs_(Direction.Bwd, defₜ.δ);
            }
            else {
                Core_1.absurd();
            }
        });
    }
    function eval_(ρ, e) {
        if (e instanceof Expr_1.Expr.ConstNum) {
            return ExplValue_1.explValue(ExplValue_1.Expl.empty(), Annotated_1.num(e.val.val));
        }
        else if (e instanceof Expr_1.Expr.ConstStr) {
            return ExplValue_1.explValue(ExplValue_1.Expl.empty(), Annotated_1.str(e.val.val));
        }
        else if (e instanceof Expr_1.Expr.Fun) {
            return ExplValue_1.explValue(ExplValue_1.Expl.empty(), closure(Versioned_1.ν(), ρ, BaseTypes_1.nil(), Match_1.evalTrie(e.σ)));
        }
        else if (e instanceof Expr_1.Expr.Constr) {
            let tv̅ = e.args.toArray().map((e) => eval_(ρ, e)), c = e.ctr.val, d = Core_1.__nonNull(DataType_1.ctrToDataType.get(c)), v = Annotated_1.annotatedAt(Versioned_1.ν(), d.ctrs.get(c).C, ...tv̅.map(({ v }) => v));
            v.__expl = Value_1.make(d.explC̅.get(c), ...tv̅.map(({ t }) => t));
            return ExplValue_1.explValue(ExplValue_1.Expl.empty(), v);
        }
        else if (e instanceof Expr_1.Expr.Quote) {
            return ExplValue_1.explValue(ExplValue_1.Expl.quote(), Versioned_1.copyAt(Versioned_1.ν(), e.e));
        }
        else if (e instanceof Expr_1.Expr.Var) {
            if (ρ.has(e.x)) {
                const v = ρ.get(e.x);
                return ExplValue_1.explValue(ExplValue_1.Expl.var_(e.x, v), Versioned_1.copyAt(Versioned_1.ν(), v));
            }
            else {
                return Core_1.error(`Variable "${e.x.val}" not found.`);
            }
        }
        else if (e instanceof Expr_1.Expr.App) {
            const [tf, tu] = [eval_(ρ, e.f), eval_(ρ, e.e)], [v, u] = [tf.v, tu.v];
            if (v instanceof Closure) {
                const [δ, ρᵟ] = recDefs(v.δ, v.ρ, v.δ), [ρʹ, ξκ] = v.f.apply(u), tv = eval_(v.ρ.concat(ρᵟ.concat(ρʹ)), ξκ.κ);
                return ExplValue_1.explValue(ExplValue_1.Expl.app(tf, tu, δ, ξκ, tv), Versioned_1.copyAt(Versioned_1.ν(), tv.v));
            }
            else if (v instanceof Primitive_1.UnaryOp) {
                if (u instanceof Value_1.Num || u instanceof Value_1.Str) {
                    return ExplValue_1.explValue(ExplValue_1.Expl.unaryApp(tf, tu), v.op(u));
                }
                else {
                    return Core_1.error(`Applying "${v.name}" to non-primitive value.`, u);
                }
            }
            else {
                return Core_1.error(`Cannot apply ${Core_1.className(v)}`);
            }
        }
        else 
        // Binary operators are (currently) "syntax", rather than first-class.
        if (e instanceof Expr_1.Expr.BinaryApp) {
            if (Primitive_1.binaryOps.has(e.opName.val)) {
                const op = Primitive_1.binaryOps.get(e.opName.val), [tv1, tv2] = [eval_(ρ, e.e1), eval_(ρ, e.e2)], [v1, v2] = [tv1.v, tv2.v];
                if ((v1 instanceof Value_1.Num || v1 instanceof Value_1.Str) && (v2 instanceof Value_1.Num || v2 instanceof Value_1.Str)) {
                    return ExplValue_1.explValue(ExplValue_1.Expl.binaryApp(tv1, e.opName, tv2), op.op(v1, v2));
                }
                else {
                    return Core_1.error(`Applying "${e.opName}" to non-primitive value.`, v1, v2);
                }
            }
            else {
                return Core_1.error(`Binary primitive "${e.opName.val}" not found.`);
            }
        }
        else if (e instanceof Expr_1.Expr.Defs) {
            const [def̅ₜ, ρʹ] = defs(ρ, e.def̅, Env_1.emptyEnv()), tv = eval_(ρ.concat(ρʹ), e.e);
            return ExplValue_1.explValue(ExplValue_1.Expl.defs(def̅ₜ, tv), Versioned_1.copyAt(Versioned_1.ν(), tv.v));
        }
        else if (e instanceof Expr_1.Expr.MatchAs) {
            const tu = eval_(ρ, e.e), [ρʹ, ξκ] = Match_1.evalTrie(e.σ).apply(tu.v), tv = eval_(ρ.concat(ρʹ), ξκ.κ);
            return ExplValue_1.explValue(ExplValue_1.Expl.matchAs(tu, ξκ, tv), Versioned_1.copyAt(Versioned_1.ν(), tv.v));
        }
        else if (e instanceof Expr_1.Expr.Typematch) {
            const tu = eval_(ρ, e.e), d = DataType_1.ctrToDataType.get(Core_1.className(tu.v)) || DataType_1.types.get(Core_1.className(tu.v)), eʹ = FiniteMap_1.get(e.cases, d.name);
            if (eʹ === undefined) {
                return Core_1.error(`Typecase mismatch: no clause for ${Core_1.className(tu.v)}.`);
            }
            else {
                const tv = eval_(ρ, eʹ);
                return ExplValue_1.explValue(ExplValue_1.Expl.typematch(tu, d.name, tv), Versioned_1.copyAt(Versioned_1.ν(), tv.v));
            }
        }
        else {
            return Core_1.absurd(`Unimplemented expression form: ${Core_1.className(e)}.`);
        }
    }
    Eval.eval_ = eval_;
    function eval_fwd(e, { t, v }) {
        if (t instanceof ExplValue_1.Expl.Empty) {
            if (v instanceof Value_1.Num || v instanceof Value_1.Str || v instanceof Closure) {
                Annotated_1.setα(e.__α, v);
            }
            else if (v instanceof DataValue_1.DataValue) {
                const eʹ = Core_1.as(e, Expr_1.Expr.Constr);
                Array_1.zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_fwd(e, ExplValue_1.explValue(t, v)));
                Annotated_1.setα(e.__α, v);
            }
        }
        else if (t instanceof ExplValue_1.Expl.Quote) {
            Annotated_1.setα(e.__α, v);
        }
        else if (t instanceof ExplValue_1.Expl.Var) {
            Annotated_1.setα(Lattice_1.ann.meet(e.__α, t.v.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.App) {
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_fwd(eʹ.f, t.tf);
            eval_fwd(eʹ.e, t.tu);
            recDefs_(Direction.Fwd, t.δ);
            eval_fwd(t.ξ.κ, t.tv);
            Annotated_1.setα(Lattice_1.ann.meet(t.tf.v.__α, Match_1.match_fwd(t.ξ), e.__α, t.tv.v.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.UnaryApp) {
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_fwd(eʹ.f, t.tf);
            eval_fwd(eʹ.e, t.tv);
            Annotated_1.setα(Lattice_1.ann.meet(t.tf.v.__α, t.tv.v.__α, e.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.BinaryApp) {
            const eʹ = Core_1.as(e, Expr_1.Expr.BinaryApp);
            eval_fwd(eʹ.e1, t.tv1);
            eval_fwd(eʹ.e2, t.tv2);
            Annotated_1.setα(Lattice_1.ann.meet(t.tv1.v.__α, t.tv2.v.__α, e.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.Defs) {
            const eʹ = Core_1.as(e, Expr_1.Expr.Defs);
            defs_fwd(eʹ.def̅, t.def̅);
            eval_fwd(eʹ.e, t.tv);
            Annotated_1.setα(Lattice_1.ann.meet(e.__α, t.tv.v.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.MatchAs) {
            const eʹ = Core_1.as(e, Expr_1.Expr.MatchAs);
            eval_fwd(eʹ.e, t.tu);
            eval_fwd(t.ξ.κ, t.tv);
            Annotated_1.setα(Lattice_1.ann.meet(Match_1.match_fwd(t.ξ), e.__α, t.tv.v.__α), v);
        }
        else if (t instanceof ExplValue_1.Expl.Typematch) {
            const eʹ = Core_1.as(e, Expr_1.Expr.Typematch);
            eval_fwd(eʹ.e, t.tu);
            eval_fwd(FiniteMap_1.get(eʹ.cases, t.d), t.tv);
            Annotated_1.setα(Lattice_1.ann.meet(e.__α, t.tv.v.__α), v);
        }
        else {
            Core_1.absurd();
        }
    }
    Eval.eval_fwd = eval_fwd;
    // Avoid excessive joins via a merging implementation; requires all annotations to have been cleared first.
    function eval_bwd(e, { t, v }) {
        if (t instanceof ExplValue_1.Expl.Empty) {
            if (v instanceof Value_1.Num || v instanceof Value_1.Str || v instanceof Closure) {
                Annotated_1.joinα(v.__α, e);
            }
            else if (v instanceof DataValue_1.DataValue) {
                const eʹ = Core_1.as(e, Expr_1.Expr.Constr);
                // reverse order but shouldn't matter in absence of side-effects:
                Array_1.zip(v.fieldExplValues(), eʹ.args.toArray()).map(([[t, v], e]) => eval_bwd(e, ExplValue_1.explValue(t, v)));
                Annotated_1.joinα(v.__α, e);
            }
            else {
                Core_1.absurd();
            }
        }
        else if (t instanceof ExplValue_1.Expl.Quote) {
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.Var) {
            Annotated_1.joinα(v.__α, t.v);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.App) {
            Core_1.assert(t.tf.v instanceof Closure);
            Annotated_1.joinα(v.__α, t.tv.v);
            eval_bwd(t.ξ.κ, t.tv);
            Match_1.match_bwd(t.ξ, v.__α);
            recDefs_(Direction.Bwd, t.δ);
            Annotated_1.joinα(v.__α, t.tf.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_bwd(eʹ.f, t.tf);
            eval_bwd(eʹ.e, t.tu);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.UnaryApp) {
            Annotated_1.joinα(v.__α, t.tf.v);
            Annotated_1.joinα(v.__α, t.tv.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.App);
            eval_bwd(eʹ.f, t.tf);
            eval_bwd(eʹ.e, t.tv);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.BinaryApp) {
            Core_1.assert(Primitive_1.binaryOps.has(t.opName.val));
            Annotated_1.joinα(v.__α, t.tv1.v);
            Annotated_1.joinα(v.__α, t.tv2.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.BinaryApp);
            eval_bwd(eʹ.e1, t.tv1);
            eval_bwd(eʹ.e2, t.tv2);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.Defs) {
            Annotated_1.joinα(v.__α, t.tv.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.Defs);
            eval_bwd(eʹ.e, t.tv);
            defs_bwd(eʹ.def̅, t.def̅);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.MatchAs) {
            Annotated_1.joinα(v.__α, t.tv.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.MatchAs);
            eval_bwd(t.ξ.κ, t.tv);
            Match_1.match_bwd(t.ξ, v.__α);
            eval_bwd(eʹ.e, t.tu);
            Annotated_1.joinα(v.__α, e);
        }
        else if (t instanceof ExplValue_1.Expl.Typematch) {
            Annotated_1.joinα(v.__α, t.tv.v);
            const eʹ = Core_1.as(e, Expr_1.Expr.Typematch);
            eval_bwd(FiniteMap_1.get(eʹ.cases, t.d), t.tv);
            eval_bwd(eʹ.e, t.tu);
            Annotated_1.joinα(v.__α, e);
        }
        else {
            Core_1.absurd();
        }
    }
    Eval.eval_bwd = eval_bwd;
})(Eval = exports.Eval || (exports.Eval = {}));
DataType_1.initDataType(Expr_1.Expr.Expr, [Expr_1.Expr.App, Expr_1.Expr.BinaryApp, Expr_1.Expr.ConstNum, Expr_1.Expr.ConstStr, Expr_1.Expr.Constr, Expr_1.Expr.Defs, Expr_1.Expr.Fun, Expr_1.Expr.MatchAs, Expr_1.Expr.Quote, Expr_1.Expr.Var]);


/***/ }),

/***/ "./src/ExplValue.ts":
/*!**************************!*\
  !*** ./src/ExplValue.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
class ExplValue extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.t = Value_1._;
        this.v = Value_1._;
    }
}
exports.ExplValue = ExplValue;
function explValue(t, v) {
    return Value_1.make(ExplValue, t, v);
}
exports.explValue = explValue;
var Expl;
(function (Expl_1) {
    class Expl extends DataValue_1.DataValue {
    }
    Expl_1.Expl = Expl;
    class App extends Expl {
        constructor() {
            super(...arguments);
            this.tf = Value_1._;
            this.tu = Value_1._;
            this.δ = Value_1._; // additional recursive functions bound at this step
            this.ξ = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.App = App;
    function app(tf, tu, δ, ξ, tv) {
        return Versioned_1.at(Versioned_1.ν(), App, tf, tu, δ, ξ, tv);
    }
    Expl_1.app = app;
    class UnaryApp extends Expl {
        constructor() {
            super(...arguments);
            this.tf = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.UnaryApp = UnaryApp;
    function unaryApp(tf, tv) {
        return Versioned_1.at(Versioned_1.ν(), UnaryApp, tf, tv);
    }
    Expl_1.unaryApp = unaryApp;
    class BinaryApp extends Expl {
        constructor() {
            super(...arguments);
            this.tv1 = Value_1._;
            this.opName = Value_1._;
            this.tv2 = Value_1._;
        }
    }
    Expl_1.BinaryApp = BinaryApp;
    function binaryApp(tv1, opName, tv2) {
        return Versioned_1.at(Versioned_1.ν(), BinaryApp, tv1, opName, tv2);
    }
    Expl_1.binaryApp = binaryApp;
    class Def extends DataValue_1.DataValue {
    }
    Expl_1.Def = Def;
    class Let extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.Let = Let;
    function let_(x, tv) {
        return Versioned_1.at(Versioned_1.ν(), Let, x, tv);
    }
    Expl_1.let_ = let_;
    class Prim extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.op = Value_1._;
        }
    }
    Expl_1.Prim = Prim;
    function prim(x, op) {
        return Versioned_1.at(Versioned_1.ν(), Prim, x, op);
    }
    Expl_1.prim = prim;
    class RecDef extends DataValue_1.DataValue {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.f = Value_1._;
        }
    }
    Expl_1.RecDef = RecDef;
    function recDef(x, f) {
        return Versioned_1.at(Versioned_1.ν(), RecDef, x, f);
    }
    Expl_1.recDef = recDef;
    class LetRec extends Def {
        constructor() {
            super(...arguments);
            this.δ = Value_1._;
        }
    }
    Expl_1.LetRec = LetRec;
    function letRec(δ) {
        return Versioned_1.at(Versioned_1.ν(), LetRec, δ);
    }
    Expl_1.letRec = letRec;
    class Defs extends Expl {
        constructor() {
            super(...arguments);
            this.def̅ = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.Defs = Defs;
    function defs(def̅, tv) {
        return Versioned_1.at(Versioned_1.ν(), Defs, def̅, tv);
    }
    Expl_1.defs = defs;
    class Empty extends Expl {
    }
    Expl_1.Empty = Empty;
    function empty() {
        return Versioned_1.at(Versioned_1.ν(), Empty);
    }
    Expl_1.empty = empty;
    class MatchAs extends Expl {
        constructor() {
            super(...arguments);
            this.tu = Value_1._;
            this.ξ = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.MatchAs = MatchAs;
    function matchAs(tu, ξ, tv) {
        return Versioned_1.at(Versioned_1.ν(), MatchAs, tu, ξ, tv);
    }
    Expl_1.matchAs = matchAs;
    class Quote extends Expl {
    }
    Expl_1.Quote = Quote;
    function quote() {
        return Versioned_1.at(Versioned_1.ν(), Quote);
    }
    Expl_1.quote = quote;
    class Typematch extends Expl {
        constructor() {
            super(...arguments);
            this.tu = Value_1._;
            this.d = Value_1._;
            this.tv = Value_1._;
        }
    }
    Expl_1.Typematch = Typematch;
    function typematch(tu, d, tv) {
        return Versioned_1.at(Versioned_1.ν(), Typematch, tu, d, tv);
    }
    Expl_1.typematch = typematch;
    // v is the resolved value of x
    class Var extends Expl {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.v = Value_1._;
        }
    }
    Expl_1.Var = Var;
    function var_(x, v) {
        return Versioned_1.at(Versioned_1.ν(), Var, x, v);
    }
    Expl_1.var_ = var_;
})(Expl = exports.Expl || (exports.Expl = {}));


/***/ }),

/***/ "./src/Expr.ts":
/*!*********************!*\
  !*** ./src/Expr.ts ***!
  \*********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Ord_1 = __webpack_require__(/*! ./util/Ord */ "./src/util/Ord.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const FiniteMap_1 = __webpack_require__(/*! ./FiniteMap */ "./src/FiniteMap.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
// Constants used for parsing, and also for toString() implementations.
var strings;
(function (strings) {
    strings.arrow = "→";
    strings.as = "as";
    strings.bracketL = "[";
    strings.bracketR = "]";
    strings.equals = "=";
    strings.fun = "fun";
    strings.in_ = "in";
    strings.let_ = "let";
    strings.letRec = "letrec";
    strings.match = "match";
    strings.primitive = "primitive";
    strings.parenL = "(";
    strings.parenR = ")";
    strings.quotes = '"';
    strings.typematch = "typematch";
})(strings = exports.strings || (exports.strings = {}));
var Expr;
(function (Expr_1) {
    // Unrelated to the annotation lattice. Expr case intentionally only defined for higher-order (function) case.
    function join(κ, κʹ) {
        if (κ instanceof Trie.Trie && κʹ instanceof Trie.Trie) {
            return Trie.Trie.join(κ, κʹ);
        }
        else if (κ instanceof Fun && κʹ instanceof Fun) {
            return fun(Versioned_1.ν(), join(κ.σ, κʹ.σ));
        }
        else {
            return Core_1.absurd("Undefined join.", κ, κʹ);
        }
    }
    class Expr extends Annotated_1.AnnotatedC(DataValue_1.DataValue) {
    }
    Expr_1.Expr = Expr;
    class App extends Expr {
        constructor() {
            super(...arguments);
            this.f = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.App = App;
    function app(k, f, e) {
        return Versioned_1.at(k, App, f, e);
    }
    Expr_1.app = app;
    class BinaryApp extends Expr {
        constructor() {
            super(...arguments);
            this.e1 = Value_1._;
            this.opName = Value_1._;
            this.e2 = Value_1._;
        }
    }
    Expr_1.BinaryApp = BinaryApp;
    function binaryApp(k, e1, opName, e2) {
        return Versioned_1.at(k, BinaryApp, e1, opName, e2);
    }
    Expr_1.binaryApp = binaryApp;
    class ConstNum extends Expr {
        constructor() {
            super(...arguments);
            this.val = Value_1._;
        }
    }
    Expr_1.ConstNum = ConstNum;
    function constNum(k, val) {
        return Versioned_1.at(k, ConstNum, val);
    }
    Expr_1.constNum = constNum;
    class ConstStr extends Expr {
        constructor() {
            super(...arguments);
            this.val = Value_1._;
        }
    }
    Expr_1.ConstStr = ConstStr;
    function constStr(k, val) {
        return Versioned_1.at(k, ConstStr, val);
    }
    Expr_1.constStr = constStr;
    class Constr extends Expr {
        constructor() {
            super(...arguments);
            this.ctr = Value_1._;
            this.args = Value_1._;
        }
    }
    Expr_1.Constr = Constr;
    function constr(k, ctr, args) {
        return Versioned_1.at(k, Constr, ctr, args);
    }
    Expr_1.constr = constr;
    // Because let/letrec no longer have "bodies", there's no real need for them to be separately versioned;
    // the variables they introduce are.
    class Def extends DataValue_1.DataValue {
    }
    Expr_1.Def = Def;
    class Let extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.Let = Let;
    function let_(x, e) {
        return Value_1.make(Let, x, e);
    }
    Expr_1.let_ = let_;
    class Prim extends Def {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
        }
    }
    Expr_1.Prim = Prim;
    function prim(x) {
        return Value_1.make(Prim, x);
    }
    Expr_1.prim = prim;
    class RecDef extends DataValue_1.DataValue {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
            this.σ = Value_1._;
        }
    }
    Expr_1.RecDef = RecDef;
    function recDef(x, σ) {
        return Value_1.make(RecDef, x, σ);
    }
    Expr_1.recDef = recDef;
    class LetRec extends Def {
        constructor() {
            super(...arguments);
            this.δ = Value_1._;
        }
    }
    Expr_1.LetRec = LetRec;
    function letRec(δ) {
        return Value_1.make(LetRec, δ);
    }
    Expr_1.letRec = letRec;
    class Defs extends Expr {
        constructor() {
            super(...arguments);
            this.def̅ = Value_1._;
            this.e = Value_1._;
        }
    }
    Expr_1.Defs = Defs;
    function defs(k, def̅, e) {
        return Versioned_1.at(k, Defs, def̅, e);
    }
    Expr_1.defs = defs;
    class Fun extends Expr {
        constructor() {
            super(...arguments);
            this.σ = Value_1._;
        }
    }
    Expr_1.Fun = Fun;
    function fun(k, σ) {
        return Versioned_1.at(k, Fun, σ);
    }
    Expr_1.fun = fun;
    class MatchAs extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
            this.σ = Value_1._;
        }
    }
    Expr_1.MatchAs = MatchAs;
    function matchAs(k, e, σ) {
        return Versioned_1.at(k, MatchAs, e, σ);
    }
    Expr_1.matchAs = matchAs;
    class Quote extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
        }
    }
    Expr_1.Quote = Quote;
    function quote(k, e) {
        return Versioned_1.at(k, Quote, e);
    }
    Expr_1.quote = quote;
    class Typematch extends Expr {
        constructor() {
            super(...arguments);
            this.e = Value_1._;
            this.cases = Value_1._;
        }
    }
    Expr_1.Typematch = Typematch;
    function typematch(k, e, cases) {
        return Versioned_1.at(k, Typematch, e, cases);
    }
    Expr_1.typematch = typematch;
    class Var extends Expr {
        constructor() {
            super(...arguments);
            this.x = Value_1._;
        }
    }
    Expr_1.Var = Var;
    function var_(k, x) {
        return Versioned_1.at(k, Var, x);
    }
    Expr_1.var_ = var_;
    let Trie;
    (function (Trie_1) {
        class Trie extends DataValue_1.DataValue {
            static join(σ, τ) {
                if (Var.is(σ) && Var.is(τ) && Ord_1.eq(σ.x, τ.x)) {
                    return var_(σ.x, join(σ.κ, τ.κ));
                }
                else if (Constr.is(σ) && Constr.is(τ)) {
                    // Both maps (which are non-empty) can (inductively) be assumed to have keys taken from the 
                    // same datatype. Ensure that invariant is preserved:
                    const c_σ = σ.cases.toArray()[0].fst.val, c_τ = τ.cases.toArray()[0].fst.val;
                    if (DataType_1.ctrToDataType.get(c_σ) !== DataType_1.ctrToDataType.get(c_τ)) {
                        Core_1.error(`${c_σ} and ${c_τ} are constructors of different datatypes.`);
                    }
                    return constr(FiniteMap_1.unionWith(σ.cases, τ.cases, join));
                }
                else {
                    return Core_1.absurd("Undefined join.", σ, τ);
                }
            }
        }
        Trie_1.Trie = Trie;
        class Constr extends Trie {
            constructor() {
                super(...arguments);
                this.cases = Value_1._;
            }
            static is(σ) {
                return σ instanceof Constr;
            }
        }
        Trie_1.Constr = Constr;
        function constr(cases) {
            return Value_1.make(Constr, cases);
        }
        Trie_1.constr = constr;
        // TODO: use annotations on x.
        class Var extends Trie {
            constructor() {
                super(...arguments);
                this.x = Value_1._;
                this.κ = Value_1._;
            }
            static is(σ) {
                return σ instanceof Var;
            }
        }
        Trie_1.Var = Var;
        function var_(x, κ) {
            return Value_1.make(Var, x, κ);
        }
        Trie_1.var_ = var_;
    })(Trie = Expr_1.Trie || (Expr_1.Trie = {}));
})(Expr = exports.Expr || (exports.Expr = {}));


/***/ }),

/***/ "./src/FiniteMap.ts":
/*!**************************!*\
  !*** ./src/FiniteMap.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const BaseTypes_1 = __webpack_require__(/*! ./BaseTypes */ "./src/BaseTypes.ts");
function get(m, k) {
    if (BaseTypes_1.NonEmpty.is(m)) {
        if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
                return m.t.snd;
            }
            else {
                return get(m.left, k);
            }
        }
        else {
            return get(m.right, k);
        }
    }
    else if (BaseTypes_1.Empty.is(m)) {
        return undefined;
    }
    else {
        return Core_1.absurd();
    }
}
exports.get = get;
function insert(m, k, v) {
    if (BaseTypes_1.NonEmpty.is(m)) {
        if (k.leq(m.t.fst)) {
            if (m.t.fst.leq(k)) {
                return BaseTypes_1.nonEmpty(m.left, BaseTypes_1.pair(k, v), m.right);
            }
            else {
                return BaseTypes_1.nonEmpty(insert(m.left, k, v), m.t, m.right);
            }
        }
        else {
            return BaseTypes_1.nonEmpty(m.left, m.t, insert(m.right, k, v));
        }
    }
    else if (BaseTypes_1.Empty.is(m)) {
        return BaseTypes_1.nonEmpty(m, BaseTypes_1.pair(k, v), m);
    }
    else {
        return Core_1.absurd();
    }
}
exports.insert = insert;
function singleton(k, v) {
    return insert(BaseTypes_1.empty(), k, v);
}
exports.singleton = singleton;
// Union with a combining function. Avoid primes in signature; seems to be incompatible with version 
// of ts-loader used by Wrattler.
function unionWith(m1, m2, f) {
    if (BaseTypes_1.NonEmpty.is(m2)) {
        const k = m2.t.fst, v = m2.t.snd, vʹ = get(m1, k), u = vʹ === undefined ? v : f(v, vʹ);
        return unionWith(insert(unionWith(m1, m2.left, f), k, u), m2.right, f);
    }
    else if (BaseTypes_1.Empty.is(m2)) {
        return m1;
    }
    else {
        return Core_1.absurd();
    }
}
exports.unionWith = unionWith;


/***/ }),

/***/ "./src/Graphics.ts":
/*!*************************!*\
  !*** ./src/Graphics.ts ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
// Basic graphical datatypes.
class Rect extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.width = Value_1._;
        this.height = Value_1._;
    }
}
exports.Rect = Rect;
class Point extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
    }
    toString() {
        return `Point(${this.x},${this.y})`;
    }
}
exports.Point = Point;
class GraphicsElement extends DataValue_1.DataValue {
}
exports.GraphicsElement = GraphicsElement;
class Graphic extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.gs = Value_1._;
    }
}
exports.Graphic = Graphic;
class Polyline extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.points = Value_1._;
    }
}
exports.Polyline = Polyline;
class Polygon extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.points = Value_1._;
        this.stroke = Value_1._;
        this.fill = Value_1._;
    }
}
exports.Polygon = Polygon;
class Text extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.str = Value_1._;
    }
}
exports.Text = Text;
// Omit scaling, rotation, etc for now; would require externalisation to SVG to handle text properly.
class Translate extends GraphicsElement {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.y = Value_1._;
        this.g = Value_1._;
    }
}
exports.Translate = Translate;
DataType_1.initDataType(GraphicsElement, [Polygon, Polyline, Text, Translate, Graphic]);
DataType_1.initDataType(Point, [Point]);
DataType_1.initDataType(Rect, [Rect]);


/***/ }),

/***/ "./src/Lib.ts":
/*!********************!*\
  !*** ./src/Lib.ts ***!
  \********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
// "typings" property of package.json requires a single TypeScript interface.
const Eval_1 = __webpack_require__(/*! ./Eval */ "./src/Eval.ts");
exports.Eval = Eval_1.Eval;
const Module_1 = __webpack_require__(/*! ./Module */ "./src/Module.ts");
exports.parseWithImports = Module_1.parseWithImports;


/***/ }),

/***/ "./src/Match.ts":
/*!**********************!*\
  !*** ./src/Match.ts ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Lattice_1 = __webpack_require__(/*! ./util/Lattice */ "./src/util/Lattice.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const BaseTypes_1 = __webpack_require__(/*! ./BaseTypes */ "./src/BaseTypes.ts");
const DataValue_1 = __webpack_require__(/*! ./DataValue */ "./src/DataValue.ts");
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const Env_1 = __webpack_require__(/*! ./Env */ "./src/Env.ts");
const Expr_1 = __webpack_require__(/*! ./Expr */ "./src/Expr.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
var Trie = Expr_1.Expr.Trie;
// Conceptually (syntactic) tries map to (semantic) elim forms, and exprs map to exprs; no easy way to 
// express this in the type system.
function evalTrie(σ) {
    return evalTrie_(σ);
}
exports.evalTrie = evalTrie;
function evalTrie_(σ) {
    if (Trie.Var.is(σ)) {
        return varElim(σ.x, evalCont(σ.κ));
    }
    else if (Trie.Constr.is(σ)) {
        const cases = σ.cases.toArray(), c̅ = cases.map(({ fst: c }) => c.val), d = Core_1.__nonNull(DataType_1.ctrToDataType.get(c̅[0])), c̅ʹ = [...d.ctrs.keys()], // also sorted
        f̅ = [];
        let n = 0;
        for (let nʹ = 0; nʹ < c̅ʹ.length; ++nʹ) {
            if (c̅.includes(c̅ʹ[nʹ])) {
                f̅.push(evalCont(cases[n++].snd));
            }
            else {
                f̅.push(undefined);
            }
        }
        Core_1.assert(n === cases.length);
        return Value_1.make(d.elimC, ...f̅);
    }
    else {
        return Core_1.absurd();
    }
}
function evalCont(κ) {
    if (κ instanceof Trie.Trie) {
        const σ = κ;
        return evalTrie(σ);
    }
    else if (κ instanceof Expr_1.Expr.Expr) {
        return κ;
    }
    else {
        return Core_1.absurd();
    }
}
class Match extends DataValue_1.DataValue {
    constructor() {
        super(...arguments);
        this.v̅ = Value_1._;
        this.κ = Value_1._;
    }
}
exports.Match = Match;
function match(ξ, κ) {
    return Value_1.make(Match, ξ, κ);
}
exports.match = match;
// See GitHub issue #128.
class Elim extends DataValue_1.DataValue {
    // could have called this "match", but conflicts with factory method of same name
    apply(v) {
        return this.apply_(v, BaseTypes_1.nil());
    }
}
exports.Elim = Elim;
// Parser ensures constructor calls are saturated.
function matchArgs(κ, v̅, u̅) {
    if (v̅.length === 0) {
        return [Env_1.emptyEnv(), match(u̅, κ)];
    }
    else {
        const [v, ...v̅ʹ] = v̅;
        if (κ instanceof Elim) {
            const f = κ, // "unfold" K into Elim<K>
            [ρ, ξ] = f.apply_(v, u̅), [ρʹ, ξʹ] = matchArgs(ξ.κ, v̅ʹ, ξ.v̅);
            return [ρ.concat(ρʹ), ξʹ];
        }
        else {
            return Core_1.absurd("Too many arguments to constructor.");
        }
    }
}
// No need to parameterise these two claseses over subtypes of RuntimeCont because only ever use them at RuntimeCont 
// itself. Concrete instances have a field per constructor, in *lexicographical* order.
class DataElim extends Elim {
    apply_(v, u̅) {
        const c = Core_1.className(v);
        if (v instanceof DataValue_1.DataValue) {
            const κ = this[c];
            if (κ !== undefined) {
                const v̅ = v.fieldValues().map(v => Annotated_1.asAnnotated(v)), [ρ, ξ] = matchArgs(κ, v̅, u̅);
                return [ρ, match(BaseTypes_1.cons(v, ξ.v̅), ξ.κ)];
            }
            else {
                const d = DataType_1.elimToDataType.get(Core_1.className(this));
                if (d.ctrs.has(c)) {
                    return Core_1.error(`Pattern mismatch: ${c} case is undefined for ${d.name.val} eliminator.`);
                }
                else {
                    return Core_1.error(`Pattern mismatch: found ${c}, expected ${d.name.val}.`);
                }
            }
        }
        else {
            return Core_1.error(`Pattern mismatch: ${c} is not a datatype.`, v, this);
        }
    }
}
exports.DataElim = DataElim;
class VarElim extends Elim {
    constructor() {
        super(...arguments);
        this.x = Value_1._;
        this.κ = Value_1._;
    }
    apply_(v, ξ) {
        return [Env_1.Env.singleton(this.x, v), match(ξ, this.κ)];
    }
}
function varElim(x, κ) {
    return Value_1.make(VarElim, x, κ);
}
function match_fwd(ξ) {
    return ξ.v̅.toArray().reduce((α, v) => Lattice_1.ann.meet(α, v.__α), Lattice_1.ann.top);
}
exports.match_fwd = match_fwd;
function match_bwd(ξ, α) {
    ξ.v̅.toArray().map(v => Annotated_1.setα(α, v));
}
exports.match_bwd = match_bwd;


/***/ }),

/***/ "./src/Module.ts":
/*!***********************!*\
  !*** ./src/Module.ts ***!
  \***********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const nearley_1 = __webpack_require__(/*! nearley */ "./node_modules/nearley/lib/nearley.js");
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const Env_1 = __webpack_require__(/*! ./Env */ "./src/Env.ts");
const Eval_1 = __webpack_require__(/*! ./Eval */ "./src/Eval.ts");
const Expr_1 = __webpack_require__(/*! ./Expr */ "./src/Expr.ts");
__webpack_require__(/*! ./Graphics */ "./src/Graphics.ts"); // for datatypes
const grammar = __webpack_require__(/*! ./Parse */ "./src/Parse.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
// Define as constants to enforce sharing; could use memoisation.
exports.module_prelude = loadModule("prelude"), exports.module_graphics = loadModule("graphics"), exports.module_renderData = loadModule("renderData");
function import_(modules, e) {
    if (modules.length === 0) {
        return e;
    }
    else {
        return Expr_1.Expr.defs(Versioned_1.ν(), modules[0], import_(modules.slice(1), e));
    }
}
function importDefaults(e) {
    return import_([exports.module_prelude], e);
}
exports.importDefaults = importDefaults;
function loadTestFile(folder, file) {
    let text;
    const xmlhttp = new XMLHttpRequest;
    xmlhttp.open("GET", "./" + folder + "/" + file + ".lcalc", false);
    xmlhttp.send();
    if (xmlhttp.status === 200) {
        text = xmlhttp.responseText;
    }
    return Core_1.__nonNull(text);
}
exports.loadTestFile = loadTestFile;
// Not sure if Nearley can parse arbitrary non-terminal, as opposed to root.
function loadModule(file) {
    const fileʹ = loadTestFile("lcalc/lib", file) + " in 0", e = Core_1.as(successfulParse(fileʹ), Expr_1.Expr.Defs);
    return e.def̅;
}
exports.loadModule = loadModule;
function open(file) {
    return openWithImports(file, []);
}
exports.open = open;
function openWithImports(file, modules) {
    return parseWithImports(loadTestFile("lcalc/example", file), modules);
}
exports.openWithImports = openWithImports;
function openDatasetAs(file, x) {
    const e = parseWithImports(loadTestFile("lcalc/dataset", file), []);
    return Env_1.Env.singleton(Annotated_1.str(x), Eval_1.Eval.eval_(Env_1.emptyEnv(), e).v);
}
exports.openDatasetAs = openDatasetAs;
function parseWithImports(src, modules) {
    return importDefaults(import_(modules, successfulParse(src)));
}
exports.parseWithImports = parseWithImports;
// https://github.com/kach/nearley/issues/276#issuecomment-324162234
function successfulParse(str) {
    const results = new nearley_1.Parser(nearley_1.Grammar.fromCompiled(grammar)).feed(str).results;
    if (results.length > 1) {
        Core_1.error("Ambiguous parse.");
    }
    return results[0];
}
exports.successfulParse = successfulParse;


/***/ }),

/***/ "./src/Parse.ts":
/*!**********************!*\
  !*** ./src/Parse.ts ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
// Generated automatically by nearley, version 2.16.0
// http://github.com/Hardmath123/nearley
// Bypasses TS6133. Allow declared but unused functions.
// @ts-ignore
function id(d) { return d[0]; }
const moo = __webpack_require__(/*! moo */ "./node_modules/moo/moo.js");
const lexer = moo.compile({
    ident: {
        match: /[a-zA-Z_][0-9a-zA-Z_]*'*/,
        type: moo.keywords({
            keyword: ["as", "match", "fun", "in", "let", "letrec", "primitive", "typematch"],
        })
    },
    whitespace: {
        match: /[ \f\t\r\n]+/,
        lineBreaks: true
    },
    singleLineComment: /\/\/.*$/,
    // JSON grammar for numbers, https://tools.ietf.org/html/rfc7159.html#section-6.
    // Seems Moo requires us to use non-capturing groups (?:)
    number: /\-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[e|E][-|+]?[0-9]+)?/,
    string: /"(?:\\["\\]|[^\n"\\])*"/,
    // not quite sure why I can't use literals here:
    sumOp: /\-|\+\+|\+/,
    exponentOp: /\*\*/,
    productOp: /\*|\//,
    compareOp: /===|==|<==|<=|<|>==|>=|>/,
    symbol: ["(", ")", "=", "→", ";", "{", "}", ",", "[", "]", "..."],
});
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const BaseTypes_1 = __webpack_require__(/*! ./BaseTypes */ "./src/BaseTypes.ts");
const DataType_1 = __webpack_require__(/*! ./DataType */ "./src/DataType.ts");
const Expr_1 = __webpack_require__(/*! ./Expr */ "./src/Expr.ts");
const FiniteMap_1 = __webpack_require__(/*! ./FiniteMap */ "./src/FiniteMap.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
var Trie = Expr_1.Expr.Trie;
// Constructors must start with an uppercase letter, a la Haskell. Will fix this as part of issue #49.
function isCtr(str) {
    const ch = str.charAt(0);
    return ch === ch.toUpperCase() && ch !== ch.toLowerCase();
}
function compose(mk_κ1, mk_κ2) {
    return (κ) => mk_κ1(mk_κ2(κ));
}
;
;
;
exports.Lexer = lexer;
exports.ParserRules = [
    { "name": "rootExpr", "symbols": ["_", "expr"], "postprocess": ([, e]) => e },
    { "name": "rootExpr", "symbols": ["expr"], "postprocess": id },
    { "name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("whitespace") ? { type: "whitespace" } : whitespace)] },
    { "name": "_$ebnf$1$subexpression$1", "symbols": [(lexer.has("singleLineComment") ? { type: "singleLineComment" } : singleLineComment)] },
    { "name": "_$ebnf$1", "symbols": ["_$ebnf$1$subexpression$1"] },
    { "name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("whitespace") ? { type: "whitespace" } : whitespace)] },
    { "name": "_$ebnf$1$subexpression$2", "symbols": [(lexer.has("singleLineComment") ? { type: "singleLineComment" } : singleLineComment)] },
    { "name": "_$ebnf$1", "symbols": ["_$ebnf$1", "_$ebnf$1$subexpression$2"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "_", "symbols": ["_$ebnf$1"] },
    { "name": "expr", "symbols": ["compareExpr"], "postprocess": id },
    { "name": "expr", "symbols": ["defs1"], "postprocess": id },
    { "name": "expr", "symbols": ["fun"], "postprocess": id },
    { "name": "expr", "symbols": ["matchAs"], "postprocess": id },
    { "name": "expr", "symbols": ["typematch"], "postprocess": id },
    { "name": "defs1$macrocall$2", "symbols": [{ "literal": "in" }] },
    { "name": "defs1$macrocall$1$macrocall$2", "symbols": ["defs1$macrocall$2"] },
    { "name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "defs1$macrocall$1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "defs1$macrocall$1", "symbols": ["defs1$macrocall$1$macrocall$1"] },
    { "name": "defs1", "symbols": ["defList", "defs1$macrocall$1", "expr"], "postprocess": ([defs, , e]) => Expr_1.Expr.defs(Versioned_1.ν(), defs, e) },
    { "name": "compareExpr", "symbols": ["compareExpr", "compareOp", "sumExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(Versioned_1.ν(), e1, Annotated_1.str(op), e2) },
    { "name": "compareExpr", "symbols": ["sumExpr"], "postprocess": id },
    { "name": "sumExpr", "symbols": ["sumExpr", "sumOp", "productExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(Versioned_1.ν(), e1, Annotated_1.str(op), e2) },
    { "name": "sumExpr", "symbols": ["productExpr"], "postprocess": id },
    { "name": "productExpr", "symbols": ["productExpr", "productOp", "exponentExpr"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(Versioned_1.ν(), e1, Annotated_1.str(op), e2) },
    { "name": "productExpr", "symbols": ["exponentExpr"], "postprocess": id },
    { "name": "exponentExpr", "symbols": ["exponentExpr", "exponentOp", "appChain"], "postprocess": ([e1, op, e2]) => Expr_1.Expr.binaryApp(Versioned_1.ν(), e1, Annotated_1.str(op), e2) },
    { "name": "exponentExpr", "symbols": ["appChain"], "postprocess": id },
    { "name": "appChain", "symbols": ["simpleExpr"], "postprocess": id },
    { "name": "appChain", "symbols": ["appChain", "simpleExpr"], "postprocess": ([e1, e2]) => Expr_1.Expr.app(Versioned_1.ν(), e1, e2) },
    { "name": "simpleExpr", "symbols": ["variable"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["string"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["number"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["parenthExpr"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["pair"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["list"], "postprocess": id },
    { "name": "simpleExpr", "symbols": ["constr"], "postprocess": id },
    { "name": "variable", "symbols": ["var"], "postprocess": ([x]) => Expr_1.Expr.var_(Versioned_1.ν(), x) },
    { "name": "var$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
    { "name": "var$macrocall$1", "symbols": ["var$macrocall$2"], "postprocess": id },
    { "name": "var$macrocall$1", "symbols": ["var$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "var", "symbols": ["var$macrocall$1"], "postprocess": ([[x]], _, reject) => {
            if (isCtr(x.value)) {
                return reject;
            }
            return Annotated_1.str(x.value);
        } },
    { "name": "string$macrocall$2", "symbols": [(lexer.has("string") ? { type: "string" } : string)] },
    { "name": "string$macrocall$1", "symbols": ["string$macrocall$2"], "postprocess": id },
    { "name": "string$macrocall$1", "symbols": ["string$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "string", "symbols": ["string$macrocall$1"], "postprocess": ([[lit]]) => Expr_1.Expr.constStr(Versioned_1.ν(), Annotated_1.str(lit.value.slice(1, -1))) },
    { "name": "number$macrocall$2", "symbols": [(lexer.has("number") ? { type: "number" } : number)] },
    { "name": "number$macrocall$1", "symbols": ["number$macrocall$2"], "postprocess": id },
    { "name": "number$macrocall$1", "symbols": ["number$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "number", "symbols": ["number$macrocall$1"], "postprocess": ([[lit]]) => Expr_1.Expr.constNum(Versioned_1.ν(), Annotated_1.num(new Number(lit.value).valueOf())) },
    { "name": "parenthExpr$macrocall$2", "symbols": [{ "literal": "(" }] },
    { "name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2"], "postprocess": id },
    { "name": "parenthExpr$macrocall$1", "symbols": ["parenthExpr$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "parenthExpr$macrocall$4", "symbols": [{ "literal": ")" }] },
    { "name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4"], "postprocess": id },
    { "name": "parenthExpr$macrocall$3", "symbols": ["parenthExpr$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "parenthExpr", "symbols": ["parenthExpr$macrocall$1", "expr", "parenthExpr$macrocall$3"], "postprocess": ([, e,]) => e },
    { "name": "pair$macrocall$2", "symbols": [{ "literal": "(" }] },
    { "name": "pair$macrocall$1", "symbols": ["pair$macrocall$2"], "postprocess": id },
    { "name": "pair$macrocall$1", "symbols": ["pair$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "pair$macrocall$4", "symbols": [{ "literal": "," }] },
    { "name": "pair$macrocall$3", "symbols": ["pair$macrocall$4"], "postprocess": id },
    { "name": "pair$macrocall$3", "symbols": ["pair$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "pair$macrocall$6", "symbols": [{ "literal": ")" }] },
    { "name": "pair$macrocall$5", "symbols": ["pair$macrocall$6"], "postprocess": id },
    { "name": "pair$macrocall$5", "symbols": ["pair$macrocall$6", "_"], "postprocess": ([x,]) => x },
    { "name": "pair", "symbols": ["pair$macrocall$1", "expr", "pair$macrocall$3", "expr", "pair$macrocall$5"], "postprocess": ([, e1, , e2,]) => Expr_1.Expr.constr(Versioned_1.ν(), Annotated_1.str(BaseTypes_1.Pair.name), BaseTypes_1.List.fromArray([e1, e2])) },
    { "name": "list$macrocall$2", "symbols": [{ "literal": "[" }] },
    { "name": "list$macrocall$1", "symbols": ["list$macrocall$2"], "postprocess": id },
    { "name": "list$macrocall$1", "symbols": ["list$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "list$macrocall$4", "symbols": [{ "literal": "]" }] },
    { "name": "list$macrocall$3", "symbols": ["list$macrocall$4"], "postprocess": id },
    { "name": "list$macrocall$3", "symbols": ["list$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "list", "symbols": ["list$macrocall$1", "listOpt", "list$macrocall$3"], "postprocess": ([, e,]) => e },
    { "name": "constr", "symbols": ["ctr", "args"], "postprocess": ([c, e̅], _, reject) => {
            Core_1.assert(c instanceof Value_1.Str);
            if (DataType_1.arity(c) !== e̅.length) {
                return reject;
            }
            return Expr_1.Expr.constr(Versioned_1.ν(), c, BaseTypes_1.List.fromArray(e̅));
        } },
    { "name": "ctr$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
    { "name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2"], "postprocess": id },
    { "name": "ctr$macrocall$1", "symbols": ["ctr$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "ctr", "symbols": ["ctr$macrocall$1"], "postprocess": ([[x]], _, reject) => {
            if (!isCtr(x.value)) {
                return reject;
            }
            return Annotated_1.str(x.value);
        } },
    { "name": "args", "symbols": [], "postprocess": () => [] },
    { "name": "args$macrocall$2", "symbols": [{ "literal": "(" }] },
    { "name": "args$macrocall$1", "symbols": ["args$macrocall$2"], "postprocess": id },
    { "name": "args$macrocall$1", "symbols": ["args$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "args$ebnf$1", "symbols": [] },
    { "name": "args$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
    { "name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "args$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "args$ebnf$1$subexpression$1", "symbols": ["args$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e },
    { "name": "args$ebnf$1", "symbols": ["args$ebnf$1", "args$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "args$macrocall$4", "symbols": [{ "literal": ")" }] },
    { "name": "args$macrocall$3", "symbols": ["args$macrocall$4"], "postprocess": id },
    { "name": "args$macrocall$3", "symbols": ["args$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "args", "symbols": ["args$macrocall$1", "expr", "args$ebnf$1", "args$macrocall$3"], "postprocess": ([, e, es,]) => [e, ...es] },
    { "name": "typematch$macrocall$2", "symbols": [{ "literal": "typematch" }] },
    { "name": "typematch$macrocall$1$macrocall$2", "symbols": ["typematch$macrocall$2"] },
    { "name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "typematch$macrocall$1$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typematch$macrocall$1", "symbols": ["typematch$macrocall$1$macrocall$1"] },
    { "name": "typematch$macrocall$4", "symbols": [{ "literal": "as" }] },
    { "name": "typematch$macrocall$3$macrocall$2", "symbols": ["typematch$macrocall$4"] },
    { "name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2"], "postprocess": id },
    { "name": "typematch$macrocall$3$macrocall$1", "symbols": ["typematch$macrocall$3$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typematch$macrocall$3", "symbols": ["typematch$macrocall$3$macrocall$1"] },
    { "name": "typematch", "symbols": ["typematch$macrocall$1", "expr", "typematch$macrocall$3", "typeMatches"], "postprocess": ([, e, , m]) => Expr_1.Expr.typematch(Versioned_1.ν(), e, m) },
    { "name": "defList$ebnf$1", "symbols": [] },
    { "name": "defList$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
    { "name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "defList$ebnf$1$subexpression$1$macrocall$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "defList$ebnf$1$subexpression$1", "symbols": ["defList$ebnf$1$subexpression$1$macrocall$1", "def"], "postprocess": ([, def]) => def },
    { "name": "defList$ebnf$1", "symbols": ["defList$ebnf$1", "defList$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "defList", "symbols": ["def", "defList$ebnf$1"], "postprocess": ([def, defs]) => BaseTypes_1.List.fromArray([def, ...defs]) },
    { "name": "def", "symbols": ["let"], "postprocess": id },
    { "name": "def", "symbols": ["letrec"], "postprocess": id },
    { "name": "def", "symbols": ["prim"], "postprocess": id },
    { "name": "let$macrocall$2", "symbols": [{ "literal": "let" }] },
    { "name": "let$macrocall$1$macrocall$2", "symbols": ["let$macrocall$2"] },
    { "name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "let$macrocall$1$macrocall$1", "symbols": ["let$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "let$macrocall$1", "symbols": ["let$macrocall$1$macrocall$1"] },
    { "name": "let$macrocall$4", "symbols": [{ "literal": "=" }] },
    { "name": "let$macrocall$3", "symbols": ["let$macrocall$4"], "postprocess": id },
    { "name": "let$macrocall$3", "symbols": ["let$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "let", "symbols": ["let$macrocall$1", "var", "let$macrocall$3", "expr"], "postprocess": ([, x, , e]) => Expr_1.Expr.let_(x, e) },
    { "name": "letrec$macrocall$2", "symbols": [{ "literal": "letrec" }] },
    { "name": "letrec$macrocall$1$macrocall$2", "symbols": ["letrec$macrocall$2"] },
    { "name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "letrec$macrocall$1$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "letrec$macrocall$1", "symbols": ["letrec$macrocall$1$macrocall$1"] },
    { "name": "letrec$ebnf$1", "symbols": [] },
    { "name": "letrec$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
    { "name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "letrec$ebnf$1$subexpression$1$macrocall$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "letrec$ebnf$1$subexpression$1", "symbols": ["letrec$ebnf$1$subexpression$1$macrocall$1", "recDef"], "postprocess": ([, recDef]) => recDef },
    { "name": "letrec$ebnf$1", "symbols": ["letrec$ebnf$1", "letrec$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "letrec", "symbols": ["letrec$macrocall$1", "recDef", "letrec$ebnf$1"], "postprocess": ([, recDef, δ]) => Expr_1.Expr.letRec(BaseTypes_1.List.fromArray([recDef, ...δ])) },
    { "name": "prim$macrocall$2", "symbols": [{ "literal": "primitive" }] },
    { "name": "prim$macrocall$1$macrocall$2", "symbols": ["prim$macrocall$2"] },
    { "name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "prim$macrocall$1$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "prim$macrocall$1", "symbols": ["prim$macrocall$1$macrocall$1"] },
    { "name": "prim", "symbols": ["prim$macrocall$1", "var"], "postprocess": ([, x]) => Expr_1.Expr.prim(x) },
    { "name": "recDef$macrocall$2", "symbols": [{ "literal": "fun" }] },
    { "name": "recDef$macrocall$1$macrocall$2", "symbols": ["recDef$macrocall$2"] },
    { "name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "recDef$macrocall$1$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "recDef$macrocall$1", "symbols": ["recDef$macrocall$1$macrocall$1"] },
    { "name": "recDef", "symbols": ["recDef$macrocall$1", "var", "matches"], "postprocess": ([, f, σ]) => Expr_1.Expr.recDef(f, σ) },
    { "name": "fun$macrocall$2", "symbols": [{ "literal": "fun" }] },
    { "name": "fun$macrocall$1$macrocall$2", "symbols": ["fun$macrocall$2"] },
    { "name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "fun$macrocall$1$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "fun$macrocall$1", "symbols": ["fun$macrocall$1$macrocall$1"] },
    { "name": "fun", "symbols": ["fun$macrocall$1", "matches"], "postprocess": ([, σ]) => Expr_1.Expr.fun(Versioned_1.ν(), σ) },
    { "name": "matchAs$macrocall$2", "symbols": [{ "literal": "match" }] },
    { "name": "matchAs$macrocall$1$macrocall$2", "symbols": ["matchAs$macrocall$2"] },
    { "name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2"], "postprocess": id },
    { "name": "matchAs$macrocall$1$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "matchAs$macrocall$1", "symbols": ["matchAs$macrocall$1$macrocall$1"] },
    { "name": "matchAs$macrocall$4", "symbols": [{ "literal": "as" }] },
    { "name": "matchAs$macrocall$3$macrocall$2", "symbols": ["matchAs$macrocall$4"] },
    { "name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2"], "postprocess": id },
    { "name": "matchAs$macrocall$3$macrocall$1", "symbols": ["matchAs$macrocall$3$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "matchAs$macrocall$3", "symbols": ["matchAs$macrocall$3$macrocall$1"] },
    { "name": "matchAs", "symbols": ["matchAs$macrocall$1", "expr", "matchAs$macrocall$3", "matches"], "postprocess": ([, e, , σ]) => Expr_1.Expr.matchAs(Versioned_1.ν(), e, σ) },
    { "name": "matches", "symbols": ["match"], "postprocess": id },
    { "name": "matches$macrocall$2", "symbols": [{ "literal": "{" }] },
    { "name": "matches$macrocall$1", "symbols": ["matches$macrocall$2"], "postprocess": id },
    { "name": "matches$macrocall$1", "symbols": ["matches$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "matches$ebnf$1", "symbols": [] },
    { "name": "matches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
    { "name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "matches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "matches$ebnf$1$subexpression$1", "symbols": ["matches$ebnf$1$subexpression$1$macrocall$1", "match"], "postprocess": ([, m]) => m },
    { "name": "matches$ebnf$1", "symbols": ["matches$ebnf$1", "matches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "matches$macrocall$4", "symbols": [{ "literal": "}" }] },
    { "name": "matches$macrocall$3", "symbols": ["matches$macrocall$4"], "postprocess": id },
    { "name": "matches$macrocall$3", "symbols": ["matches$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "matches", "symbols": ["matches$macrocall$1", "match", "matches$ebnf$1", "matches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce(Trie.Trie.join) },
    { "name": "match$macrocall$2", "symbols": [{ "literal": "→" }] },
    { "name": "match$macrocall$1", "symbols": ["match$macrocall$2"], "postprocess": id },
    { "name": "match$macrocall$1", "symbols": ["match$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "match", "symbols": ["pattern", "match$macrocall$1", "expr"], "postprocess": ([mk_κ, , e]) => mk_κ(e) },
    { "name": "match", "symbols": ["pattern", "matches"], "postprocess": ([mk_κ1, σ]) => mk_κ1(Expr_1.Expr.fun(Versioned_1.ν(), σ)) },
    { "name": "typeMatches", "symbols": ["typeMatch"], "postprocess": id },
    { "name": "typeMatches$macrocall$2", "symbols": [{ "literal": "{" }] },
    { "name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2"], "postprocess": id },
    { "name": "typeMatches$macrocall$1", "symbols": ["typeMatches$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typeMatches$ebnf$1", "symbols": [] },
    { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": ";" }] },
    { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "typeMatches$ebnf$1$subexpression$1$macrocall$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typeMatches$ebnf$1$subexpression$1", "symbols": ["typeMatches$ebnf$1$subexpression$1$macrocall$1", "typeMatch"], "postprocess": ([, m]) => m },
    { "name": "typeMatches$ebnf$1", "symbols": ["typeMatches$ebnf$1", "typeMatches$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "typeMatches$macrocall$4", "symbols": [{ "literal": "}" }] },
    { "name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4"], "postprocess": id },
    { "name": "typeMatches$macrocall$3", "symbols": ["typeMatches$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "typeMatches", "symbols": ["typeMatches$macrocall$1", "typeMatch", "typeMatches$ebnf$1", "typeMatches$macrocall$3"], "postprocess": ([, m, ms,]) => [m, ...ms].reduce((m1, m2) => FiniteMap_1.unionWith(m1, m2, (e, eʹ) => Core_1.error("Overlapping typecase branches."))) },
    { "name": "typeMatch$macrocall$2", "symbols": [{ "literal": "→" }] },
    { "name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2"], "postprocess": id },
    { "name": "typeMatch$macrocall$1", "symbols": ["typeMatch$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typeMatch", "symbols": ["typename", "typeMatch$macrocall$1", "expr"], "postprocess": ([x, , e]) => {
            Core_1.assert(x instanceof Value_1.Str);
            if (!DataType_1.types.has(x.val)) {
                Core_1.error(`Type name ${x.val} not found.`);
            }
            return FiniteMap_1.singleton(x, e);
        } },
    { "name": "typename$macrocall$2", "symbols": [(lexer.has("ident") ? { type: "ident" } : ident)] },
    { "name": "typename$macrocall$1", "symbols": ["typename$macrocall$2"], "postprocess": id },
    { "name": "typename$macrocall$1", "symbols": ["typename$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "typename", "symbols": ["typename$macrocall$1"], "postprocess": ([[x]]) => Annotated_1.str(x.value) },
    { "name": "listOpt", "symbols": [], "postprocess": () => Expr_1.Expr.constr(Versioned_1.ν(), Annotated_1.str(BaseTypes_1.Nil.name), BaseTypes_1.nil()) },
    { "name": "listOpt$ebnf$1", "symbols": [] },
    { "name": "listOpt$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
    { "name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "listOpt$ebnf$1$subexpression$1$macrocall$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "listOpt$ebnf$1$subexpression$1", "symbols": ["listOpt$ebnf$1$subexpression$1$macrocall$1", "expr"], "postprocess": ([, e]) => e },
    { "name": "listOpt$ebnf$1", "symbols": ["listOpt$ebnf$1", "listOpt$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "listOpt", "symbols": ["expr", "listOpt$ebnf$1", "listRestOpt"], "postprocess": ([e, es, eʹ]) => [e, ...es, eʹ].reverse().reduce((e̅, e) => Expr_1.Expr.constr(Versioned_1.ν(), Annotated_1.str(BaseTypes_1.Cons.name), BaseTypes_1.List.fromArray([e, e̅]))) },
    { "name": "listRestOpt", "symbols": [], "postprocess": () => Expr_1.Expr.constr(Versioned_1.ν(), Annotated_1.str(BaseTypes_1.Nil.name), BaseTypes_1.nil()) },
    { "name": "listRestOpt$macrocall$2", "symbols": [{ "literal": "," }] },
    { "name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2"], "postprocess": id },
    { "name": "listRestOpt$macrocall$1", "symbols": ["listRestOpt$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "listRestOpt$macrocall$4", "symbols": [{ "literal": "..." }] },
    { "name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4"], "postprocess": id },
    { "name": "listRestOpt$macrocall$3", "symbols": ["listRestOpt$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "listRestOpt", "symbols": ["listRestOpt$macrocall$1", "listRestOpt$macrocall$3", "expr"], "postprocess": ([, , e]) => e },
    { "name": "pattern", "symbols": ["variable_pattern"], "postprocess": id },
    { "name": "pattern", "symbols": ["pair_pattern"], "postprocess": id },
    { "name": "pattern", "symbols": ["list_pattern"], "postprocess": id },
    { "name": "pattern", "symbols": ["constr_pattern"], "postprocess": id },
    { "name": "variable_pattern", "symbols": ["var"], "postprocess": ([x]) => (κ) => Trie.var_(x, κ) },
    { "name": "pair_pattern$macrocall$2", "symbols": [{ "literal": "(" }] },
    { "name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2"], "postprocess": id },
    { "name": "pair_pattern$macrocall$1", "symbols": ["pair_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "pair_pattern$macrocall$4", "symbols": [{ "literal": "," }] },
    { "name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4"], "postprocess": id },
    { "name": "pair_pattern$macrocall$3", "symbols": ["pair_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "pair_pattern$macrocall$6", "symbols": [{ "literal": ")" }] },
    { "name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6"], "postprocess": id },
    { "name": "pair_pattern$macrocall$5", "symbols": ["pair_pattern$macrocall$6", "_"], "postprocess": ([x,]) => x },
    { "name": "pair_pattern", "symbols": ["pair_pattern$macrocall$1", "pattern", "pair_pattern$macrocall$3", "pattern", "pair_pattern$macrocall$5"], "postprocess": ([, mk_κ1, , mk_κ2, ,]) => (κ) => Trie.constr(FiniteMap_1.singleton(Annotated_1.str(BaseTypes_1.Pair.name), compose(mk_κ1, mk_κ2)(κ))) },
    { "name": "list_pattern$macrocall$2", "symbols": [{ "literal": "[" }] },
    { "name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2"], "postprocess": id },
    { "name": "list_pattern$macrocall$1", "symbols": ["list_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "list_pattern$macrocall$4", "symbols": [{ "literal": "]" }] },
    { "name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4"], "postprocess": id },
    { "name": "list_pattern$macrocall$3", "symbols": ["list_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "list_pattern", "symbols": ["list_pattern$macrocall$1", "listOpt_pattern", "list_pattern$macrocall$3"], "postprocess": ([, mk_κ,]) => mk_κ },
    { "name": "listOpt_pattern", "symbols": [], "postprocess": () => (κ) => Trie.constr(FiniteMap_1.singleton(Annotated_1.str(BaseTypes_1.Nil.name), κ)) },
    { "name": "listOpt_pattern", "symbols": ["list1_pattern"], "postprocess": id },
    { "name": "list1_pattern", "symbols": ["pattern", "listRestOpt_pattern"], "postprocess": ([mk_κ1, mk_κ2]) => (κ) => Trie.constr(FiniteMap_1.singleton(Annotated_1.str(BaseTypes_1.Cons.name), compose(mk_κ1, mk_κ2)(κ))) },
    { "name": "listRestOpt_pattern", "symbols": [], "postprocess": () => (κ) => Trie.constr(FiniteMap_1.singleton(Annotated_1.str(BaseTypes_1.Nil.name), κ)) },
    { "name": "listRestOpt_pattern$macrocall$2", "symbols": [{ "literal": "," }] },
    { "name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2"], "postprocess": id },
    { "name": "listRestOpt_pattern$macrocall$1", "symbols": ["listRestOpt_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "listRestOpt_pattern$macrocall$4", "symbols": [{ "literal": "..." }] },
    { "name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4"], "postprocess": id },
    { "name": "listRestOpt_pattern$macrocall$3", "symbols": ["listRestOpt_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$1", "listRestOpt_pattern$macrocall$3", "pattern"], "postprocess": ([, , mk_κ]) => mk_κ },
    { "name": "listRestOpt_pattern$macrocall$6", "symbols": [{ "literal": "," }] },
    { "name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6"], "postprocess": id },
    { "name": "listRestOpt_pattern$macrocall$5", "symbols": ["listRestOpt_pattern$macrocall$6", "_"], "postprocess": ([x,]) => x },
    { "name": "listRestOpt_pattern", "symbols": ["listRestOpt_pattern$macrocall$5", "list1_pattern"], "postprocess": ([, mk_κ]) => mk_κ },
    { "name": "constr_pattern", "symbols": ["ctr", "args_pattern"], "postprocess": ([c, mk_κs], _, reject) => {
            Core_1.assert(c instanceof Value_1.Str);
            if (DataType_1.arity(c) !== mk_κs.length) {
                return reject;
            }
            return (κ) => Trie.constr(FiniteMap_1.singleton(c, mk_κs.reduce(compose, (κ) => κ)(κ)));
        } },
    { "name": "args_pattern", "symbols": [], "postprocess": () => [] },
    { "name": "args_pattern$macrocall$2", "symbols": [{ "literal": "(" }] },
    { "name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2"], "postprocess": id },
    { "name": "args_pattern$macrocall$1", "symbols": ["args_pattern$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "args_pattern$ebnf$1", "symbols": [] },
    { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$2", "symbols": [{ "literal": "," }] },
    { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2"], "postprocess": id },
    { "name": "args_pattern$ebnf$1$subexpression$1$macrocall$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "args_pattern$ebnf$1$subexpression$1", "symbols": ["args_pattern$ebnf$1$subexpression$1$macrocall$1", "pattern"], "postprocess": ([, mk_κ]) => mk_κ },
    { "name": "args_pattern$ebnf$1", "symbols": ["args_pattern$ebnf$1", "args_pattern$ebnf$1$subexpression$1"], "postprocess": (d) => d[0].concat([d[1]]) },
    { "name": "args_pattern$macrocall$4", "symbols": [{ "literal": ")" }] },
    { "name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4"], "postprocess": id },
    { "name": "args_pattern$macrocall$3", "symbols": ["args_pattern$macrocall$4", "_"], "postprocess": ([x,]) => x },
    { "name": "args_pattern", "symbols": ["args_pattern$macrocall$1", "pattern", "args_pattern$ebnf$1", "args_pattern$macrocall$3"], "postprocess": ([, mk_κ, mk_κs,]) => [mk_κ, ...mk_κs] },
    { "name": "compareOp$macrocall$2", "symbols": [(lexer.has("compareOp") ? { type: "compareOp" } : compareOp)] },
    { "name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2"], "postprocess": id },
    { "name": "compareOp$macrocall$1", "symbols": ["compareOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "compareOp", "symbols": ["compareOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
    { "name": "exponentOp$macrocall$2", "symbols": [(lexer.has("exponentOp") ? { type: "exponentOp" } : exponentOp)] },
    { "name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2"], "postprocess": id },
    { "name": "exponentOp$macrocall$1", "symbols": ["exponentOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "exponentOp", "symbols": ["exponentOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
    { "name": "productOp$macrocall$2", "symbols": [(lexer.has("productOp") ? { type: "productOp" } : productOp)] },
    { "name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2"], "postprocess": id },
    { "name": "productOp$macrocall$1", "symbols": ["productOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "productOp", "symbols": ["productOp$macrocall$1"], "postprocess": ([[x]]) => x.value },
    { "name": "sumOp$macrocall$2", "symbols": [(lexer.has("sumOp") ? { type: "sumOp" } : sumOp)] },
    { "name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2"], "postprocess": id },
    { "name": "sumOp$macrocall$1", "symbols": ["sumOp$macrocall$2", "_"], "postprocess": ([x,]) => x },
    { "name": "sumOp", "symbols": ["sumOp$macrocall$1"], "postprocess": ([[x]]) => x.value }
];
exports.ParserStart = "rootExpr";


/***/ }),

/***/ "./src/Primitive.ts":
/*!**************************!*\
  !*** ./src/Primitive.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Annotated_1 = __webpack_require__(/*! ./Annotated */ "./src/Annotated.ts");
const BaseTypes_1 = __webpack_require__(/*! ./BaseTypes */ "./src/BaseTypes.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const Versioned_1 = __webpack_require__(/*! ./Versioned */ "./src/Versioned.ts");
// In the following two classes, we store the operation without generic type parameters, as fields can't
// have polymorphic type. Then access the operation via a method and reinstate the polymorphism via a cast.
class PrimOp extends Value_1.Value {
    constructor() {
        super(...arguments);
        this.name = Value_1._;
    }
}
exports.PrimOp = PrimOp;
class UnaryOp extends PrimOp {
    constructor() {
        super(...arguments);
        this.op = Value_1._;
    }
}
exports.UnaryOp = UnaryOp;
class BinaryOp extends PrimOp {
    constructor() {
        super(...arguments);
        this.op = Value_1._;
    }
}
exports.BinaryOp = BinaryOp;
const ceiling = (x) => Annotated_1.num(Math.ceil(x.val));
// Used to take arbitrary value as additional argument, but now primitives have primitive arguments.
const error = (message) => Core_1.assert(false, "LambdaCalc error:\n" + message.val);
const floor = (x) => Annotated_1.num(Math.floor(x.val));
const log = (x) => Annotated_1.num(Math.log(Core_1.as(x, Value_1.Num).val));
const numToStr = (x) => Annotated_1.str(x.val.toString());
const trace = (v) => { console.log(v); return Annotated_1.asAnnotated(v); };
// No longer support overloaded functions, since the pattern-matching semantics is non-trivial; might require typecase.
// If we want integer division, apparently ~~(x / y) will round in the right direction.
const div = (x, y) => Annotated_1.num(Core_1.as(x, Value_1.Num).val / Core_1.as(y, Value_1.Num).val);
const concat = (x, y) => Annotated_1.str(Core_1.as(x, Value_1.Str).val + Core_1.as(y, Value_1.Str).val);
const equalInt = (x, y) => Core_1.as(x, Value_1.Num).val === Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const equalStr = (x, y) => Core_1.as(x, Value_1.Str).val === Core_1.as(y, Value_1.Str).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const greaterEqInt = (x, y) => Core_1.as(x, Value_1.Num).val >= Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
// String comparison delegates to central implementation for consistency.
const greaterEqStr = (x, y) => Core_1.as(x, Value_1.Str).geq(Core_1.as(y, Value_1.Str)) ? BaseTypes_1.true_() : BaseTypes_1.false_();
const greaterInt = (x, y) => Core_1.as(x, Value_1.Num).val > Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const lessEqInt = (x, y) => Core_1.as(x, Value_1.Num).val <= Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const lessEqStr = (x, y) => Core_1.as(x, Value_1.Str).leq(Core_1.as(y, Value_1.Str)) ? BaseTypes_1.true_() : BaseTypes_1.false_();
const lessInt = (x, y) => Core_1.as(x, Value_1.Num).val < Core_1.as(y, Value_1.Num).val ? BaseTypes_1.true_() : BaseTypes_1.false_();
const minus = (x, y) => Annotated_1.num(Core_1.as(x, Value_1.Num).val - Core_1.as(y, Value_1.Num).val);
const plus = (x, y) => Annotated_1.num(Core_1.as(x, Value_1.Num).val + Core_1.as(y, Value_1.Num).val);
const pow = (x, y) => Annotated_1.num(Math.pow(Core_1.as(x, Value_1.Num).val, Core_1.as(y, Value_1.Num).val));
const times = (x, y) => Annotated_1.num(Core_1.as(x, Value_1.Num).val * Core_1.as(y, Value_1.Num).val);
// Convenience methods for building the maps. Export to allow other modules to provide operations.
function unary_(op) {
    return Annotated_1.annotatedAt(Versioned_1.ν(), UnaryOp, op.name, op);
}
exports.unary_ = unary_;
function binary_(op) {
    return Annotated_1.annotatedAt(Versioned_1.ν(), BinaryOp, op.name, op);
}
exports.binary_ = binary_;
// Primitives with identifiers as names are unary and first-class.
exports.unaryOps = new Map([
    [ceiling.name, unary_(ceiling)],
    [error.name, unary_(error)],
    [floor.name, unary_(floor)],
    [log.name, unary_(log)],
    [numToStr.name, unary_(numToStr)],
    [trace.name, unary_(trace)]
]);
exports.binaryOps = new Map([
    ["-", binary_(minus)],
    ["+", binary_(plus)],
    ["*", binary_(times)],
    ["**", binary_(pow)],
    ["/", binary_(div)],
    ["==", binary_(equalInt)],
    ["===", binary_(equalStr)],
    [">", binary_(greaterInt)],
    [">=", binary_(greaterEqInt)],
    [">==", binary_(greaterEqStr)],
    ["<", binary_(lessInt)],
    ["<=", binary_(lessEqInt)],
    ["<==", binary_(lessEqStr)],
    ["++", binary_(concat)]
]);


/***/ }),

/***/ "./src/Value.ts":
/*!**********************!*\
  !*** ./src/Value.ts ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
// Use to initialise fields for reflection, without requiring constructors.
exports._ = undefined;
// Value in the metalanguage. Nominal idiom breaks down here in requiring use of "any".
class Value {
    fieldValues() {
        return fields(this).map(k => this[k]);
    }
}
exports.Value = Value;
// Address or location of persistent object.
class Id extends Value {
}
exports.Id = Id;
class FunctionId extends Id {
    constructor() {
        super(...arguments);
        this.f = exports._;
    }
    get args() {
        return [];
    }
}
function functionId(f) {
    return make(FunctionId, f);
}
class ApplicationId extends Id {
    constructor() {
        super(...arguments);
        this.k = exports._;
        this.v = exports._;
    }
    get args() {
        const v̅ = this.k.args;
        v̅.push(this.v);
        return v̅;
    }
}
function applicationId(k, v) {
    return make(ApplicationId, k, v);
}
class TaggedId extends Id {
    constructor() {
        super(...arguments);
        this.k = exports._;
        this.tag = exports._;
    }
}
exports.TaggedId = TaggedId;
function taggedId(k, tag) {
    return make(TaggedId, k, tag);
}
exports.taggedId = taggedId;
function memoId(f, v̅) {
    const fʹ = functionId(f);
    let k = fʹ;
    for (let v of v̅) {
        k = applicationId(k, v);
    }
    return k;
}
exports.memoId = memoId;
class Num extends Value {
    constructor() {
        super(...arguments);
        this.val = exports._;
    }
    toString() {
        return this.val.toString();
    }
}
exports.Num = Num;
class Str extends Value {
    constructor() {
        super(...arguments);
        this.val = exports._;
    }
    toString() {
        return `"${this.val}"`;
    }
    leq(str) {
        return this.val.localeCompare(str.val) <= 0;
    }
    eq(str) {
        return this.val.localeCompare(str.val) === 0;
    }
    geq(str) {
        return this.val.localeCompare(str.val) >= 0;
    }
}
exports.Str = Str;
// Hash-consed constructors are invariant across worlds, whereas functions are not.
const __ctrMemo = new Map;
function lookupArg(f, m, v̅, n) {
    // for memoisation purposes, treat f's key as argument -1
    const k = n === -1 ? f.key : v̅[n];
    let v = m.get(k);
    if (v === undefined) {
        if (n === v̅.length - 1) {
            v = f.call(v̅);
            v = v; // TS confused; thinks v can be undefined here
        }
        else {
            v = new Map;
        }
        m.set(k, v);
    }
    return v;
}
class MemoCtr {
    constructor(C) {
        this.C = C;
    }
    get key() {
        return this.C;
    }
    call(v̅) {
        const o = new this.C;
        construct(o, v̅);
        Object.freeze(o);
        return o;
    }
}
function memoCall(memo, f, v̅) {
    let v = lookupArg(f, memo, v̅, -1);
    for (let n = 0; n < v̅.length; ++n) {
        // since there are more arguments, the last v was a (possibly nested) map
        v = lookupArg(f, v, v̅, n);
    }
    return v;
}
exports.memoCall = memoCall;
// Experimented with dictionary-based construction pattern; eliminates field order mismatch as a possible
// source of error, but the benefit is very small and doesn't really suit the memoisation pattern.
function make(C, ...v̅) {
    return memoCall(__ctrMemo, new MemoCtr(C), v̅);
}
exports.make = make;
// Depends heavily on (1) getOwnPropertyNames() returning fields in definition-order; and (2)
// constructor functions supplying arguments in the same order.
function construct(tgt, v̅) {
    const tgtʹ = tgt, f̅ = fields(tgt);
    Core_1.assert(f̅.length === v̅.length);
    let n = 0;
    f̅.forEach((f) => {
        tgtʹ[f] = v̅[n++];
    });
    return tgt;
}
exports.construct = construct;
// Exclude metadata according to our convention.
function isField(prop) {
    return !prop.startsWith("__");
}
exports.isField = isField;
function fields(v) {
    return Object.getOwnPropertyNames(v).filter(isField);
}
exports.fields = fields;
function metadataFields(v) {
    return Object.getOwnPropertyNames(v).filter(f => !isField(f) && f !== "__id");
}
exports.metadataFields = metadataFields;


/***/ }),

/***/ "./src/Versioned.ts":
/*!**************************!*\
  !*** ./src/Versioned.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./util/Core */ "./src/util/Core.ts");
const Value_1 = __webpack_require__(/*! ./Value */ "./src/Value.ts");
const __versioned = new Map;
// The (possibly already extant) versioned object uniquely identified by a memo-key.
function at(k, C, ...v̅) {
    let v = __versioned.get(k);
    if (v === undefined) {
        const v = new C;
        Object.defineProperty(v, "__id", {
            value: k,
            enumerable: false
        });
        const vʹ = v;
        __versioned.set(k, vʹ);
        return Value_1.construct(vʹ, v̅);
    }
    else if (v instanceof C) {
        return Value_1.construct(v, v̅); // hmm, TS thinks v is versioned here - why?
    }
    else {
        return reclassify(v, C);
    }
}
exports.at = at;
// Should emulate the post-state of "new C". Probably need to worry about how this works with inherited properties.
function reclassify(v, ctr) {
    return Core_1.notYetImplemented();
}
function copyAt(k, v) {
    const vʹ = at(k, Core_1.classOf(v), ...v.fieldValues());
    Value_1.metadataFields(v).forEach((prop) => {
        vʹ[prop] = v[prop];
    });
    return vʹ;
}
exports.copyAt = copyAt;
// A memo key which is sourced externally to the system. (The name "External" is already taken.)
class Extern extends Value_1.Id {
    constructor() {
        super(...arguments);
        this.id = Value_1._;
    }
}
exports.Extern = Extern;
function extern(id) {
    return Value_1.make(Extern, id);
}
// Fresh keys represent inputs to the system, e.g. addresses of syntax nodes provided by an external structure editor.
exports.ν = (() => {
    let count = 0;
    return () => {
        return extern(count++);
    };
})();


/***/ }),

/***/ "./src/util/Array.ts":
/*!***************************!*\
  !*** ./src/util/Array.ts ***!
  \***************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./Core */ "./src/util/Core.ts");
function flatten(x̅̅) {
    return [].concat.apply([], x̅̅);
}
exports.flatten = flatten;
function counts(x̅) {
    const counts = new Map;
    x̅.forEach(x => {
        if (counts.has(x)) {
            counts.set(x, Core_1.__nonNull(counts.get(x)) + 1);
        }
        else {
            counts.set(x, 1);
        }
    });
    return counts;
}
exports.counts = counts;
function zip(x̅, y̅) {
    return x̅.map((x, n) => [x, y̅[n]]);
}
exports.zip = zip;
function eq(x̅, y̅) {
    let n = x̅.length;
    if (n != y̅.length) {
        return false;
    }
    else {
        while (n--) {
            if (x̅[n] !== y̅[n])
                return false;
        }
        return true;
    }
}
exports.eq = eq;


/***/ }),

/***/ "./src/util/Core.ts":
/*!**************************!*\
  !*** ./src/util/Core.ts ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
function classOf(x) {
    return __nonNull(x).constructor; // weirdly failing on CircleCI without cast
}
exports.classOf = classOf;
function className(o) {
    return classOf(o).name;
}
exports.className = className;
function as(x, C) {
    if (__nonNull(x) instanceof C) {
        return x;
    }
    else {
        return assert(false, "[as] Expected " + C.name + ", got " + className(x));
    }
}
exports.as = as;
function asOpt(x, cls) {
    if (x === null || x === undefined) {
        return x;
    }
    else {
        return as(x, cls);
    }
}
exports.asOpt = asOpt;
function assert(b, msg, ...x̅) {
    if (!b) {
        if (x̅.length > 0) {
            console.warn("Assertion data:\n");
            x̅.forEach(x => console.warn(x));
        }
        throw new Error(msg || "Assertion failure");
    }
}
exports.assert = assert;
function absurd(msg, ...x̅) {
    assert(false, msg, ...x̅);
}
exports.absurd = absurd;
// User-level error.
function error(msg, ...x̅) {
    if (x̅.length > 0) {
        console.warn("Error data:\n");
        x̅.forEach(x => console.warn(x));
    }
    throw new Error("User error: " + msg);
}
exports.error = error;
function notYetImplemented() {
    throw new Error("Not yet implemented");
}
exports.notYetImplemented = notYetImplemented;
// Useful when a notionally abstract class needs to be concrete.
function abstractMethodError(this_) {
    return assert(false, "Abstract method in " + this_);
}
exports.abstractMethodError = abstractMethodError;
function __nonNull(x) {
    if (x !== null && x !== undefined) {
        return x;
    }
    else {
        return assert(false, "Unexpected null | undefined.");
    }
}
exports.__nonNull = __nonNull;
function debug(o) {
    return className(o) + "#" + o.__id;
}
exports.debug = debug;
function log(x, msg, transform = (it) => it) {
    const x_ = transform(x);
    if (msg) {
        console.log(msg(x_));
    }
    console.log(x_);
    return x;
}
exports.log = log;
function __check(x, predicate) {
    assert(predicate(x));
    return x;
}
exports.__check = __check;


/***/ }),

/***/ "./src/util/Lattice.ts":
/*!*****************************!*\
  !*** ./src/util/Lattice.ts ***!
  \*****************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
const Core_1 = __webpack_require__(/*! ./Core */ "./src/util/Core.ts");
// Actually a boolean lattice...
class LatticeImpl {
    join(...t̅) {
        return t̅.reduce((t1, t2) => this.join2(t1, t2));
    }
    meet(...t̅) {
        return t̅.reduce((t1, t2) => this.meet2(t1, t2));
    }
}
class BoolLattice extends LatticeImpl {
    constructor() {
        super(...arguments);
        this.bot = false;
        this.top = true;
    }
    // Important to assert that arguments are defined since undefined propagates in an unhelpful way.
    join2(b1, b2) {
        return Core_1.__nonNull(b1) || Core_1.__nonNull(b2);
    }
    meet2(b1, b2) {
        return Core_1.__nonNull(b1) && Core_1.__nonNull(b2);
    }
    negate(b) {
        return !b;
    }
}
exports.BoolLattice = BoolLattice;
exports.ann = new BoolLattice();


/***/ }),

/***/ "./src/util/Ord.ts":
/*!*************************!*\
  !*** ./src/util/Ord.ts ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

Object.defineProperty(exports, "__esModule", { value: true });
function eq(a, b) {
    return a.leq(b) && b.leq(a);
}
exports.eq = eq;


/***/ })

/******/ })["Lib2FFFqaad"];
//# sourceMappingURL=data:application/json;charset=utf-8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIndlYnBhY2s6Ly9GbG9vaWQvd2VicGFjay9ib290c3RyYXAiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vbm9kZV9tb2R1bGVzL21vby9tb28uanMiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vbm9kZV9tb2R1bGVzL25lYXJsZXkvbGliL25lYXJsZXkuanMiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vc3JjL0Fubm90YXRlZC50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvQmFzZVR5cGVzLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9EYXRhVHlwZS50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvRGF0YVZhbHVlLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9FbnYudHMiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vc3JjL0V2YWwudHMiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vc3JjL0V4cGxWYWx1ZS50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvRXhwci50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvRmluaXRlTWFwLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9HcmFwaGljcy50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvTGliLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9NYXRjaC50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvTW9kdWxlLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9QYXJzZS50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvUHJpbWl0aXZlLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy9WYWx1ZS50cyIsIndlYnBhY2s6Ly9GbG9vaWQvLi9zcmMvVmVyc2lvbmVkLnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy91dGlsL0FycmF5LnRzIiwid2VicGFjazovL0Zsb29pZC8uL3NyYy91dGlsL0NvcmUudHMiLCJ3ZWJwYWNrOi8vRmxvb2lkLy4vc3JjL3V0aWwvTGF0dGljZS50cyIsIndlYnBhY2s6Ly9G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QUMsRUFBRSxFQUFFLElBQUksRUFBRSxDQUFDLENBQUMsRUFBQztJQUMvSCxFQUFDLE1BQU0sRUFBRSxhQUFhLEVBQUUsU0FBUyxFQUFFLENBQUMsYUFBYSxFQUFFLFdBQVcsRUFBRSxTQUFTLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxDQUFDLFdBQUksQ0FBQyxTQUFTLENBQUMsYUFBQyxFQUFFLEVBQUUsRUFBRSxFQUFFLGVBQUcsQ0FBQyxFQUFFLENBQUMsRUFBRSxFQUFFLENBQUMsRUFBQztJQUNsSixFQUFDLE1BQU0sRUFBRSxhQUFhLEVBQUUsU0FBUyxFQUFFLENBQUMsU0FBUyxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNsRSxFQUFDLE1BQU0sRUFBRSxTQUFTLEVBQUUsU0FBUyxFQUFFLENBQUMsU0FBUyxFQUFFLE9BQU8sRUFBRSxhQUFhLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxDQUFDLFdBQUksQ0FBQyxTQUFTLENBQUMsYUFBQyxFQUFFLEVBQUUsRUFBRSxFQUFFLGVBQUcsQ0FBQyxFQUFFLENBQUMsRUFBRSxFQUFFLENBQUMsRUFBQztJQUMxSSxFQUFDLE1BQU0sRUFBRSxTQUFTLEVBQUUsU0FBUyxFQUFFLENBQUMsYUFBYSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNsRSxFQUFDLE1BQU0sRUFBRSxhQUFhLEVBQUUsU0FBUyxFQUFFLENBQUMsYUFBYSxFQUFFLFdBQVcsRUFBRSxjQUFjLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxDQUFDLFdBQUksQ0FBQyxTQUFTLENBQUMsYUFBQyxFQUFFLEVBQUUsRUFBRSxFQUFFLGVBQUcsQ0FBQyxFQUFFLENBQUMsRUFBRSxFQUFFLENBQUMsRUFBQztJQUN2SixFQUFDLE1BQU0sRUFBRSxhQUFhLEVBQUUsU0FBUyxFQUFFLENBQUMsY0FBYyxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUN2RSxFQUFDLE1BQU0sRUFBRSxjQUFjLEVBQUUsU0FBUyxFQUFFLENBQUMsY0FBYyxFQUFFLFlBQVksRUFBRSxVQUFVLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLEVBQUUsRUFBRSxFQUFFLEVBQUUsRUFBRSxDQUFDLEVBQUUsRUFBRSxDQUFDLFdBQUksQ0FBQyxTQUFTLENBQUMsYUFBQyxFQUFFLEVBQUUsRUFBRSxFQUFFLGVBQUcsQ0FBQyxFQUFFLENBQUMsRUFBRSxFQUFFLENBQUMsRUFBQztJQUN0SixFQUFDLE1BQU0sRUFBRSxjQUFjLEVBQUUsU0FBUyxFQUFFLENBQUMsVUFBVSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNwRSxFQUFDLE1BQU0sRUFBRSxVQUFVLEVBQUUsU0FBUyxFQUFFLENBQUMsWUFBWSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNsRSxFQUFDLE1BQU0sRUFBRSxVQUFVLEVBQUUsU0FBUyxFQUFFLENBQUMsVUFBVSxFQUFFLFlBQVksQ0FBQyxFQUFFLGFBQWEsRUFBRSxDQUFDLENBQUMsRUFBRSxFQUFFLEVBQUUsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxXQUFJLENBQUMsR0FBRyxDQUFDLGFBQUMsRUFBRSxFQUFFLEVBQUUsRUFBRSxFQUFFLENBQUMsRUFBQztJQUMvRyxFQUFDLE1BQU0sRUFBRSxZQUFZLEVBQUUsU0FBUyxFQUFFLENBQUMsVUFBVSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNsRSxFQUFDLE1BQU0sRUFBRSxZQUFZLEVBQUUsU0FBUyxFQUFFLENBQUMsUUFBUSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNoRSxFQUFDLE1BQU0sRUFBRSxZQUFZLEVBQUUsU0FBUyxFQUFFLENBQUMsUUFBUSxDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNo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QUFDLE1BQU0sRUFBRSxTQUFTLEVBQUUsU0FBUyxFQUFFLENBQUMscUJBQXFCLEVBQUUsT0FBTyxFQUFFLGdCQUFnQixFQUFFLHFCQUFxQixDQUFDLEVBQUUsYUFBYSxFQUFFLENBQUMsQ0FBQyxFQUFFLENBQUMsRUFBRSxFQUFFLEVBQUUsRUFBRSxFQUFFLENBQUMsQ0FBQyxDQUFDLEVBQUUsR0FBRyxFQUFFLENBQUMsQ0FBQyxNQUFNLENBQUMsSUFBSSxDQUFDLElBQUksQ0FBQyxJQUFJLENBQUMsRUFBQztJQUMzSyxFQUFDLE1BQU0sRUFBRSxtQkFBbUIsRUFBRSxTQUFTLEVBQUUsQ0FBQyxFQUFDLFNBQVMsRUFBQyxHQUFHLEVBQUMsQ0FBQyxFQUFDO0lBQzNELEVBQUMsTUFBTSxFQUFFLG1CQUFtQixFQUFFLFNBQVMsRUFBRSxDQUFDLG1CQUFtQixDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNsRixFQUFDLE1BQU0sRUFBRSxtQkFBbUIsRUFBRSxTQUFTLEVBQUUsQ0FBQyxtQkFBbUIsRUFBRSxHQUFHLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLENBQUMsRUFBRyxFQUFFLEVBQUUsQ0FBQyxDQUFDLEVBQUM7SUFDakcsRUFBQyxNQUFNLEVBQUUsT0FBTyxFQUFFLFNBQVMsRUFBRSxDQUFDLFNBQVMsRUFBRSxtQkFBbUIsRUFBRSxNQUFNLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLElBQUksRUFBRSxFQUFFLENBQUMsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDLEVBQUM7SUFDL0csRUFBQyxNQUFNLEVBQUUsT0FBTyxFQUFFLFNBQVMsRUFBRSxDQUFDLFNBQVMsRUFBRSxTQUFTLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLEtBQUssRUFBRSxDQUFDLENBQUMsRUFBRSxFQUFFLENBQUMsS0FBSyxDQUFDLFdBQUksQ0FBQyxHQUFHLENBQUMsYUFBQyxFQUFFLEVBQUUsQ0FBQyxDQUFDLENBQUMsRUFBQztJQUM1RyxFQUFDLE1B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QyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsTUFBTSxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsRUFBQztJQUNySixFQUFDLE1BQU0sRUFBRSwwQkFBMEIsRUFBRSxTQUFTLEVBQUUsQ0FBQyxFQUFDLFNBQVMsRUFBQyxHQUFHLEVBQUMsQ0FBQyxFQUFDO0lBQ2xFLEVBQUMsTUFBTSxFQUFFLDBCQUEwQixFQUFFLFNBQVMsRUFBRSxDQUFDLDBCQUEwQixDQUFDLEVBQUUsYUFBYSxFQUFFLEVBQUUsRUFBQztJQUNoRyxFQUFDLE1BQU0sRUFBRSwwQkFBMEIsRUFBRSxTQUFTLEVBQUUsQ0FBQywwQkFBMEIsRUFBRSxHQUFHLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLENBQUMsRUFBRyxFQUFFLEVBQUUsQ0FBQyxDQUFDLEVBQUM7SUFDL0csRUFBQyxNQUFNLEVBQUUsY0FBYyxFQUFFLFNBQVMsRUFBRSxDQUFDLDBCQUEwQixFQUFFLFNBQVMsRUFBRSxxQkFBcUIsRUFBRSwwQkFBMEIsQ0FBQyxFQUFFLGFBQWEsRUFBRSxDQUFDLENBQUMsRUFBRSxJQUFJLEVBQUUsS0FBSyxFQUFFLEVBQUUsRUFBRSxDQUFDLENBQUMsSUFBSSxFQUFFLEdBQUcsS0FBSyxDQUFDLEVBQUM7SUFDdEwsRUFBQyxNQUFNLEVBQUUsdUJBQXVCLEVBQUUsU0FBUyxFQUFFLENBQUMsQ0FBQyxLQUFLLENBQUMsR0FBRyxDQUFDLFdBQVcsQ0FBQyxDQUFDLENBQUMsQ0FBQyxFQUFDLElBQUksRUFBRSxXQUFXLEVBQUMsQ0FBQyxDQUFDLENBQUMsU0FBUyxDQUFDLENBQUMsRUFBQztJQUMxRyxFQUFDLE1BQU0sRUFBRSx1QkFBdUIsRUFBRSxTQUFTLEVBQUUsQ0FBQyx1QkFBdUIsQ0FBQyxFQUFFLGFBQWEsRUFBRSxFQUFFLEVBQUM7SUFDMUYsRUFBQyxNQUFNLEVBQUUsdUJBQXVCLEVBQUUsU0FBUyxFQUFFLENBQUMsdUJBQXVCLEVBQUUsR0FBRyxDQUFDLEVBQUUsYUFBYSxFQUFFLENBQUMsQ0FBQyxDQUFDLEVBQUcsRUFBRSxFQUFFLENBQUMsQ0FBQyxFQUFDO0lBQ3pHLEVBQUMsTUFBTSxFQUFFLFdBQVcsRUFBRSxTQUFTLEVBQUUsQ0FBQyx1QkFBdUIsQ0FBQyxFQUFFLGFBQWEsRUFBRSxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsQ0FBQyxFQUFFLEVBQUUsQ0FBQyxDQUFDLENBQUMsS0FBSyxFQUFDO0lBQzlGLEVBQUMsTUFBTSxFQUFFLHdCQUF3QixFQUFFLFNBQVMsRUFBRSxDQUFDLENBQUMsS0FBSyxDQUFDLEdBQUcsQ0FBQyxZQUFZLENBQUMsQ0FBQyxDQUFDLENBQUMsRUFBQyxJQUFJLEVBQUUsWUFBWSxFQUFDLENBQUMsQ0FBQyxDQUFDLFVBQVUsQ0FBQyxDQUFDLEVBQUM7SUFDOUcsRUFBQyxNQUFNLEVBQUUsd0JBQXdCLEVBQUUsU0FBUyxFQUFFLENBQUMsd0JBQXdCLENBQUMsRUFBRSxhQUFhLEVBQUUsRUFBRSxFQUFDO0lBQzVGLEVBQUMsTUFBTSxFQUFFLHdCQUF3QixFQUFFLFNBQVMsRUFBRSxDQUFDLHdCQUF3QixFQUFFLEdBQUcsQ0FBQyxFQUFFLGFBQWEsRUFBRSxDQUFDLENBQUMsQ0FBQyxFQUFHLEVBQUUsRUFBRSxDQUFDLENBQUMsRUFBQztJQUMzRyxFQUFDLE1BQU0sRUFBRSxZQUFZLEVBQUUsU0FBUyxFQUFFLENBQUMsd0JBQXdCLENBQUMsRUFBRSxhQUFhLEVBQUUsQ0FBQyxDQUFDLENBQUMsQ0FBQyxDQUFDLENBQUMsRUFBRSxFQUFFLENBQUMsQ0FBQyxDQUFDLEtBQUssRUFBQztJQUNoRyxFQUFDLE1BQU0sRUFBRSx1QkFBdUIsRUFBRSxTQUFTLEVBQUUsQ0FBQyxDQUFDLEtBQUssQ0FBQyxHQUFHLENBQUMsV0FBVyxDQUFDLENBQUMsQ0FBQyxDQUFDLEVBQUMsSUFBSSxFQUFFLFdBQVcsRUFBQyxDQUFDLENBQUMsQ0FBQyxTQUFTLENBQUMsQ0FBQyxFQUFDO0lBQzFHLEVBQUMsTUFBTSxFQUFFLHVCQUF1QixFQUFFLFNBQVMsRUFBRSxDQUFDLHVCQUF1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Q0FBQyxFQUFFLEVBQUUsRUFBRSxDQUFDLENBQUM7SUFDbkQsQ0FBQztDQUtIO0FBRUQsTUFBYSxXQUFZLFNBQVEsV0FBb0I7SUFBckQ7O1FBQ0csUUFBRyxHQUFHLEtBQUs7UUFDWCxRQUFHLEdBQUcsSUFBSTtJQWNiLENBQUM7SUFaRSxpR0FBaUc7SUFDakcsS0FBSyxDQUFFLEVBQVcsRUFBRSxFQUFXO1FBQzVCLE9BQU8sZ0JBQVMsQ0FBQyxFQUFFLENBQUMsSUFBSSxnQkFBUyxDQUFDLEVBQUUsQ0FBQztJQUN4QyxDQUFDO0lBRUQsS0FBSyxDQUFFLEVBQVcsRUFBRSxFQUFXO1FBQzVCLE9BQU8sZ0JBQVMsQ0FBQyxFQUFFLENBQUMsSUFBSSxnQkFBUyxDQUFDLEVBQUUsQ0FBQztJQUN4QyxDQUFDO0lBRUQsTUFBTSxDQUFFLENBQVU7UUFDZixPQUFPLENBQUMsQ0FBQztJQUNaLENBQUM7Q0FDSDtBQWhCRCxrQ0FnQkM7QUFFWSxXQUFHLEdBQStCLElBQUksV0FBVyxFQUFFOzs7Ozs7Ozs7Ozs7Ozs7QUNoQ2hFLFNBQWdCLEVBQUUsQ0FBbUIsQ0FBSSxFQUFFLENBQUk7SUFDNUMsT0FBTyxDQUFDLENBQUMsR0FBRyxDQUFDLENBQUMsQ0FBQyxJQUFJLENBQUMsQ0FBQyxHQUFHLENBQUMsQ0FBQyxDQUFDO0FBQzlCLENBQUM7QUFGRCxnQkFFQyIsImZpbGUiOiJsaWIuYnVuZGxlLmpzIiwic291cmNlc0NvbnRlbnQiOlsiIFx0Ly8gVGhlIG1vZHVsZSBjYWNoZVxuIFx0dmFyIGluc3RhbGxlZE1vZHVsZXMgPSB7fTtcblxuIFx0Ly8gVGhlIHJlcXVpcmUgZnVuY3Rpb25cbiBcdGZ1bmN0aW9uIF9fd2VicGFja19yZXF1aXJlX18obW9kdWxlSWQpIHtcblxuIFx0XHQvLyBDaGVjayBpZiBtb2R1bGUgaXMgaW4gY2FjaGVcbiBcdFx0aWYoaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0pIHtcbiBcdFx0XHRyZXR1cm4gaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0uZXhwb3J0cztcbiBcdFx0fVxuIFx0XHQvLyBDcmVhdGUgYSBuZXcgbW9kdWxlIChhbmQgcHV0IGl0IGludG8gdGhlIGNhY2hlKVxuIFx0XHR2YXIgbW9kdWxlID0gaW5zdGFsbGVkTW9kdWxlc1ttb2R1bGVJZF0gPSB7XG4gXHRcdFx0aTogbW9kdWxlSWQsXG4gXHRcdFx0bDogZmFsc2UsXG4gXHRcdFx0ZXhwb3J0czoge31cbiBcdFx0fTtcblxuIFx0XHQvLyBFeGVjdXRlIHRoZSBtb2R1bGUgZnVuY3Rpb25cbiBcdFx0bW9kdWxlc1ttb2R1bGVJZF0uY2FsbChtb2R1bGUuZXhwb3J0cywgbW9kdWxlLCBtb2R1bGUuZXhwb3J0cywgX193ZWJwYWNrX3JlcXVpcmVfXyk7XG5cbiBcdFx0Ly8gRmxhZyB0aGUgbW9kdWxlIGFzIGxvYWRlZFxuIFx0XHRtb2R1bGUubCA9IHRydWU7XG5cbiBcdFx0Ly8gUmV0dXJuIHRoZSBleHBvcnRzIG9mIHRoZSBtb2R1bGVcbiBcdFx0cmV0dXJuIG1vZHVsZS5leHBvcnRzO1xuIFx0fVxuXG5cbiBcdC8vIGV4cG9zZSB0aGUgbW9kdWxlcyBvYmplY3QgKF9fd2VicGFja19tb2R1bGVzX18pXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLm0gPSBtb2R1bGVzO1xuXG4gXHQvLyBleHBvc2UgdGhlIG1vZHVsZSBjYWNoZVxuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5jID0gaW5zdGFsbGVkTW9kdWxlcztcblxuIFx0Ly8gZGVmaW5lIGdldHRlciBmdW5jdGlvbiBmb3IgaGFybW9ueSBleHBvcnRzXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLmQgPSBmdW5jdGlvbihleHBvcnRzLCBuYW1lLCBnZXR0ZXIpIHtcbiBcdFx0aWYoIV9fd2VicGFja19yZXF1aXJlX18ubyhleHBvcnRzLCBuYW1lKSkge1xuIFx0XHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCBuYW1lLCB7IGVudW1lcmFibGU6IHRydWUsIGdldDogZ2V0dGVyIH0pO1xuIFx0XHR9XG4gXHR9O1xuXG4gXHQvLyBkZWZpbmUgX19lc01vZHVsZSBvbiBleHBvcnRzXG4gXHRfX3dlYnBhY2tfcmVxdWlyZV9fLnIgPSBmdW5jdGlvbihleHBvcnRzKSB7XG4gXHRcdGlmKHR5cGVvZiBTeW1ib2wgIT09ICd1bmRlZmluZWQnICYmIFN5bWJvbC50b1N0cmluZ1RhZykge1xuIFx0XHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCBTeW1ib2wudG9TdHJpbmdUYWcsIHsgdmFsdWU6ICdNb2R1bGUnIH0pO1xuIFx0XHR9XG4gXHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCAnX19lc01vZHVsZScsIHsgdmFsdWU6IHRydWUgfSk7XG4gXHR9O1xuXG4gXHQvLyBjcmVhdGUgYSBmYWtlIG5hbWVzcGFjZSBvYmplY3RcbiBcdC8vIG1vZGUgJiAxOiB2YWx1ZSBpcyBhIG1vZHVsZSBpZCwgcmVxdWlyZSBpdFxuIFx0Ly8gbW9kZSAmIDI6IG1lcmdlIGFsbCBwcm9wZXJ0aWVzIG9mIHZhbHVlIGludG8gdGhlIG5zXG4gXHQvLyBtb2RlICYgNDogcmV0dXJuIHZhbHVlIHdoZW4gYWxyZWFkeSBucyBvYmplY3RcbiBcdC8vIG1vZGUgJiA4fDE6IGJlaGF2ZSBsaWtlIHJlcXVpcmVcbiBcdF9fd2VicGFja19yZXF1aXJlX18udCA9IGZ1bmN0aW9uKHZhbHVlLCBtb2RlKSB7XG4gXHRcdGlmKG1vZGUgJiAxKSB2YWx1ZSA9IF9fd2VicGFja19yZXF1aXJlX18odmFsdWUpO1xuIFx0XHRpZihtb2RlICYgOCkgcmV0dXJuIHZhbHVlO1xuIFx0XHRpZigobW9kZSAmIDQpICYmIHR5cGVvZiB2YWx1ZSA9PT0gJ29iamVjdCcgJiYgdmFsdWUgJiYgdmFsdWUuX19lc01vZHVsZSkgcmV0dXJuIHZhbHVlO1xuIFx0XHR2YXIgbnMgPSBPYmplY3QuY3JlYXRlKG51bGwpO1xuIFx0XHRfX3dlYnBhY2tfcmVxdWlyZV9fLnIobnMpO1xuIFx0XHRPYmplY3QuZGVmaW5lUHJvcGVydHkobnMsICdkZWZhdWx0JywgeyBlbnVtZXJhYmxlOiB0cnVlLCB2YWx1ZTogdmFsdWUgfSk7XG4gXHRcdGlmKG1vZGUgJiAyICYmIHR5cGVvZiB2YWx1ZSAhPSAnc3RyaW5nJykgZm9yKHZhciBrZXkgaW4gdmFsdWUpIF9fd2VicGFja19yZXF1aXJlX18uZChucywga2V5LCBmdW5jdGlvbihrZXkpIHsgcmV0dXJuIHZhbHVlW2tleV07IH0uYmluZChudWxsLCBrZXkpKTtcbiBcdFx0cmV0dXJuIG5zO1xuIFx0fTtcblxuIFx0Ly8gZ2V0RGVmYXVsdEV4cG9ydCBmdW5jdGlvbiBmb3IgY29tcGF0aWJpbGl0eSB3aXRoIG5vbi1oYXJtb255IG1vZHVsZXNcbiBcdF9fd2VicGFja19yZXF1aXJlX18ubiA9IGZ1bmN0aW9uKG1vZHVsZSkge1xuIFx0XHR2YXIgZ2V0dGVyID0gbW9kdWxlICYmIG1vZHVsZS5fX2VzTW9kdWxlID9cbiBcdFx0XHRmdW5jdGlvbiBnZXREZWZhdWx0KCkgeyByZXR1cm4gbW9kdWxlWydkZWZhdWx0J107IH0gOlxuIFx0XHRcdGZ1bmN0aW9uIGdldE1vZHVsZUV4cG9ydHMoKSB7IHJldHVybiBtb2R1bGU7IH07XG4gXHRcdF9fd2VicGFja19yZXF1aXJlX18uZChnZXR0ZXIsICdhJywgZ2V0dGVyKTtcbiBcdFx0cmV0dXJuIGdldHRlcjtcbiBcdH07XG5cbiBcdC8vIE9iamVjdC5wcm90b3R5cGUuaGFzT3duUHJvcGVydHkuY2FsbFxuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5vID0gZnVuY3Rpb24ob2JqZWN0LCBwcm9wZXJ0eSkgeyByZXR1cm4gT2JqZWN0LnByb3RvdHlwZS5oYXNPd25Qcm9wZXJ0eS5jYWxsKG9iamVjdCwgcHJvcGVydHkpOyB9O1xuXG4gXHQvLyBfX3dlYnBhY2tfcHVibGljX3BhdGhfX1xuIFx0X193ZWJwYWNrX3JlcXVpcmVfXy5wID0gXCJcIjtcblxuXG4gXHQvLyBMb2FkIGVudHJ5IG1vZHVsZSBhbmQgcmV0dXJuIGV4cG9ydHNcbiBcdHJldHVybiBfX3dlYnBhY2tfcmVxdWlyZV9fKF9fd2VicGFja19yZXF1aXJlX18ucyA9IFwiLi9zcmMvTGliLnRzXCIpO1xuIiwiKGZ1bmN0aW9uKHJvb3QsIGZhY3RvcnkpIHtcbiAgaWYgKHR5cGVvZiBkZWZpbmUgPT09ICdmdW5jdGlvbicgJiYgZGVmaW5lLmFtZCkge1xuICAgIGRlZmluZShbXSwgZmFjdG9yeSkgLyogZ2xvYmFsIGRlZmluZSAqL1xuICB9IGVsc2UgaWYgKHR5cGVvZiBtb2R1bGUgPT09ICdvYmplY3QnICYmIG1vZHVsZS5leHBvcnRzKSB7XG4gICAgbW9kdWxlLmV4cG9ydHMgPSBmYWN0b3J5KClcbiAgfSBlbHNlIHtcbiAgICByb290Lm1vbyA9IGZhY3RvcnkoKVxuICB9XG59KHRoaXMsIGZ1bmN0aW9uKCkge1xuICAndXNlIHN0cmljdCc7XG5cbiAgdmFyIGhhc093blByb3BlcnR5ID0gT2JqZWN0LnByb3RvdHlwZS5oYXNPd25Qcm9wZXJ0eVxuICB2YXIgdG9TdHJpbmcgPSBPYmplY3QucHJvdG90eXBlLnRvU3RyaW5nXG4gIHZhciBoYXNTdGlja3kgPSB0eXBlb2YgbmV3IFJlZ0V4cCgpLnN0aWNreSA9PT0gJ2Jvb2xlYW4nXG5cbiAgLyoqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKi9cblxuICBmdW5jdGlvbiBpc1JlZ0V4cChvKSB7IHJldHVybiBvICYmIHRvU3RyaW5nLmNhbGwobykgPT09ICdbb2JqZWN0IFJlZ0V4cF0nIH1cbiAgZnVuY3Rpb24gaXNPYmplY3QobykgeyByZXR1cm4gbyAmJiB0eXBlb2YgbyA9PT0gJ29iamVjdCcgJiYgIWlzUmVnRXhwKG8pICYmICFBcnJheS5pc0FycmF5KG8pIH1cblxuICBmdW5jdGlvbiByZUVzY2FwZShzKSB7XG4gICAgcmV0dXJuIHMucmVwbGFjZSgvWy1cXC9cXFxcXiQqKz8uKCl8W1xcXXt9XS9nLCAnXFxcXCQmJylcbiAgfVxuICBmdW5jdGlvbiByZUdyb3VwcyhzKSB7XG4gICAgdmFyIHJlID0gbmV3IFJlZ0V4cCgnfCcgKyBzKVxuICAgIHJldHVybiByZS5leGVjKCcnKS5sZW5ndGggLSAxXG4gIH1cbiAgZnVuY3Rpb24gcmVDYXB0dXJlKHMpIHtcbiAgICByZXR1cm4gJygnICsgcyArICcpJ1xuICB9XG4gIGZ1bmN0aW9uIHJlVW5pb24ocmVnZXhwcykge1xuICAgIGlmICghcmVnZXhwcy5sZW5ndGgpIHJldHVybiAnKD8hKSdcbiAgICB2YXIgc291cmNlID0gIHJlZ2V4cHMubWFwKGZ1bmN0aW9uKHMpIHtcbiAgICAgIHJldHVybiBcIig/OlwiICsgcyArIFwiKVwiXG4gICAgfSkuam9pbignfCcpXG4gICAgcmV0dXJuIFwiKD86XCIgKyBzb3VyY2UgKyBcIilcIlxuICB9XG5cbiAgZnVuY3Rpb24gcmVnZXhwT3JMaXRlcmFsKG9iaikge1xuICAgIGlmICh0eXBlb2Ygb2JqID09PSAnc3RyaW5nJykge1xuICAgICAgcmV0dXJuICcoPzonICsgcmVFc2NhcGUob2JqKSArICcpJ1xuXG4gICAgfSBlbHNlIGlmIChpc1JlZ0V4cChvYmopKSB7XG4gICAgICAvLyBUT0RPOiBjb25zaWRlciAvdSBzdXBwb3J0XG4gICAgICBpZiAob2JqLmlnbm9yZUNhc2UpIHRocm93IG5ldyBFcnJvcignUmVnRXhwIC9pIGZsYWcgbm90IGFsbG93ZWQnKVxuICAgICAgaWYgKG9iai5nbG9iYWwpIHRocm93IG5ldyBFcnJvcignUmVnRXhwIC9nIGZsYWcgaXMgaW1wbGllZCcpXG4gICAgICBpZiAob2JqLnN0aWNreSkgdGhyb3cgbmV3IEVycm9yKCdSZWdFeHAgL3kgZmxhZyBpcyBpbXBsaWVkJylcbiAgICAgIGlmIChvYmoubXVsdGlsaW5lKSB0aHJvdyBuZXcgRXJyb3IoJ1JlZ0V4cCAvbSBmbGFnIGlzIGltcGxpZWQnKVxuICAgICAgaWYgKG9iai51bmljb2RlKSB0aHJvdyBuZXcgRXJyb3IoJ1JlZ0V4cCAvdSBmbGFnIGlzIG5vdCBhbGxvd2VkJylcbiAgICAgIHJldHVybiBvYmouc291cmNlXG5cbiAgICB9IGVsc2Uge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKCdOb3QgYSBwYXR0ZXJuOiAnICsgb2JqKVxuICAgIH1cbiAgfVxuXG4gIGZ1bmN0aW9uIG9iamVjdFRvUnVsZXMob2JqZWN0KSB7XG4gICAgdmFyIGtleXMgPSBPYmplY3QuZ2V0T3duUHJvcGVydHlOYW1lcyhvYmplY3QpXG4gICAgdmFyIHJlc3VsdCA9IFtdXG4gICAgZm9yICh2YXIgaSA9IDA7IGkgPCBrZXlzLmxlbmd0aDsgaSsrKSB7XG4gICAgICB2YXIga2V5ID0ga2V5c1tpXVxuICAgICAgdmFyIHRoaW5nID0gb2JqZWN0W2tleV1cbiAgICAgIHZhciBydWxlcyA9IFtdLmNvbmNhdCh0aGluZylcbiAgICAgIGlmIChrZXkgPT09ICdpbmNsdWRlJykge1xuICAgICAgICBmb3IgKHZhciBqID0gMDsgaiA8IHJ1bGVzLmxlbmd0aDsgaisrKSB7XG4gICAgICAgICAgcmVzdWx0LnB1c2goe2luY2x1ZGU6IHJ1bGVzW2pdfSlcbiAgICAgICAgfVxuICAgICAgICBjb250aW51ZVxuICAgICAgfVxuICAgICAgdmFyIG1hdGNoID0gW11cbiAgICAgIHJ1bGVzLmZvckVhY2goZnVuY3Rpb24ocnVsZSkge1xuICAgICAgICBpZiAoaXNPYmplY3QocnVsZSkpIHtcbiAgICAgICAgICBpZiAobWF0Y2gubGVuZ3RoKSByZXN1bHQucHVzaChydWxlT3B0aW9ucyhrZXksIG1hdGNoKSlcbiAgICAgICAgICByZXN1bHQucHVzaChydWxlT3B0aW9ucyhrZXksIHJ1bGUpKVxuICAgICAgICAgIG1hdGNoID0gW11cbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICBtYXRjaC5wdXNoKHJ1bGUpXG4gICAgICAgIH1cbiAgICAgIH0pXG4gICAgICBpZiAobWF0Y2gubGVuZ3RoKSByZXN1bHQucHVzaChydWxlT3B0aW9ucyhrZXksIG1hdGNoKSlcbiAgICB9XG4gICAgcmV0dXJuIHJlc3VsdFxuICB9XG5cbiAgZnVuY3Rpb24gYXJyYXlUb1J1bGVzKGFycmF5KSB7XG4gICAgdmFyIHJlc3VsdCA9IFtdXG4gICAgZm9yICh2YXIgaSA9IDA7IGkgPCBhcnJheS5sZW5ndGg7IGkrKykge1xuICAgICAgdmFyIG9iaiA9IGFycmF5W2ldXG4gICAgICBpZiAob2JqLmluY2x1ZGUpIHtcbiAgICAgICAgdmFyIGluY2x1ZGUgPSBbXS5jb25jYXQob2JqLmluY2x1ZGUpXG4gICAgICAgIGZvciAodmFyIGogPSAwOyBqIDwgaW5jbHVkZS5sZW5ndGg7IGorKykge1xuICAgICAgICAgIHJlc3VsdC5wdXNoKHtpbmNsdWRlOiBpbmNsdWRlW2pdfSlcbiAgICAgICAgfVxuICAgICAgICBjb250aW51ZVxuICAgICAgfVxuICAgICAgaWYgKCFvYmoudHlwZSkge1xuICAgICAgICB0aHJvdyBuZXcgRXJyb3IoJ1J1bGUgaGFzIG5vIHR5cGU6ICcgKyBKU09OLnN0cmluZ2lmeShvYmopKVxuICAgICAgfVxuICAgICAgcmVzdWx0LnB1c2gocnVsZU9wdGlvbnMob2JqLnR5cGUsIG9iaikpXG4gICAgfVxuICAgIHJldHVybiByZXN1bHRcbiAgfVxuXG4gIGZ1bmN0aW9uIHJ1bGVPcHRpb25zKHR5cGUsIG9iaikge1xuICAgIGlmICghaXNPYmplY3Qob2JqKSkge1xuICAgICAgb2JqID0geyBtYXRjaDogb2JqIH1cbiAgICB9XG4gICAgaWYgKG9iai5pbmNsdWRlKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoJ01hdGNoaW5nIHJ1bGVzIGNhbm5vdCBhbHNvIGluY2x1ZGUgc3RhdGVzJylcbiAgICB9XG5cbiAgICAvLyBuYi4gZXJyb3IgYW5kIGZhbGxiYWNrIGltcGx5IGxpbmVCcmVha3NcbiAgICB2YXIgb3B0aW9ucyA9IHtcbiAgICAgIGRlZmF1bHRUeXBlOiB0eXBlLFxuICAgICAgbGluZUJyZWFrczogISFvYmouZXJyb3IgfHwgISFvYmouZmFsbGJhY2ssXG4gICAgICBwb3A6IGZhbHNlLFxuICAgICAgbmV4dDogbnVsbCxcbiAgICAgIHB1c2g6IG51bGwsXG4gICAgICBlcnJvcjogZmFsc2UsXG4gICAgICBmYWxsYmFjazogZmFsc2UsXG4gICAgICB2YWx1ZTogbnVsbCxcbiAgICAgIHR5cGU6IG51bGwsXG4gICAgICBzaG91bGRUaHJvdzogZmFsc2UsXG4gICAgfVxuXG4gICAgLy8gQXZvaWQgT2JqZWN0LmFzc2lnbigpLCBzbyB3ZSBzdXBwb3J0IElFOStcbiAgICBmb3IgKHZhciBrZXkgaW4gb2JqKSB7XG4gICAgICBpZiAoaGFzT3duUHJvcGVydHkuY2FsbChvYmosIGtleSkpIHtcbiAgICAgICAgb3B0aW9uc1trZXldID0gb2JqW2tleV1cbiAgICAgIH1cbiAgICB9XG5cbiAgICAvLyB0eXBlIHRyYW5zZm9ybSBjYW5ub3QgYmUgYSBzdHJpbmdcbiAgICBpZiAodHlwZW9mIG9wdGlvbnMudHlwZSA9PT0gJ3N0cmluZycgJiYgdHlwZSAhPT0gb3B0aW9ucy50eXBlKSB7XG4gICAgICB0aHJvdyBuZXcgRXJyb3IoXCJUeXBlIHRyYW5zZm9ybSBjYW5ub3QgYmUgYSBzdHJpbmcgKHR5cGUgJ1wiICsgb3B0aW9ucy50eXBlICsgXCInIGZvciB0b2tlbiAnXCIgKyB0eXBlICsgXCInKVwiKVxuICAgIH1cblxuICAgIC8vIGNvbnZlcnQgdG8gYXJyYXlcbiAgICB2YXIgbWF0Y2ggPSBvcHRpb25zLm1hdGNoXG4gICAgb3B0aW9ucy5tYXRjaCA9IEFycmF5LmlzQXJyYXkobWF0Y2gpID8gbWF0Y2ggOiBtYXRjaCA/IFttYXRjaF0gOiBbXVxuICAgIG9wdGlvbnMubWF0Y2guc29ydChmdW5jdGlvbihhLCBiKSB7XG4gICAgICByZXR1cm4gaXNSZWdFeHAoYSkgJiYgaXNSZWdFeHAoYikgPyAwXG4gICAgICAgICAgIDogaXNSZWdFeHAoYikgPyAtMSA6IGlzUmVnRXhwKGEpID8gKzEgOiBiLmxlbmd0aCAtIGEubGVuZ3RoXG4gICAgfSlcbiAgICByZXR1cm4gb3B0aW9uc1xuICB9XG5cbiAgZnVuY3Rpb24gdG9SdWxlcyhzcGVjKSB7XG4gICAgcmV0dXJuIEFycmF5LmlzQXJyYXkoc3BlYykgPyBhcnJheVRvUnVsZXMoc3BlYykgOiBvYmplY3RUb1J1bGVzKHNwZWMpXG4gIH1cblxuICB2YXIgZGVmYXVsdEVycm9yUnVsZSA9IHJ1bGVPcHRpb25zKCdlcnJvcicsIHtsaW5lQnJlYWtzOiB0cnVlLCBzaG91bGRUaHJvdzogdHJ1ZX0pXG4gIGZ1bmN0aW9uIGNvbXBpbGVSdWxlcyhydWxlcywgaGFzU3RhdGVzKSB7XG4gICAgdmFyIGVycm9yUnVsZSA9IG51bGxcbiAgICB2YXIgZmFzdCA9IE9iamVjdC5jcmVhdGUobnVsbClcbiAgICB2YXIgZmFzdEFsbG93ZWQgPSB0cnVlXG4gICAgdmFyIGdyb3VwcyA9IFtdXG4gICAgdmFyIHBhcnRzID0gW11cblxuICAgIC8vIElmIHRoZXJlIGlzIGEgZmFsbGJhY2sgcnVsZSwgdGhlbiBkaXNhYmxlIGZhc3QgbWF0Y2hpbmdcbiAgICBmb3IgKHZhciBpID0gMDsgaSA8IHJ1bGVzLmxlbmd0aDsgaSsrKSB7XG4gICAgICBpZiAocnVsZXNbaV0uZmFsbGJhY2spIHtcbiAgICAgICAgZmFzdEFsbG93ZWQgPSBmYWxzZVxuICAgICAgfVxuICAgIH1cblxuICAgIGZvciAodmFyIGkgPSAwOyBpIDwgcnVsZXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHZhciBvcHRpb25zID0gcnVsZXNbaV1cblxuICAgICAgaWYgKG9wdGlvbnMuaW5jbHVkZSkge1xuICAgICAgICAvLyBhbGwgdmFsaWQgaW5jbHVzaW9ucyBhcmUgcmVtb3ZlZCBieSBzdGF0ZXMoKSBwcmVwcm9jZXNzb3JcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdJbmhlcml0YW5jZSBpcyBub3QgYWxsb3dlZCBpbiBzdGF0ZWxlc3MgbGV4ZXJzJylcbiAgICAgIH1cblxuICAgICAgaWYgKG9wdGlvbnMuZXJyb3IgfHwgb3B0aW9ucy5mYWxsYmFjaykge1xuICAgICAgICAvLyBlcnJvclJ1bGUgY2FuIG9ubHkgYmUgc2V0IG9uY2VcbiAgICAgICAgaWYgKGVycm9yUnVsZSkge1xuICAgICAgICAgIGlmICghb3B0aW9ucy5mYWxsYmFjayA9PT0gIWVycm9yUnVsZS5mYWxsYmFjaykge1xuICAgICAgICAgICAgdGhyb3cgbmV3IEVycm9yKFwiTXVsdGlwbGUgXCIgKyAob3B0aW9ucy5mYWxsYmFjayA/IFwiZmFsbGJhY2tcIiA6IFwiZXJyb3JcIikgKyBcIiBydWxlcyBub3QgYWxsb3dlZCAoZm9yIHRva2VuICdcIiArIG9wdGlvbnMuZGVmYXVsdFR5cGUgKyBcIicpXCIpXG4gICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcihcImZhbGxiYWNrIGFuZCBlcnJvciBhcmUgbXV0dWFsbHkgZXhjbHVzaXZlIChmb3IgdG9rZW4gJ1wiICsgb3B0aW9ucy5kZWZhdWx0VHlwZSArIFwiJylcIilcbiAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICAgICAgZXJyb3JSdWxlID0gb3B0aW9uc1xuICAgICAgfVxuXG4gICAgICB2YXIgbWF0Y2ggPSBvcHRpb25zLm1hdGNoXG4gICAgICBpZiAoZmFzdEFsbG93ZWQpIHtcbiAgICAgICAgd2hpbGUgKG1hdGNoLmxlbmd0aCAmJiB0eXBlb2YgbWF0Y2hbMF0gPT09ICdzdHJpbmcnICYmIG1hdGNoWzBdLmxlbmd0aCA9PT0gMSkge1xuICAgICAgICAgIHZhciB3b3JkID0gbWF0Y2guc2hpZnQoKVxuICAgICAgICAgIGZhc3Rbd29yZC5jaGFyQ29kZUF0KDApXSA9IG9wdGlvbnNcbiAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICAvLyBXYXJuIGFib3V0IGluYXBwcm9wcmlhdGUgc3RhdGUtc3dpdGNoaW5nIG9wdGlvbnNcbiAgICAgIGlmIChvcHRpb25zLnBvcCB8fCBvcHRpb25zLnB1c2ggfHwgb3B0aW9ucy5uZXh0KSB7XG4gICAgICAgIGlmICghaGFzU3RhdGVzKSB7XG4gICAgICAgICAgdGhyb3cgbmV3IEVycm9yKFwiU3RhdGUtc3dpdGNoaW5nIG9wdGlvbnMgYXJlIG5vdCBhbGxvd2VkIGluIHN0YXRlbGVzcyBsZXhlcnMgKGZvciB0b2tlbiAnXCIgKyBvcHRpb25zLmRlZmF1bHRUeXBlICsgXCInKVwiKVxuICAgICAgICB9XG4gICAgICAgIGlmIChvcHRpb25zLmZhbGxiYWNrKSB7XG4gICAgICAgICAgdGhyb3cgbmV3IEVycm9yKFwiU3RhdGUtc3dpdGNoaW5nIG9wdGlvbnMgYXJlIG5vdCBhbGxvd2VkIG9uIGZhbGxiYWNrIHRva2VucyAoZm9yIHRva2VuICdcIiArIG9wdGlvbnMuZGVmYXVsdFR5cGUgKyBcIicpXCIpXG4gICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgLy8gT25seSBydWxlcyB3aXRoIGEgLm1hdGNoIGFyZSBpbmNsdWRlZCBpbiB0aGUgUmVnRXhwXG4gICAgICBpZiAobWF0Y2gubGVuZ3RoID09PSAwKSB7XG4gICAgICAgIGNvbnRpbnVlXG4gICAgICB9XG4gICAgICBmYXN0QWxsb3dlZCA9IGZhbHNlXG5cbiAgICAgIGdyb3Vwcy5wdXNoKG9wdGlvbnMpXG5cbiAgICAgIC8vIGNvbnZlcnQgdG8gUmVnRXhwXG4gICAgICB2YXIgcGF0ID0gcmVVbmlvbihtYXRjaC5tYXAocmVnZXhwT3JMaXRlcmFsKSlcblxuICAgICAgLy8gdmFsaWRhdGVcbiAgICAgIHZhciByZWdleHAgPSBuZXcgUmVnRXhwKHBhdClcbiAgICAgIGlmIChyZWdleHAudGVzdChcIlwiKSkge1xuICAgICAgICB0aHJvdyBuZXcgRXJyb3IoXCJSZWdFeHAgbWF0Y2hlcyBlbXB0eSBzdHJpbmc6IFwiICsgcmVnZXhwKVxuICAgICAgfVxuICAgICAgdmFyIGdyb3VwQ291bnQgPSByZUdyb3VwcyhwYXQpXG4gICAgICBpZiAoZ3JvdXBDb3VudCA+IDApIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKFwiUmVnRXhwIGhhcyBjYXB0dXJlIGdyb3VwczogXCIgKyByZWdleHAgKyBcIlxcblVzZSAoPzog4oCmICkgaW5zdGVhZFwiKVxuICAgICAgfVxuXG4gICAgICAvLyB0cnkgYW5kIGRldGVjdCBydWxlcyBtYXRjaGluZyBuZXdsaW5lc1xuICAgICAgaWYgKCFvcHRpb25zLmxpbmVCcmVha3MgJiYgcmVnZXhwLnRlc3QoJ1xcbicpKSB7XG4gICAgICAgIHRocm93IG5ldyBFcnJvcignUnVsZSBzaG91bGQgZGVjbGFyZSBsaW5lQnJlYWtzOiAnICsgcmVnZXhwKVxuICAgICAgfVxuXG4gICAgICAvLyBzdG9yZSByZWdleFxuICAgICAgcGFydHMucHVzaChyZUNhcHR1cmUocGF0KSlcbiAgICB9XG5cblxuICAgIC8vIElmIHRoZXJlJ3Mgbm8gZmFsbGJhY2sgcnVsZSwgdXNlIHRoZSBzdGlja3kgZmxhZyBzbyB3ZSBvbmx5IGxvb2sgZm9yXG4gICAgLy8gbWF0Y2hlcyBhdCB0aGUgY3VycmVudCBpbmRleC5cbiAgICAvL1xuICAgIC8vIElmIHdlIGRvbid0IHN1cHBvcnQgdGhlIHN0aWNreSBmbGFnLCB0aGVuIGZha2UgaXQgdXNpbmcgYW4gaXJyZWZ1dGFibGVcbiAgICAvLyBtYXRjaCAoaS5lLiBhbiBlbXB0eSBwYXR0ZXJuKS5cbiAgICB2YXIgZmFsbGJhY2tSdWxlID0gZXJyb3JSdWxlICYmIGVycm9yUnVsZS5mYWxsYmFja1xuICAgIHZhciBmbGFncyA9IGhhc1N0aWNreSAmJiAhZmFsbGJhY2tSdWxlID8gJ3ltJyA6ICdnbSdcbiAgICB2YXIgc3VmZml4ID0gaGFzU3RpY2t5IHx8IGZhbGxiYWNrUnVsZSA/ICcnIDogJ3wnXG4gICAgdmFyIGNvbWJpbmVkID0gbmV3IFJlZ0V4cChyZVVuaW9uKHBhcnRzKSArIHN1ZmZpeCwgZmxhZ3MpXG5cbiAgICByZXR1cm4ge3JlZ2V4cDogY29tYmluZWQsIGdyb3VwczogZ3JvdXBzLCBmYXN0OiBmYXN0LCBlcnJvcjogZXJyb3JSdWxlIHx8IGRlZmF1bHRFcnJvclJ1bGV9XG4gIH1cblxuICBmdW5jdGlvbiBjb21waWxlKHJ1bGVzKSB7XG4gICAgdmFyIHJlc3VsdCA9IGNvbXBpbGVSdWxlcyh0b1J1bGVzKHJ1bGVzKSlcbiAgICByZXR1cm4gbmV3IExleGVyKHtzdGFydDogcmVzdWx0fSwgJ3N0YXJ0JylcbiAgfVxuXG4gIGZ1bmN0aW9uIGNoZWNrU3RhdGVHcm91cChnLCBuYW1lLCBtYXApIHtcbiAgICB2YXIgc3RhdGUgPSBnICYmIChnLnB1c2ggfHwgZy5uZXh0KVxuICAgIGlmIChzdGF0ZSAmJiAhbWFwW3N0YXRlXSkge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKFwiTWlzc2luZyBzdGF0ZSAnXCIgKyBzdGF0ZSArIFwiJyAoaW4gdG9rZW4gJ1wiICsgZy5kZWZhdWx0VHlwZSArIFwiJyBvZiBzdGF0ZSAnXCIgKyBuYW1lICsgXCInKVwiKVxuICAgIH1cbiAgICBpZiAoZyAmJiBnLnBvcCAmJiArZy5wb3AgIT09IDEpIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcihcInBvcCBtdXN0IGJlIDEgKGluIHRva2VuICdcIiArIGcuZGVmYXVsdFR5cGUgKyBcIicgb2Ygc3RhdGUgJ1wiICsgbmFtZSArIFwiJylcIilcbiAgICB9XG4gIH1cbiAgZnVuY3Rpb24gY29tcGlsZVN0YXRlcyhzdGF0ZXMsIHN0YXJ0KSB7XG4gICAgdmFyIGFsbCA9IHN0YXRlcy4kYWxsID8gdG9SdWxlcyhzdGF0ZXMuJGFsbCkgOiBbXVxuICAgIGRlbGV0ZSBzdGF0ZXMuJGFsbFxuXG4gICAgdmFyIGtleXMgPSBPYmplY3QuZ2V0T3duUHJvcGVydHlOYW1lcyhzdGF0ZXMpXG4gICAgaWYgKCFzdGFydCkgc3RhcnQgPSBrZXlzWzBdXG5cbiAgICB2YXIgcnVsZU1hcCA9IE9iamVjdC5jcmVhdGUobnVsbClcbiAgICBmb3IgKHZhciBpID0gMDsgaSA8IGtleXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHZhciBrZXkgPSBrZXlzW2ldXG4gICAgICBydWxlTWFwW2tleV0gPSB0b1J1bGVzKHN0YXRlc1trZXldKS5jb25jYXQoYWxsKVxuICAgIH1cbiAgICBmb3IgKHZhciBpID0gMDsgaSA8IGtleXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHZhciBrZXkgPSBrZXlzW2ldXG4gICAgICB2YXIgcnVsZXMgPSBydWxlTWFwW2tleV1cbiAgICAgIHZhciBpbmNsdWRlZCA9IE9iamVjdC5jcmVhdGUobnVsbClcbiAgICAgIGZvciAodmFyIGogPSAwOyBqIDwgcnVsZXMubGVuZ3RoOyBqKyspIHtcbiAgICAgICAgdmFyIHJ1bGUgPSBydWxlc1tqXVxuICAgICAgICBpZiAoIXJ1bGUuaW5jbHVkZSkgY29udGludWVcbiAgICAgICAgdmFyIHNwbGljZSA9IFtqLCAxXVxuICAgICAgICBpZiAocnVsZS5pbmNsdWRlICE9PSBrZXkgJiYgIWluY2x1ZGVkW3J1bGUuaW5jbHVkZV0pIHtcbiAgICAgICAgICBpbmNsdWRlZFtydWxlLmluY2x1ZGVdID0gdHJ1ZVxuICAgICAgICAgIHZhciBuZXdSdWxlcyA9IHJ1bGVNYXBbcnVsZS5pbmNsdWRlXVxuICAgICAgICAgIGlmICghbmV3UnVsZXMpIHtcbiAgICAgICAgICAgIHRocm93IG5ldyBFcnJvcihcIkNhbm5vdCBpbmNsdWRlIG5vbmV4aXN0ZW50IHN0YXRlICdcIiArIHJ1bGUuaW5jbHVkZSArIFwiJyAoaW4gc3RhdGUgJ1wiICsga2V5ICsgXCInKVwiKVxuICAgICAgICAgIH1cbiAgICAgICAgICBmb3IgKHZhciBrID0gMDsgayA8IG5ld1J1bGVzLmxlbmd0aDsgaysrKSB7XG4gICAgICAgICAgICB2YXIgbmV3UnVsZSA9IG5ld1J1bGVzW2tdXG4gICAgICAgICAgICBpZiAocnVsZXMuaW5kZXhPZihuZXdSdWxlKSAhPT0gLTEpIGNvbnRpbnVlXG4gICAgICAgICAgICBzcGxpY2UucHVzaChuZXdSdWxlKVxuICAgICAgICAgIH1cbiAgICAgICAgfVxuICAgICAgICBydWxlcy5zcGxpY2UuYXBwbHkocnVsZXMsIHNwbGljZSlcbiAgICAgICAgai0tXG4gICAgICB9XG4gICAgfVxuXG4gICAgdmFyIG1hcCA9IE9iamVjdC5jcmVhdGUobnVsbClcbiAgICBmb3IgKHZhciBpID0gMDsgaSA8IGtleXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHZhciBrZXkgPSBrZXlzW2ldXG4gICAgICBtYXBba2V5XSA9IGNvbXBpbGVSdWxlcyhydWxlTWFwW2tleV0sIHRydWUpXG4gICAgfVxuXG4gICAgZm9yICh2YXIgaSA9IDA7IGkgPCBrZXlzLmxlbmd0aDsgaSsrKSB7XG4gICAgICB2YXIgbmFtZSA9IGtleXNbaV1cbiAgICAgIHZhciBzdGF0ZSA9IG1hcFtuYW1lXVxuICAgICAgdmFyIGdyb3VwcyA9IHN0YXRlLmdyb3Vwc1xuICAgICAgZm9yICh2YXIgaiA9IDA7IGogPCBncm91cHMubGVuZ3RoOyBqKyspIHtcbiAgICAgICAgY2hlY2tTdGF0ZUdyb3VwKGdyb3Vwc1tqXSwgbmFtZSwgbWFwKVxuICAgICAgfVxuICAgICAgdmFyIGZhc3RLZXlzID0gT2JqZWN0LmdldE93blByb3BlcnR5TmFtZXMoc3RhdGUuZmFzdClcbiAgICAgIGZvciAodmFyIGogPSAwOyBqIDwgZmFzdEtleXMubGVuZ3RoOyBqKyspIHtcbiAgICAgICAgY2hlY2tTdGF0ZUdyb3VwKHN0YXRlLmZhc3RbZmFzdEtleXNbal1dLCBuYW1lLCBtYXApXG4gICAgICB9XG4gICAgfVxuXG4gICAgcmV0dXJuIG5ldyBMZXhlcihtYXAsIHN0YXJ0KVxuICB9XG5cbiAgZnVuY3Rpb24ga2V5d29yZFRyYW5zZm9ybShtYXApIHtcbiAgICB2YXIgcmV2ZXJzZU1hcCA9IE9iamVjdC5jcmVhdGUobnVsbClcbiAgICB2YXIgYnlMZW5ndGggPSBPYmplY3QuY3JlYXRlKG51bGwpXG4gICAgdmFyIHR5cGVzID0gT2JqZWN0LmdldE93blByb3BlcnR5TmFtZXMobWFwKVxuICAgIGZvciAodmFyIGkgPSAwOyBpIDwgdHlwZXMubGVuZ3RoOyBpKyspIHtcbiAgICAgIHZhciB0b2tlblR5cGUgPSB0eXBlc1tpXVxuICAgICAgdmFyIGl0ZW0gPSBtYXBbdG9rZW5UeXBlXVxuICAgICAgdmFyIGtleXdvcmRMaXN0ID0gQXJyYXkuaXNBcnJheShpdGVtKSA/IGl0ZW0gOiBbaXRlbV1cbiAgICAgIGtleXdvcmRMaXN0LmZvckVhY2goZnVuY3Rpb24oa2V5d29yZCkge1xuICAgICAgICAoYnlMZW5ndGhba2V5d29yZC5sZW5ndGhdID0gYnlMZW5ndGhba2V5d29yZC5sZW5ndGhdIHx8IFtdKS5wdXNoKGtleXdvcmQpXG4gICAgICAgIGlmICh0eXBlb2Yga2V5d29yZCAhPT0gJ3N0cmluZycpIHtcbiAgICAgICAgICB0aHJvdyBuZXcgRXJyb3IoXCJrZXl3b3JkIG11c3QgYmUgc3RyaW5nIChpbiBrZXl3b3JkICdcIiArIHRva2VuVHlwZSArIFwiJylcIilcbiAgICAgICAgfVxuICAgICAgICByZXZlcnNlTWFwW2tleXdvcmRdID0gdG9rZW5UeXBlXG4gICAgICB9KVxuICAgIH1cblxuICAgIC8vIGZhc3Qgc3RyaW5nIGxvb2t1cFxuICAgIC8vIGh0dHBzOi8vanNwZXJmLmNvbS9zdHJpbmctbG9va3Vwc1xuICAgIGZ1bmN0aW9uIHN0cih4KSB7IHJldHVybiBKU09OLnN0cmluZ2lmeSh4KSB9XG4gICAgdmFyIHNvdXJjZSA9ICcnXG4gICAgc291cmNlICs9ICdzd2l0Y2ggKHZhbHVlLmxlbmd0aCkge1xcbidcbiAgICBmb3IgKHZhciBsZW5ndGggaW4gYnlMZW5ndGgpIHtcbiAgICAgIHZhciBrZXl3b3JkcyA9IGJ5TGVuZ3RoW2xlbmd0aF1cbiAgICAgIHNvdXJjZSArPSAnY2FzZSAnICsgbGVuZ3RoICsgJzpcXG4nXG4gICAgICBzb3VyY2UgKz0gJ3N3aXRjaCAodmFsdWUpIHtcXG4nXG4gICAgICBrZXl3b3Jkcy5mb3JFYWNoKGZ1bmN0aW9uKGtleXdvcmQpIHtcbiAgICAgICAgdmFyIHRva2VuVHlwZSA9IHJldmVyc2VNYXBba2V5d29yZF1cbiAgICAgICAgc291cmNlICs9ICdjYXNlICcgKyBzdHIoa2V5d29yZCkgKyAnOiByZXR1cm4gJyArIHN0cih0b2tlblR5cGUpICsgJ1xcbidcbiAgICAgIH0pXG4gICAgICBzb3VyY2UgKz0gJ31cXG4nXG4gICAgfVxuICAgIHNvdXJjZSArPSAnfVxcbidcbiAgICByZXR1cm4gRnVuY3Rpb24oJ3ZhbHVlJywgc291cmNlKSAvLyB0eXBlXG4gIH1cblxuICAvKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqL1xuXG4gIHZhciBMZXhlciA9IGZ1bmN0aW9uKHN0YXRlcywgc3RhdGUpIHtcbiAgICB0aGlzLnN0YXJ0U3RhdGUgPSBzdGF0ZVxuICAgIHRoaXMuc3RhdGVzID0gc3RhdGVzXG4gICAgdGhpcy5idWZmZXIgPSAnJ1xuICAgIHRoaXMuc3RhY2sgPSBbXVxuICAgIHRoaXMucmVzZXQoKVxuICB9XG5cbiAgTGV4ZXIucHJvdG90eXBlLnJlc2V0ID0gZnVuY3Rpb24oZGF0YSwgaW5mbykge1xuICAgIHRoaXMuYnVmZmVyID0gZGF0YSB8fCAnJ1xuICAgIHRoaXMuaW5kZXggPSAwXG4gICAgdGhpcy5saW5lID0gaW5mbyA/IGluZm8ubGluZSA6IDFcbiAgICB0aGlzLmNvbCA9IGluZm8gPyBpbmZvLmNvbCA6IDFcbiAgICB0aGlzLnF1ZXVlZFRva2VuID0gaW5mbyA/IGluZm8ucXVldWVkVG9rZW4gOiBudWxsXG4gICAgdGhpcy5xdWV1ZWRUaHJvdyA9IGluZm8gPyBpbmZvLnF1ZXVlZFRocm93IDogbnVsbFxuICAgIHRoaXMuc2V0U3RhdGUoaW5mbyA/IGluZm8uc3RhdGUgOiB0aGlzLnN0YXJ0U3RhdGUpXG4gICAgdGhpcy5zdGFjayA9IGluZm8gJiYgaW5mby5zdGFjayA/IGluZm8uc3RhY2suc2xpY2UoKSA6IFtdXG4gICAgcmV0dXJuIHRoaXNcbiAgfVxuXG4gIExleGVyLnByb3RvdHlwZS5zYXZlID0gZnVuY3Rpb24oKSB7XG4gICAgcmV0dXJuIHtcbiAgICAgIGxpbmU6IHRoaXMubGluZSxcbiAgICAgIGNvbDogdGhpcy5jb2wsXG4gICAgICBzdGF0ZTogdGhpcy5zdGF0ZSxcbiAgICAgIHN0YWNrOiB0aGlzLnN0YWNrLnNsaWNlKCksXG4gICAgICBxdWV1ZWRUb2tlbjogdGhpcy5xdWV1ZWRUb2tlbixcbiAgICAgIHF1ZXVlZFRocm93OiB0aGlzLnF1ZXVlZFRocm93LFxuICAgIH1cbiAgfVxuXG4gIExleGVyLnByb3RvdHlwZS5zZXRTdGF0ZSA9IGZ1bmN0aW9uKHN0YXRlKSB7XG4gICAgaWYgKCFzdGF0ZSB8fCB0aGlzLnN0YXRlID09PSBzdGF0ZSkgcmV0dXJuXG4gICAgdGhpcy5zdGF0ZSA9IHN0YXRlXG4gICAgdmFyIGluZm8gPSB0aGlzLnN0YXRlc1tzdGF0ZV1cbiAgICB0aGlzLmdyb3VwcyA9IGluZm8uZ3JvdXBzXG4gICAgdGhpcy5lcnJvciA9IGluZm8uZXJyb3JcbiAgICB0aGlzLnJlID0gaW5mby5yZWdleHBcbiAgICB0aGlzLmZhc3QgPSBpbmZvLmZhc3RcbiAgfVxuXG4gIExleGVyLnByb3RvdHlwZS5wb3BTdGF0ZSA9IGZ1bmN0aW9uKCkge1xuICAgIHRoaXMuc2V0U3RhdGUodGhpcy5zdGFjay5wb3AoKSlcbiAgfVxuXG4gIExleGVyLnByb3RvdHlwZS5wdXNoU3RhdGUgPSBmdW5jdGlvbihzdGF0ZSkge1xuICAgIHRoaXMuc3RhY2sucHVzaCh0aGlzLnN0YXRlKVxuICAgIHRoaXMuc2V0U3RhdGUoc3RhdGUpXG4gIH1cblxuICB2YXIgZWF0ID0gaGFzU3RpY2t5ID8gZnVuY3Rpb24ocmUsIGJ1ZmZlcikgeyAvLyBhc3N1bWUgcmUgaXMgL3lcbiAgICByZXR1cm4gcmUuZXhlYyhidWZmZXIpXG4gIH0gOiBmdW5jdGlvbihyZSwgYnVmZmVyKSB7IC8vIGFzc3VtZSByZSBpcyAvZ1xuICAgIHZhciBtYXRjaCA9IHJlLmV4ZWMoYnVmZmVyKVxuICAgIC8vIHdpbGwgYWx3YXlzIG1hdGNoLCBzaW5jZSB3ZSB1c2VkIHRoZSB8KD86KSB0cmlja1xuICAgIGlmIChtYXRjaFswXS5sZW5ndGggPT09IDApIHtcbiAgICAgIHJldHVybiBudWxsXG4gICAgfVxuICAgIHJldHVybiBtYXRjaFxuICB9XG5cbiAgTGV4ZXIucHJvdG90eXBlLl9nZXRHcm91cCA9IGZ1bmN0aW9uKG1hdGNoKSB7XG4gICAgdmFyIGdyb3VwQ291bnQgPSB0aGlzLmdyb3Vwcy5sZW5ndGhcbiAgICBmb3IgKHZhciBpID0gMDsgaSA8IGdyb3VwQ291bnQ7IGkrKykge1xuICAgICAgaWYgKG1hdGNoW2kgKyAxXSAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICAgIHJldHVybiB0aGlzLmdyb3Vwc1tpXVxuICAgICAgfVxuICAgIH1cbiAgICB0aHJvdyBuZXcgRXJyb3IoJ0Nhbm5vdCBmaW5kIHRva2VuIHR5cGUgZm9yIG1hdGNoZWQgdGV4dCcpXG4gIH1cblxuICBmdW5jdGlvbiB0b2tlblRvU3RyaW5nKCkge1xuICAgIHJldHVybiB0aGlzLnZhbHVlXG4gIH1cblxuICBMZXhlci5wcm90b3R5cGUubmV4dCA9IGZ1bmN0aW9uKCkge1xuICAgIHZhciBpbmRleCA9IHRoaXMuaW5kZXhcblxuICAgIC8vIElmIGEgZmFsbGJhY2sgdG9rZW4gbWF0Y2hlZCwgd2UgZG9uJ3QgbmVlZCB0byByZS1ydW4gdGhlIFJlZ0V4cFxuICAgIGlmICh0aGlzLnF1ZXVlZEdyb3VwKSB7XG4gICAgICB2YXIgdG9rZW4gPSB0aGlzLl90b2tlbih0aGlzLnF1ZXVlZEdyb3VwLCB0aGlzLnF1ZXVlZFRleHQsIGluZGV4KVxuICAgICAgdGhpcy5xdWV1ZWRHcm91cCA9IG51bGxcbiAgICAgIHRoaXMucXVldWVkVGV4dCA9IFwiXCJcbiAgICAgIHJldHVybiB0b2tlblxuICAgIH1cblxuICAgIHZhciBidWZmZXIgPSB0aGlzLmJ1ZmZlclxuICAgIGlmIChpbmRleCA9PT0gYnVmZmVyLmxlbmd0aCkge1xuICAgICAgcmV0dXJuIC8vIEVPRlxuICAgIH1cblxuICAgIC8vIEZhc3QgbWF0Y2hpbmcgZm9yIHNpbmdsZSBjaGFyYWN0ZXJzXG4gICAgdmFyIGdyb3VwID0gdGhpcy5mYXN0W2J1ZmZlci5jaGFyQ29kZUF0KGluZGV4KV1cbiAgICBpZiAoZ3JvdXApIHtcbiAgICAgIHJldHVybiB0aGlzLl90b2tlbihncm91cCwgYnVmZmVyLmNoYXJBdChpbmRleCksIGluZGV4KVxuICAgIH1cblxuICAgIC8vIEV4ZWN1dGUgUmVnRXhwXG4gICAgdmFyIHJlID0gdGhpcy5yZVxuICAgIHJlLmxhc3RJbmRleCA9IGluZGV4XG4gICAgdmFyIG1hdGNoID0gZWF0KHJlLCBidWZmZXIpXG5cbiAgICAvLyBFcnJvciB0b2tlbnMgbWF0Y2ggdGhlIHJlbWFpbmluZyBidWZmZXJcbiAgICB2YXIgZXJyb3IgPSB0aGlzLmVycm9yXG4gICAgaWYgKG1hdGNoID09IG51bGwpIHtcbiAgICAgIHJldHVybiB0aGlzLl90b2tlbihlcnJvciwgYnVmZmVyLnNsaWNlKGluZGV4LCBidWZmZXIubGVuZ3RoKSwgaW5kZXgpXG4gICAgfVxuXG4gICAgdmFyIGdyb3VwID0gdGhpcy5fZ2V0R3JvdXAobWF0Y2gpXG4gICAgdmFyIHRleHQgPSBtYXRjaFswXVxuXG4gICAgaWYgKGVycm9yLmZhbGxiYWNrICYmIG1hdGNoLmluZGV4ICE9PSBpbmRleCkge1xuICAgICAgdGhpcy5xdWV1ZWRHcm91cCA9IGdyb3VwXG4gICAgICB0aGlzLnF1ZXVlZFRleHQgPSB0ZXh0XG5cbiAgICAgIC8vIEZhbGxiYWNrIHRva2VucyBjb250YWluIHRoZSB1bm1hdGNoZWQgcG9ydGlvbiBvZiB0aGUgYnVmZmVyXG4gICAgICByZXR1cm4gdGhpcy5fdG9rZW4oZXJyb3IsIGJ1ZmZlci5zbGljZShpbmRleCwgbWF0Y2guaW5kZXgpLCBpbmRleClcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcy5fdG9rZW4oZ3JvdXAsIHRleHQsIGluZGV4KVxuICB9XG5cbiAgTGV4ZXIucHJvdG90eXBlLl90b2tlbiA9IGZ1bmN0aW9uKGdyb3VwLCB0ZXh0LCBvZmZzZXQpIHtcbiAgICAvLyBjb3VudCBsaW5lIGJyZWFrc1xuICAgIHZhciBsaW5lQnJlYWtzID0gMFxuICAgIGlmIChncm91cC5saW5lQnJlYWtzKSB7XG4gICAgICB2YXIgbWF0Y2hOTCA9IC9cXG4vZ1xuICAgICAgdmFyIG5sID0gMVxuICAgICAgaWYgKHRleHQgPT09ICdcXG4nKSB7XG4gICAgICAgIGxpbmVCcmVha3MgPSAxXG4gICAgICB9IGVsc2Uge1xuICAgICAgICB3aGlsZSAobWF0Y2hOTC5leGVjKHRleHQpKSB7IGxpbmVCcmVha3MrKzsgbmwgPSBtYXRjaE5MLmxhc3RJbmRleCB9XG4gICAgICB9XG4gICAgfVxuXG4gICAgdmFyIHRva2VuID0ge1xuICAgICAgdHlwZTogKHR5cGVvZiBncm91cC50eXBlID09PSAnZnVuY3Rpb24nICYmIGdyb3VwLnR5cGUodGV4dCkpIHx8IGdyb3VwLmRlZmF1bHRUeXBlLFxuICAgICAgdmFsdWU6IHR5cGVvZiBncm91cC52YWx1ZSA9PT0gJ2Z1bmN0aW9uJyA/IGdyb3VwLnZhbHVlKHRleHQpIDogdGV4dCxcbiAgICAgIHRleHQ6IHRleHQsXG4gICAgICB0b1N0cmluZzogdG9rZW5Ub1N0cmluZyxcbiAgICAgIG9mZnNldDogb2Zmc2V0LFxuICAgICAgbGluZUJyZWFrczogbGluZUJyZWFrcyxcbiAgICAgIGxpbmU6IHRoaXMubGluZSxcbiAgICAgIGNvbDogdGhpcy5jb2wsXG4gICAgfVxuICAgIC8vIG5iLiBhZGRpbmcgbW9yZSBwcm9wcyB0byB0b2tlbiBvYmplY3Qgd2lsbCBtYWtlIFY4IHNhZCFcblxuICAgIHZhciBzaXplID0gdGV4dC5sZW5ndGhcbiAgICB0aGlzLmluZGV4ICs9IHNpemVcbiAgICB0aGlzLmxpbmUgKz0gbGluZUJyZWFrc1xuICAgIGlmIChsaW5lQnJlYWtzICE9PSAwKSB7XG4gICAgICB0aGlzLmNvbCA9IHNpemUgLSBubCArIDFcbiAgICB9IGVsc2Uge1xuICAgICAgdGhpcy5jb2wgKz0gc2l6ZVxuICAgIH1cblxuICAgIC8vIHRocm93LCBpZiBubyBydWxlIHdpdGgge2Vycm9yOiB0cnVlfVxuICAgIGlmIChncm91cC5zaG91bGRUaHJvdykge1xuICAgICAgdGhyb3cgbmV3IEVycm9yKHRoaXMuZm9ybWF0RXJyb3IodG9rZW4sIFwiaW52YWxpZCBzeW50YXhcIikpXG4gICAgfVxuXG4gICAgaWYgKGdyb3VwLnBvcCkgdGhpcy5wb3BTdGF0ZSgpXG4gICAgZWxzZSBpZiAoZ3JvdXAucHVzaCkgdGhpcy5wdXNoU3RhdGUoZ3JvdXAucHVzaClcbiAgICBlbHNlIGlmIChncm91cC5uZXh0KSB0aGlzLnNldFN0YXRlKGdyb3VwLm5leHQpXG5cbiAgICByZXR1cm4gdG9rZW5cbiAgfVxuXG4gIGlmICh0eXBlb2YgU3ltYm9sICE9PSAndW5kZWZpbmVkJyAmJiBTeW1ib2wuaXRlcmF0b3IpIHtcbiAgICB2YXIgTGV4ZXJJdGVyYXRvciA9IGZ1bmN0aW9uKGxleGVyKSB7XG4gICAgICB0aGlzLmxleGVyID0gbGV4ZXJcbiAgICB9XG5cbiAgICBMZXhlckl0ZXJhdG9yLnByb3RvdHlwZS5uZXh0ID0gZnVuY3Rpb24oKSB7XG4gICAgICB2YXIgdG9rZW4gPSB0aGlzLmxleGVyLm5leHQoKVxuICAgICAgcmV0dXJuIHt2YWx1ZTogdG9rZW4sIGRvbmU6ICF0b2tlbn1cbiAgICB9XG5cbiAgICBMZXhlckl0ZXJhdG9yLnByb3RvdHlwZVtTeW1ib2wuaXRlcmF0b3JdID0gZnVuY3Rpb24oKSB7XG4gICAgICByZXR1cm4gdGhpc1xuICAgIH1cblxuICAgIExleGVyLnByb3RvdHlwZVtTeW1ib2wuaXRlcmF0b3JdID0gZnVuY3Rpb24oKSB7XG4gICAgICByZXR1cm4gbmV3IExleGVySXRlcmF0b3IodGhpcylcbiAgICB9XG4gIH1cblxuICBMZXhlci5wcm90b3R5cGUuZm9ybWF0RXJyb3IgPSBmdW5jdGlvbih0b2tlbiwgbWVzc2FnZSkge1xuICAgIHZhciB2YWx1ZSA9IHRva2VuLnRleHRcbiAgICB2YXIgaW5kZXggPSB0b2tlbi5vZmZzZXRcbiAgICB2YXIgZW9sID0gdG9rZW4ubGluZUJyZWFrcyA/IHZhbHVlLmluZGV4T2YoJ1xcbicpIDogdmFsdWUubGVuZ3RoXG4gICAgdmFyIHN0YXJ0ID0gTWF0aC5tYXgoMCwgaW5kZXggLSB0b2tlbi5jb2wgKyAxKVxuICAgIHZhciBmaXJzdExpbmUgPSB0aGlzLmJ1ZmZlci5zdWJzdHJpbmcoc3RhcnQsIGluZGV4ICsgZW9sKVxuICAgIG1lc3NhZ2UgKz0gXCIgYXQgbGluZSBcIiArIHRva2VuLmxpbmUgKyBcIiBjb2wgXCIgKyB0b2tlbi5jb2wgKyBcIjpcXG5cXG5cIlxuICAgIG1lc3NhZ2UgKz0gXCIgIFwiICsgZmlyc3RMaW5lICsgXCJcXG5cIlxuICAgIG1lc3NhZ2UgKz0gXCIgIFwiICsgQXJyYXkodG9rZW4uY29sKS5qb2luKFwiIFwiKSArIFwiXlwiXG4gICAgcmV0dXJuIG1lc3NhZ2VcbiAgfVxuXG4gIExleGVyLnByb3RvdHlwZS5jbG9uZSA9IGZ1bmN0aW9uKCkge1xuICAgIHJldHVybiBuZXcgTGV4ZXIodGhpcy5zdGF0ZXMsIHRoaXMuc3RhdGUpXG4gIH1cblxuICBMZXhlci5wcm90b3R5cGUuaGFzID0gZnVuY3Rpb24odG9rZW5UeXBlKSB7XG4gICAgcmV0dXJuIHRydWVcbiAgfVxuXG5cbiAgcmV0dXJuIHtcbiAgICBjb21waWxlOiBjb21waWxlLFxuICAgIHN0YXRlczogY29tcGlsZVN0YXRlcyxcbiAgICBlcnJvcjogT2JqZWN0LmZyZWV6ZSh7ZXJyb3I6IHRydWV9KSxcbiAgICBmYWxsYmFjazogT2JqZWN0LmZyZWV6ZSh7ZmFsbGJhY2s6IHRydWV9KSxcbiAgICBrZXl3b3Jkczoga2V5d29yZFRyYW5zZm9ybSxcbiAgfVxuXG59KSk7XG4iLCIoZnVuY3Rpb24ocm9vdCwgZmFjdG9yeSkge1xuICAgIGlmICh0eXBlb2YgbW9kdWxlID09PSAnb2JqZWN0JyAmJiBtb2R1bGUuZXhwb3J0cykge1xuICAgICAgICBtb2R1bGUuZXhwb3J0cyA9IGZhY3RvcnkoKTtcbiAgICB9IGVsc2Uge1xuICAgICAgICByb290Lm5lYXJsZXkgPSBmYWN0b3J5KCk7XG4gICAgfVxufSh0aGlzLCBmdW5jdGlvbigpIHtcblxuICAgIGZ1bmN0aW9uIFJ1bGUobmFtZSwgc3ltYm9scywgcG9zdHByb2Nlc3MpIHtcbiAgICAgICAgdGhpcy5pZCA9ICsrUnVsZS5oaWdoZXN0SWQ7XG4gICAgICAgIHRoaXMubmFtZSA9IG5hbWU7XG4gICAgICAgIHRoaXMuc3ltYm9scyA9IHN5bWJvbHM7ICAgICAgICAvLyBhIGxpc3Qgb2YgbGl0ZXJhbCB8IHJlZ2V4IGNsYXNzIHwgbm9udGVybWluYWxcbiAgICAgICAgdGhpcy5wb3N0cHJvY2VzcyA9IHBvc3Rwcm9jZXNzO1xuICAgICAgICByZXR1cm4gdGhpcztcbiAgICB9XG4gICAgUnVsZS5oaWdoZXN0SWQgPSAwO1xuXG4gICAgUnVsZS5wcm90b3R5cGUudG9TdHJpbmcgPSBmdW5jdGlvbih3aXRoQ3Vyc29yQXQpIHtcbiAgICAgICAgZnVuY3Rpb24gc3RyaW5naWZ5U3ltYm9sU2VxdWVuY2UgKGUpIHtcbiAgICAgICAgICAgIHJldHVybiBlLmxpdGVyYWwgPyBKU09OLnN0cmluZ2lmeShlLmxpdGVyYWwpIDpcbiAgICAgICAgICAgICAgICAgICBlLnR5cGUgPyAnJScgKyBlLnR5cGUgOiBlLnRvU3RyaW5nKCk7XG4gICAgICAgIH1cbiAgICAgICAgdmFyIHN5bWJvbFNlcXVlbmNlID0gKHR5cGVvZiB3aXRoQ3Vyc29yQXQgPT09IFwidW5kZWZpbmVkXCIpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgID8gdGhpcy5zeW1ib2xzLm1hcChzdHJpbmdpZnlTeW1ib2xTZXF1ZW5jZSkuam9pbignICcpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgIDogKCAgIHRoaXMuc3ltYm9scy5zbGljZSgwLCB3aXRoQ3Vyc29yQXQpLm1hcChzdHJpbmdpZnlTeW1ib2xTZXF1ZW5jZSkuam9pbignICcpXG4gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICArIFwiIOKXjyBcIlxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgKyB0aGlzLnN5bWJvbHMuc2xpY2Uod2l0aEN1cnNvckF0KS5tYXAoc3RyaW5naWZ5U3ltYm9sU2VxdWVuY2UpLmpvaW4oJyAnKSAgICAgKTtcbiAgICAgICAgcmV0dXJuIHRoaXMubmFtZSArIFwiIOKGkiBcIiArIHN5bWJvbFNlcXVlbmNlO1xuICAgIH1cblxuXG4gICAgLy8gYSBTdGF0ZSBpcyBhIHJ1bGUgYXQgYSBwb3NpdGlvbiBmcm9tIGEgZ2l2ZW4gc3RhcnRpbmcgcG9pbnQgaW4gdGhlIGlucHV0IHN0cmVhbSAocmVmZXJlbmNlKVxuICAgIGZ1bmN0aW9uIFN0YXRlKHJ1bGUsIGRvdCwgcmVmZXJlbmNlLCB3YW50ZWRCeSkge1xuICAgICAgICB0aGlzLnJ1bGUgPSBydWxlO1xuICAgICAgICB0aGlzLmRvdCA9IGRvdDtcbiAgICAgICAgdGhpcy5yZWZlcmVuY2UgPSByZWZlcmVuY2U7XG4gICAgICAgIHRoaXMuZGF0YSA9IFtdO1xuICAgICAgICB0aGlzLndhbnRlZEJ5ID0gd2FudGVkQnk7XG4gICAgICAgIHRoaXMuaXNDb21wbGV0ZSA9IHRoaXMuZG90ID09PSBydWxlLnN5bWJvbHMubGVuZ3RoO1xuICAgIH1cblxuICAgIFN0YXRlLnByb3RvdHlwZS50b1N0cmluZyA9IGZ1bmN0aW9uKCkge1xuICAgICAgICByZXR1cm4gXCJ7XCIgKyB0aGlzLnJ1bGUudG9TdHJpbmcodGhpcy5kb3QpICsgXCJ9LCBmcm9tOiBcIiArICh0aGlzLnJlZmVyZW5jZSB8fCAwKTtcbiAgICB9O1xuXG4gICAgU3RhdGUucHJvdG90eXBlLm5leHRTdGF0ZSA9IGZ1bmN0aW9uKGNoaWxkKSB7XG4gICAgICAgIHZhciBzdGF0ZSA9IG5ldyBTdGF0ZSh0aGlzLnJ1bGUsIHRoaXMuZG90ICsgMSwgdGhpcy5yZWZlcmVuY2UsIHRoaXMud2FudGVkQnkpO1xuICAgICAgICBzdGF0ZS5sZWZ0ID0gdGhpcztcbiAgICAgICAgc3RhdGUucmlnaHQgPSBjaGlsZDtcbiAgICAgICAgaWYgKHN0YXRlLmlzQ29tcGxldGUpIHtcbiAgICAgICAgICAgIHN0YXRlLmRhdGEgPSBzdGF0ZS5idWlsZCgpO1xuICAgICAgICB9XG4gICAgICAgIHJldHVybiBzdGF0ZTtcbiAgICB9O1xuXG4gICAgU3RhdGUucHJvdG90eXBlLmJ1aWxkID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciBjaGlsZHJlbiA9IFtdO1xuICAgICAgICB2YXIgbm9kZSA9IHRoaXM7XG4gICAgICAgIGRvIHtcbiAgICAgICAgICAgIGNoaWxkcmVuLnB1c2gobm9kZS5yaWdodC5kYXRhKTtcbiAgICAgICAgICAgIG5vZGUgPSBub2RlLmxlZnQ7XG4gICAgICAgIH0gd2hpbGUgKG5vZGUubGVmdCk7XG4gICAgICAgIGNoaWxkcmVuLnJldmVyc2UoKTtcbiAgICAgICAgcmV0dXJuIGNoaWxkcmVuO1xuICAgIH07XG5cbiAgICBTdGF0ZS5wcm90b3R5cGUuZmluaXNoID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIGlmICh0aGlzLnJ1bGUucG9zdHByb2Nlc3MpIHtcbiAgICAgICAgICAgIHRoaXMuZGF0YSA9IHRoaXMucnVsZS5wb3N0cHJvY2Vzcyh0aGlzLmRhdGEsIHRoaXMucmVmZXJlbmNlLCBQYXJzZXIuZmFpbCk7XG4gICAgICAgIH1cbiAgICB9O1xuXG5cbiAgICBmdW5jdGlvbiBDb2x1bW4oZ3JhbW1hciwgaW5kZXgpIHtcbiAgICAgICAgdGhpcy5ncmFtbWFyID0gZ3JhbW1hcjtcbiAgICAgICAgdGhpcy5pbmRleCA9IGluZGV4O1xuICAgICAgICB0aGlzLnN0YXRlcyA9IFtdO1xuICAgICAgICB0aGlzLndhbnRzID0ge307IC8vIHN0YXRlcyBpbmRleGVkIGJ5IHRoZSBub24tdGVybWluYWwgdGhleSBleHBlY3RcbiAgICAgICAgdGhpcy5zY2FubmFibGUgPSBbXTsgLy8gbGlzdCBvZiBzdGF0ZXMgdGhhdCBleHBlY3QgYSB0b2tlblxuICAgICAgICB0aGlzLmNvbXBsZXRlZCA9IHt9OyAvLyBzdGF0ZXMgdGhhdCBhcmUgbnVsbGFibGVcbiAgICB9XG5cblxuICAgIENvbHVtbi5wcm90b3R5cGUucHJvY2VzcyA9IGZ1bmN0aW9uKG5leHRDb2x1bW4pIHtcbiAgICAgICAgdmFyIHN0YXRlcyA9IHRoaXMuc3RhdGVzO1xuICAgICAgICB2YXIgd2FudHMgPSB0aGlzLndhbnRzO1xuICAgICAgICB2YXIgY29tcGxldGVkID0gdGhpcy5jb21wbGV0ZWQ7XG5cbiAgICAgICAgZm9yICh2YXIgdyA9IDA7IHcgPCBzdGF0ZXMubGVuZ3RoOyB3KyspIHsgLy8gbmIuIHdlIHB1c2goKSBkdXJpbmcgaXRlcmF0aW9uXG4gICAgICAgICAgICB2YXIgc3RhdGUgPSBzdGF0ZXNbd107XG5cbiAgICAgICAgICAgIGlmIChzdGF0ZS5pc0NvbXBsZXRlKSB7XG4gICAgICAgICAgICAgICAgc3RhdGUuZmluaXNoKCk7XG4gICAgICAgICAgICAgICAgaWYgKHN0YXRlLmRhdGEgIT09IFBhcnNlci5mYWlsKSB7XG4gICAgICAgICAgICAgICAgICAgIC8vIGNvbXBsZXRlXG4gICAgICAgICAgICAgICAgICAgIHZhciB3YW50ZWRCeSA9IHN0YXRlLndhbnRlZEJ5O1xuICAgICAgICAgICAgICAgICAgICBmb3IgKHZhciBpID0gd2FudGVkQnkubGVuZ3RoOyBpLS07ICkgeyAvLyB0aGlzIGxpbmUgaXMgaG90XG4gICAgICAgICAgICAgICAgICAgICAgICB2YXIgbGVmdCA9IHdhbnRlZEJ5W2ldO1xuICAgICAgICAgICAgICAgICAgICAgICAgdGhpcy5jb21wbGV0ZShsZWZ0LCBzdGF0ZSk7XG4gICAgICAgICAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgICAgICAgICAvLyBzcGVjaWFsLWNhc2UgbnVsbGFibGVzXG4gICAgICAgICAgICAgICAgICAgIGlmIChzdGF0ZS5yZWZlcmVuY2UgPT09IHRoaXMuaW5kZXgpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIC8vIG1ha2Ugc3VyZSBmdXR1cmUgcHJlZGljdG9ycyBvZiB0aGlzIHJ1bGUgZ2V0IGNvbXBsZXRlZC5cbiAgICAgICAgICAgICAgICAgICAgICAgIHZhciBleHAgPSBzdGF0ZS5ydWxlLm5hbWU7XG4gICAgICAgICAgICAgICAgICAgICAgICAodGhpcy5jb21wbGV0ZWRbZXhwXSA9IHRoaXMuY29tcGxldGVkW2V4cF0gfHwgW10pLnB1c2goc3RhdGUpO1xuICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgIC8vIHF1ZXVlIHNjYW5uYWJsZSBzdGF0ZXNcbiAgICAgICAgICAgICAgICB2YXIgZXhwID0gc3RhdGUucnVsZS5zeW1ib2xzW3N0YXRlLmRvdF07XG4gICAgICAgICAgICAgICAgaWYgKHR5cGVvZiBleHAgIT09ICdzdHJpbmcnKSB7XG4gICAgICAgICAgICAgICAgICAgIHRoaXMuc2Nhbm5hYmxlLnB1c2goc3RhdGUpO1xuICAgICAgICAgICAgICAgICAgICBjb250aW51ZTtcbiAgICAgICAgICAgICAgICB9XG5cbiAgICAgICAgICAgICAgICAvLyBwcmVkaWN0XG4gICAgICAgICAgICAgICAgaWYgKHdhbnRzW2V4cF0pIHtcbiAgICAgICAgICAgICAgICAgICAgd2FudHNbZXhwXS5wdXNoKHN0YXRlKTtcblxuICAgICAgICAgICAgICAgICAgICBpZiAoY29tcGxldGVkLmhhc093blByb3BlcnR5KGV4cCkpIHtcbiAgICAgICAgICAgICAgICAgICAgICAgIHZhciBudWxscyA9IGNvbXBsZXRlZFtleHBdO1xuICAgICAgICAgICAgICAgICAgICAgICAgZm9yICh2YXIgaSA9IDA7IGkgPCBudWxscy5sZW5ndGg7IGkrKykge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHZhciByaWdodCA9IG51bGxzW2ldO1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIHRoaXMuY29tcGxldGUoc3RhdGUsIHJpZ2h0KTtcbiAgICAgICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICAgICAgIHdhbnRzW2V4cF0gPSBbc3RhdGVdO1xuICAgICAgICAgICAgICAgICAgICB0aGlzLnByZWRpY3QoZXhwKTtcbiAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9XG4gICAgICAgIH1cbiAgICB9XG5cbiAgICBDb2x1bW4ucHJvdG90eXBlLnByZWRpY3QgPSBmdW5jdGlvbihleHApIHtcbiAgICAgICAgdmFyIHJ1bGVzID0gdGhpcy5ncmFtbWFyLmJ5TmFtZVtleHBdIHx8IFtdO1xuXG4gICAgICAgIGZvciAodmFyIGkgPSAwOyBpIDwgcnVsZXMubGVuZ3RoOyBpKyspIHtcbiAgICAgICAgICAgIHZhciByID0gcnVsZXNbaV07XG4gICAgICAgICAgICB2YXIgd2FudGVkQnkgPSB0aGlzLndhbnRzW2V4cF07XG4gICAgICAgICAgICB2YXIgcyA9IG5ldyBTdGF0ZShyLCAwLCB0aGlzLmluZGV4LCB3YW50ZWRCeSk7XG4gICAgICAgICAgICB0aGlzLnN0YXRlcy5wdXNoKHMpO1xuICAgICAgICB9XG4gICAgfVxuXG4gICAgQ29sdW1uLnByb3RvdHlwZS5jb21wbGV0ZSA9IGZ1bmN0aW9uKGxlZnQsIHJpZ2h0KSB7XG4gICAgICAgIHZhciBjb3B5ID0gbGVmdC5uZXh0U3RhdGUocmlnaHQpO1xuICAgICAgICB0aGlzLnN0YXRlcy5wdXNoKGNvcHkpO1xuICAgIH1cblxuXG4gICAgZnVuY3Rpb24gR3JhbW1hcihydWxlcywgc3RhcnQpIHtcbiAgICAgICAgdGhpcy5ydWxlcyA9IHJ1bGVzO1xuICAgICAgICB0aGlzLnN0YXJ0ID0gc3RhcnQgfHwgdGhpcy5ydWxlc1swXS5uYW1lO1xuICAgICAgICB2YXIgYnlOYW1lID0gdGhpcy5ieU5hbWUgPSB7fTtcbiAgICAgICAgdGhpcy5ydWxlcy5mb3JFYWNoKGZ1bmN0aW9uKHJ1bGUpIHtcbiAgICAgICAgICAgIGlmICghYnlOYW1lLmhhc093blByb3BlcnR5KHJ1bGUubmFtZSkpIHtcbiAgICAgICAgICAgICAgICBieU5hbWVbcnVsZS5uYW1lXSA9IFtdO1xuICAgICAgICAgICAgfVxuICAgICAgICAgICAgYnlOYW1lW3J1bGUubmFtZV0ucHVzaChydWxlKTtcbiAgICAgICAgfSk7XG4gICAgfVxuXG4gICAgLy8gU28gd2UgY2FuIGFsbG93IHBhc3NpbmcgKHJ1bGVzLCBzdGFydCkgZGlyZWN0bHkgdG8gUGFyc2VyIGZvciBiYWNrd2FyZHMgY29tcGF0aWJpbGl0eVxuICAgIEdyYW1tYXIuZnJvbUNvbXBpbGVkID0gZnVuY3Rpb24ocnVsZXMsIHN0YXJ0KSB7XG4gICAgICAgIHZhciBsZXhlciA9IHJ1bGVzLkxleGVyO1xuICAgICAgICBpZiAocnVsZXMuUGFyc2VyU3RhcnQpIHtcbiAgICAgICAgICBzdGFydCA9IHJ1bGVzLlBhcnNlclN0YXJ0O1xuICAgICAgICAgIHJ1bGVzID0gcnVsZXMuUGFyc2VyUnVsZXM7XG4gICAgICAgIH1cbiAgICAgICAgdmFyIHJ1bGVzID0gcnVsZXMubWFwKGZ1bmN0aW9uIChyKSB7IHJldHVybiAobmV3IFJ1bGUoci5uYW1lLCByLnN5bWJvbHMsIHIucG9zdHByb2Nlc3MpKTsgfSk7XG4gICAgICAgIHZhciBnID0gbmV3IEdyYW1tYXIocnVsZXMsIHN0YXJ0KTtcbiAgICAgICAgZy5sZXhlciA9IGxleGVyOyAvLyBuYi4gc3RvcmluZyBsZXhlciBvbiBHcmFtbWFyIGlzIGlmZnksIGJ1dCB1bmF2b2lkYWJsZVxuICAgICAgICByZXR1cm4gZztcbiAgICB9XG5cblxuICAgIGZ1bmN0aW9uIFN0cmVhbUxleGVyKCkge1xuICAgICAgdGhpcy5yZXNldChcIlwiKTtcbiAgICB9XG5cbiAgICBTdHJlYW1MZXhlci5wcm90b3R5cGUucmVzZXQgPSBmdW5jdGlvbihkYXRhLCBzdGF0ZSkge1xuICAgICAgICB0aGlzLmJ1ZmZlciA9IGRhdGE7XG4gICAgICAgIHRoaXMuaW5kZXggPSAwO1xuICAgICAgICB0aGlzLmxpbmUgPSBzdGF0ZSA/IHN0YXRlLmxpbmUgOiAxO1xuICAgICAgICB0aGlzLmxhc3RMaW5lQnJlYWsgPSBzdGF0ZSA/IC1zdGF0ZS5jb2wgOiAwO1xuICAgIH1cblxuICAgIFN0cmVhbUxleGVyLnByb3RvdHlwZS5uZXh0ID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIGlmICh0aGlzLmluZGV4IDwgdGhpcy5idWZmZXIubGVuZ3RoKSB7XG4gICAgICAgICAgICB2YXIgY2ggPSB0aGlzLmJ1ZmZlclt0aGlzLmluZGV4KytdO1xuICAgICAgICAgICAgaWYgKGNoID09PSAnXFxuJykge1xuICAgICAgICAgICAgICB0aGlzLmxpbmUgKz0gMTtcbiAgICAgICAgICAgICAgdGhpcy5sYXN0TGluZUJyZWFrID0gdGhpcy5pbmRleDtcbiAgICAgICAgICAgIH1cbiAgICAgICAgICAgIHJldHVybiB7dmFsdWU6IGNofTtcbiAgICAgICAgfVxuICAgIH1cblxuICAgIFN0cmVhbUxleGVyLnByb3RvdHlwZS5zYXZlID0gZnVuY3Rpb24oKSB7XG4gICAgICByZXR1cm4ge1xuICAgICAgICBsaW5lOiB0aGlzLmxpbmUsXG4gICAgICAgIGNvbDogdGhpcy5pbmRleCAtIHRoaXMubGFzdExpbmVCcmVhayxcbiAgICAgIH1cbiAgICB9XG5cbiAgICBTdHJlYW1MZXhlci5wcm90b3R5cGUuZm9ybWF0RXJyb3IgPSBmdW5jdGlvbih0b2tlbiwgbWVzc2FnZSkge1xuICAgICAgICAvLyBuYi4gdGhpcyBnZXRzIGNhbGxlZCBhZnRlciBjb25zdW1pbmcgdGhlIG9mZmVuZGluZyB0b2tlbixcbiAgICAgICAgLy8gc28gdGhlIGN1bHByaXQgaXMgaW5kZXgtMVxuICAgICAgICB2YXIgYnVmZmVyID0gdGhpcy5idWZmZXI7XG4gICAgICAgIGlmICh0eXBlb2YgYnVmZmVyID09PSAnc3RyaW5nJykge1xuICAgICAgICAgICAgdmFyIG5leHRMaW5lQnJlYWsgPSBidWZmZXIuaW5kZXhPZignXFxuJywgdGhpcy5pbmRleCk7XG4gICAgICAgICAgICBpZiAobmV4dExpbmVCcmVhayA9PT0gLTEpIG5leHRMaW5lQnJlYWsgPSBidWZmZXIubGVuZ3RoO1xuICAgICAgICAgICAgdmFyIGxpbmUgPSBidWZmZXIuc3Vic3RyaW5nKHRoaXMubGFzdExpbmVCcmVhaywgbmV4dExpbmVCcmVhaylcbiAgICAgICAgICAgIHZhciBjb2wgPSB0aGlzLmluZGV4IC0gdGhpcy5sYXN0TGluZUJyZWFrO1xuICAgICAgICAgICAgbWVzc2FnZSArPSBcIiBhdCBsaW5lIFwiICsgdGhpcy5saW5lICsgXCIgY29sIFwiICsgY29sICsgXCI6XFxuXFxuXCI7XG4gICAgICAgICAgICBtZXNzYWdlICs9IFwiICBcIiArIGxpbmUgKyBcIlxcblwiXG4gICAgICAgICAgICBtZXNzYWdlICs9IFwiICBcIiArIEFycmF5KGNvbCkuam9pbihcIiBcIikgKyBcIl5cIlxuICAgICAgICAgICAgcmV0dXJuIG1lc3NhZ2U7XG4gICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICByZXR1cm4gbWVzc2FnZSArIFwiIGF0IGluZGV4IFwiICsgKHRoaXMuaW5kZXggLSAxKTtcbiAgICAgICAgfVxuICAgIH1cblxuXG4gICAgZnVuY3Rpb24gUGFyc2VyKHJ1bGVzLCBzdGFydCwgb3B0aW9ucykge1xuICAgICAgICBpZiAocnVsZXMgaW5zdGFuY2VvZiBHcmFtbWFyKSB7XG4gICAgICAgICAgICB2YXIgZ3JhbW1hciA9IHJ1bGVzO1xuICAgICAgICAgICAgdmFyIG9wdGlvbnMgPSBzdGFydDtcbiAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHZhciBncmFtbWFyID0gR3JhbW1hci5mcm9tQ29tcGlsZWQocnVsZXMsIHN0YXJ0KTtcbiAgICAgICAgfVxuICAgICAgICB0aGlzLmdyYW1tYXIgPSBncmFtbWFyO1xuXG4gICAgICAgIC8vIFJlYWQgb3B0aW9uc1xuICAgICAgICB0aGlzLm9wdGlvbnMgPSB7XG4gICAgICAgICAgICBrZWVwSGlzdG9yeTogZmFsc2UsXG4gICAgICAgICAgICBsZXhlcjogZ3JhbW1hci5sZXhlciB8fCBuZXcgU3RyZWFtTGV4ZXIsXG4gICAgICAgIH07XG4gICAgICAgIGZvciAodmFyIGtleSBpbiAob3B0aW9ucyB8fCB7fSkpIHtcbiAgICAgICAgICAgIHRoaXMub3B0aW9uc1trZXldID0gb3B0aW9uc1trZXldO1xuICAgICAgICB9XG5cbiAgICAgICAgLy8gU2V0dXAgbGV4ZXJcbiAgICAgICAgdGhpcy5sZXhlciA9IHRoaXMub3B0aW9ucy5sZXhlcjtcbiAgICAgICAgdGhpcy5sZXhlclN0YXRlID0gdW5kZWZpbmVkO1xuXG4gICAgICAgIC8vIFNldHVwIGEgdGFibGVcbiAgICAgICAgdmFyIGNvbHVtbiA9IG5ldyBDb2x1bW4oZ3JhbW1hciwgMCk7XG4gICAgICAgIHZhciB0YWJsZSA9IHRoaXMudGFibGUgPSBbY29sdW1uXTtcblxuICAgICAgICAvLyBJIGNvdWxkIGJlIGV4cGVjdGluZyBhbnl0aGluZy5cbiAgICAgICAgY29sdW1uLndhbnRzW2dyYW1tYXIuc3RhcnRdID0gW107XG4gICAgICAgIGNvbHVtbi5wcmVkaWN0KGdyYW1tYXIuc3RhcnQpO1xuICAgICAgICAvLyBUT0RPIHdoYXQgaWYgc3RhcnQgcnVsZSBpcyBudWxsYWJsZT9cbiAgICAgICAgY29sdW1uLnByb2Nlc3MoKTtcbiAgICAgICAgdGhpcy5jdXJyZW50ID0gMDsgLy8gdG9rZW4gaW5kZXhcbiAgICB9XG5cbiAgICAvLyBjcmVhdGUgYSByZXNlcnZlZCB0b2tlbiBmb3IgaW5kaWNhdGluZyBhIHBhcnNlIGZhaWxcbiAgICBQYXJzZXIuZmFpbCA9IHt9O1xuXG4gICAgUGFyc2VyLnByb3RvdHlwZS5mZWVkID0gZnVuY3Rpb24oY2h1bmspIHtcbiAgICAgICAgdmFyIGxleGVyID0gdGhpcy5sZXhlcjtcbiAgICAgICAgbGV4ZXIucmVzZXQoY2h1bmssIHRoaXMubGV4ZXJTdGF0ZSk7XG5cbiAgICAgICAgdmFyIHRva2VuO1xuICAgICAgICB3aGlsZSAodG9rZW4gPSBsZXhlci5uZXh0KCkpIHtcbiAgICAgICAgICAgIC8vIFdlIGFkZCBuZXcgc3RhdGVzIHRvIHRhYmxlW2N1cnJlbnQrMV1cbiAgICAgICAgICAgIHZhciBjb2x1bW4gPSB0aGlzLnRhYmxlW3RoaXMuY3VycmVudF07XG5cbiAgICAgICAgICAgIC8vIEdDIHVudXNlZCBzdGF0ZXNcbiAgICAgICAgICAgIGlmICghdGhpcy5vcHRpb25zLmtlZXBIaXN0b3J5KSB7XG4gICAgICAgICAgICAgICAgZGVsZXRlIHRoaXMudGFibGVbdGhpcy5jdXJyZW50IC0gMV07XG4gICAgICAgICAgICB9XG5cbiAgICAgICAgICAgIHZhciBuID0gdGhpcy5jdXJyZW50ICsgMTtcbiAgICAgICAgICAgIHZhciBuZXh0Q29sdW1uID0gbmV3IENvbHVtbih0aGlzLmdyYW1tYXIsIG4pO1xuICAgICAgICAgICAgdGhpcy50YWJsZS5wdXNoKG5leHRDb2x1bW4pO1xuXG4gICAgICAgICAgICAvLyBBZHZhbmNlIGFsbCB0b2tlbnMgdGhhdCBleHBlY3QgdGhlIHN5bWJvbFxuICAgICAgICAgICAgdmFyIGxpdGVyYWwgPSB0b2tlbi50ZXh0ICE9PSB1bmRlZmluZWQgPyB0b2tlbi50ZXh0IDogdG9rZW4udmFsdWU7XG4gICAgICAgICAgICB2YXIgdmFsdWUgPSBsZXhlci5jb25zdHJ1Y3RvciA9PT0gU3RyZWFtTGV4ZXIgPyB0b2tlbi52YWx1ZSA6IHRva2VuO1xuICAgICAgICAgICAgdmFyIHNjYW5uYWJsZSA9IGNvbHVtbi5zY2FubmFibGU7XG4gICAgICAgICAgICBmb3IgKHZhciB3ID0gc2Nhbm5hYmxlLmxlbmd0aDsgdy0tOyApIHtcbiAgICAgICAgICAgICAgICB2YXIgc3RhdGUgPSBzY2FubmFibGVbd107XG4gICAgICAgICAgICAgICAgdmFyIGV4cGVjdCA9IHN0YXRlLnJ1bGUuc3ltYm9sc1tzdGF0ZS5kb3RdO1xuICAgICAgICAgICAgICAgIC8vIFRyeSB0byBjb25zdW1lIHRoZSB0b2tlblxuICAgICAgICAgICAgICAgIC8vIGVpdGhlciByZWdleCBvciBsaXRlcmFsXG4gICAgICAgICAgICAgICAgaWYgKGV4cGVjdC50ZXN0ID8gZXhwZWN0LnRlc3QodmFsdWUpIDpcbiAgICAgICAgICAgICAgICAgICAgZXhwZWN0LnR5cGUgPyBleHBlY3QudHlwZSA9PT0gdG9rZW4udHlwZVxuICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA6IGV4cGVjdC5saXRlcmFsID09PSBsaXRlcmFsKSB7XG4gICAgICAgICAgICAgICAgICAgIC8vIEFkZCBpdFxuICAgICAgICAgICAgICAgICAgICB2YXIgbmV4dCA9IHN0YXRlLm5leHRTdGF0ZSh7ZGF0YTogdmFsdWUsIHRva2VuOiB0b2tlbiwgaXNUb2tlbjogdHJ1ZSwgcmVmZXJlbmNlOiBuIC0gMX0pO1xuICAgICAgICAgICAgICAgICAgICBuZXh0Q29sdW1uLnN0YXRlcy5wdXNoKG5leHQpO1xuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgLy8gTmV4dCwgZm9yIGVhY2ggb2YgdGhlIHJ1bGVzLCB3ZSBlaXRoZXJcbiAgICAgICAgICAgIC8vIChhKSBjb21wbGV0ZSBpdCwgYW5kIHRyeSB0byBzZWUgaWYgdGhlIHJlZmVyZW5jZSByb3cgZXhwZWN0ZWQgdGhhdFxuICAgICAgICAgICAgLy8gICAgIHJ1bGVcbiAgICAgICAgICAgIC8vIChiKSBwcmVkaWN0IHRoZSBuZXh0IG5vbnRlcm1pbmFsIGl0IGV4cGVjdHMgYnkgYWRkaW5nIHRoYXRcbiAgICAgICAgICAgIC8vICAgICBub250ZXJtaW5hbCdzIHN0YXJ0IHN0YXRlXG4gICAgICAgICAgICAvLyBUbyBwcmV2ZW50IGR1cGxpY2F0aW9uLCB3ZSBhbHNvIGtlZXAgdHJhY2sgb2YgcnVsZXMgd2UgaGF2ZSBhbHJlYWR5XG4gICAgICAgICAgICAvLyBhZGRlZFxuXG4gICAgICAgICAgICBuZXh0Q29sdW1uLnByb2Nlc3MoKTtcblxuICAgICAgICAgICAgLy8gSWYgbmVlZGVkLCB0aHJvdyBhbiBlcnJvcjpcbiAgICAgICAgICAgIGlmIChuZXh0Q29sdW1uLnN0YXRlcy5sZW5ndGggPT09IDApIHtcbiAgICAgICAgICAgICAgICAvLyBObyBzdGF0ZXMgYXQgYWxsISBUaGlzIGlzIG5vdCBnb29kLlxuICAgICAgICAgICAgICAgIHZhciBtZXNzYWdlID0gdGhpcy5sZXhlci5mb3JtYXRFcnJvcih0b2tlbiwgXCJpbnZhbGlkIHN5bnRheFwiKSArIFwiXFxuXCI7XG4gICAgICAgICAgICAgICAgbWVzc2FnZSArPSBcIlVuZXhwZWN0ZWQgXCIgKyAodG9rZW4udHlwZSA/IHRva2VuLnR5cGUgKyBcIiB0b2tlbjogXCIgOiBcIlwiKTtcbiAgICAgICAgICAgICAgICBtZXNzYWdlICs9IEpTT04uc3RyaW5naWZ5KHRva2VuLnZhbHVlICE9PSB1bmRlZmluZWQgPyB0b2tlbi52YWx1ZSA6IHRva2VuKSArIFwiXFxuXCI7XG4gICAgICAgICAgICAgICAgdmFyIGVyciA9IG5ldyBFcnJvcihtZXNzYWdlKTtcbiAgICAgICAgICAgICAgICBlcnIub2Zmc2V0ID0gdGhpcy5jdXJyZW50O1xuICAgICAgICAgICAgICAgIGVyci50b2tlbiA9IHRva2VuO1xuICAgICAgICAgICAgICAgIHRocm93IGVycjtcbiAgICAgICAgICAgIH1cblxuICAgICAgICAgICAgLy8gbWF5YmUgc2F2ZSBsZXhlciBzdGF0ZVxuICAgICAgICAgICAgaWYgKHRoaXMub3B0aW9ucy5rZWVwSGlzdG9yeSkge1xuICAgICAgICAgICAgICBjb2x1bW4ubGV4ZXJTdGF0ZSA9IGxleGVyLnNhdmUoKVxuICAgICAgICAgICAgfVxuXG4gICAgICAgICAgICB0aGlzLmN1cnJlbnQrKztcbiAgICAgICAgfVxuICAgICAgICBpZiAoY29sdW1uKSB7XG4gICAgICAgICAgdGhpcy5sZXhlclN0YXRlID0gbGV4ZXIuc2F2ZSgpXG4gICAgICAgIH1cblxuICAgICAgICAvLyBJbmNyZW1lbnRhbGx5IGtlZXAgdHJhY2sgb2YgcmVzdWx0c1xuICAgICAgICB0aGlzLnJlc3VsdHMgPSB0aGlzLmZpbmlzaCgpO1xuXG4gICAgICAgIC8vIEFsbG93IGNoYWluaW5nLCBmb3Igd2hhdGV2ZXIgaXQncyB3b3J0aFxuICAgICAgICByZXR1cm4gdGhpcztcbiAgICB9O1xuXG4gICAgUGFyc2VyLnByb3RvdHlwZS5zYXZlID0gZnVuY3Rpb24oKSB7XG4gICAgICAgIHZhciBjb2x1bW4gPSB0aGlzLnRhYmxlW3RoaXMuY3VycmVudF07XG4gICAgICAgIGNvbHVtbi5sZXhlclN0YXRlID0gdGhpcy5sZXhlclN0YXRlO1xuICAgICAgICByZXR1cm4gY29sdW1uO1xuICAgIH07XG5cbiAgICBQYXJzZXIucHJvdG90eXBlLnJlc3RvcmUgPSBmdW5jdGlvbihjb2x1bW4pIHtcbiAgICAgICAgdmFyIGluZGV4ID0gY29sdW1uLmluZGV4O1xuICAgICAgICB0aGlzLmN1cnJlbnQgPSBpbmRleDtcbiAgICAgICAgdGhpcy50YWJsZVtpbmRleF0gPSBjb2x1bW47XG4gICAgICAgIHRoaXMudGFibGUuc3BsaWNlKGluZGV4ICsgMSk7XG4gICAgICAgIHRoaXMubGV4ZXJTdGF0ZSA9IGNvbHVtbi5sZXhlclN0YXRlO1xuXG4gICAgICAgIC8vIEluY3JlbWVudGFsbHkga2VlcCB0cmFjayBvZiByZXN1bHRzXG4gICAgICAgIHRoaXMucmVzdWx0cyA9IHRoaXMuZmluaXNoKCk7XG4gICAgfTtcblxuICAgIC8vIG5iLiBkZXByZWNhdGVkOiB1c2Ugc2F2ZS9yZXN0b3JlIGluc3RlYWQhXG4gICAgUGFyc2VyLnByb3RvdHlwZS5yZXdpbmQgPSBmdW5jdGlvbihpbmRleCkge1xuICAgICAgICBpZiAoIXRoaXMub3B0aW9ucy5rZWVwSGlzdG9yeSkge1xuICAgICAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdzZXQgb3B0aW9uIGBrZWVwSGlzdG9yeWAgdG8gZW5hYmxlIHJld2luZGluZycpXG4gICAgICAgIH1cbiAgICAgICAgLy8gbmIuIHJlY2FsbCBjb2x1bW4gKHRhYmxlKSBpbmRpY2llcyBmYWxsIGJldHdlZW4gdG9rZW4gaW5kaWNpZXMuXG4gICAgICAgIC8vICAgICAgICBjb2wgMCAgIC0tICAgdG9rZW4gMCAgIC0tICAgY29sIDFcbiAgICAgICAgdGhpcy5yZXN0b3JlKHRoaXMudGFibGVbaW5kZXhdKTtcbiAgICB9O1xuXG4gICAgUGFyc2VyLnByb3RvdHlwZS5maW5pc2ggPSBmdW5jdGlvbigpIHtcbiAgICAgICAgLy8gUmV0dXJuIHRoZSBwb3NzaWJsZSBwYXJzaW5nc1xuICAgICAgICB2YXIgY29uc2lkZXJhdGlvbnMgPSBbXTtcbiAgICAgICAgdmFyIHN0YXJ0ID0gdGhpcy5ncmFtbWFyLnN0YXJ0O1xuICAgICAgICB2YXIgY29sdW1uID0gdGhpcy50YWJsZVt0aGlzLnRhYmxlLmxlbmd0aCAtIDFdXG4gICAgICAgIGNvbHVtbi5zdGF0ZXMuZm9yRWFjaChmdW5jdGlvbiAodCkge1xuICAgICAgICAgICAgaWYgKHQucnVsZS5uYW1lID09PSBzdGFydFxuICAgICAgICAgICAgICAgICAgICAmJiB0LmRvdCA9PT0gdC5ydWxlLnN5bWJvbHMubGVuZ3RoXG4gICAgICAgICAgICAgICAgICAgICYmIHQucmVmZXJlbmNlID09PSAwXG4gICAgICAgICAgICAgICAgICAgICYmIHQuZGF0YSAhPT0gUGFyc2VyLmZhaWwpIHtcbiAgICAgICAgICAgICAgICBjb25zaWRlcmF0aW9ucy5wdXNoKHQpO1xuICAgICAgICAgICAgfVxuICAgICAgICB9KTtcbiAgICAgICAgcmV0dXJuIGNvbnNpZGVyYXRpb25zLm1hcChmdW5jdGlvbihjKSB7cmV0dXJuIGMuZGF0YTsgfSk7XG4gICAgfTtcblxuICAgIHJldHVybiB7XG4gICAgICAgIFBhcnNlcjogUGFyc2VyLFxuICAgICAgICBHcmFtbWFyOiBHcmFtbWFyLFxuICAgICAgICBSdWxlOiBSdWxlLFxuICAgIH07XG5cbn0pKTtcbiIsImltcG9ydCB7IENsYXNzLCBhYnN1cmQsIGNsYXNzTmFtZSB9IGZyb20gXCIuL3V0aWwvQ29yZVwiXG5pbXBvcnQgeyBBbm5vdGF0aW9uLCBhbm4gfSBmcm9tIFwiLi91dGlsL0xhdHRpY2VcIlxuaW1wb3J0IHsgSWQsIE51bSwgUGVyc2lzdGVudCwgU3RyLCBWYWx1ZSwgVmFsdWVUYWcsIF8gfSBmcm9tIFwiLi9WYWx1ZVwiXG5pbXBvcnQgeyDOvSwgYXQgfSBmcm9tIFwiLi9WZXJzaW9uZWRcIlxuXG4vLyBGb3IgdHJhaXQgaWRpb20gc2VlIGh0dHBzOi8vd3d3LmJyeW50dW0uY29tL2Jsb2cvdGhlLW1peGluLXBhdHRlcm4taW4tdHlwZXNjcmlwdC1hbGwteW91LW5lZWQtdG8ta25vdy8gYW5kXG4vLyBodHRwczovL2dpdGh1Yi5jb20vTWljcm9zb2Z0L1R5cGVTY3JpcHQvaXNzdWVzLzIxNzEwLlxuZXhwb3J0IGZ1bmN0aW9uIEFubm90YXRlZEM8VCBleHRlbmRzIENsYXNzPFZhbHVlPj4gKEM6IFQpIHtcbiAgIC8vIGh0dHBzOi8vc3RhY2tvdmVyZmxvdy5jb20vcXVlc3Rpb25zLzMzNjA1Nzc1XG4gICByZXR1cm4ge1xuICAgICAgW0MubmFtZV06IGNsYXNzIGV4dGVuZHMgQyB7XG4gICAgICAgICAgICBfX86xOiBBbm5vdGF0aW9uID0gX1xuICAgICAgICAgfVxuICAgfVtDLm5hbWVdIC8vIGdpdmUgdmVyc2lvbmVkIGNsYXNzIHNhbWUgbmFtZSBhcyBDXG59XG5cbi8vIE5vdCBzdXJlIGhvdyB0byBhdm9pZCBkdXBsaWNhdGluZyB0aGUgYm9keSB3aXRoIHRob3NlIG9mIHRoZSBjbGFzc2VzIHJldHVybmVkIGJ5IEFubm90YXRlZEMuXG5leHBvcnQgaW50ZXJmYWNlIEFubm90YXRlZF8ge1xuICAgX1/OsTogQW5ub3RhdGlvblxufVxuXG5leHBvcnQgdHlwZSBBbm5vdGF0ZWQ8VD4gPSBBbm5vdGF0ZWRfICYgVFxuXG5leHBvcnQgZnVuY3Rpb24gYW5ub3RhdGVkPFQgZXh0ZW5kcyBPYmplY3Q+ICh2OiBUKTogdiBpcyBBbm5vdGF0ZWQ8VD4ge1xuICAgcmV0dXJuIHYuaGFzT3duUHJvcGVydHkoXCJfX86xXCIpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBhc0Fubm90YXRlZDxUPiAodjogVCk6IEFubm90YXRlZDxUPiB7XG4gICBpZiAoYW5ub3RhdGVkKHYpKSB7XG4gICAgICByZXR1cm4gdlxuICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBhYnN1cmQoYE5vdCBhbiBhbm5vdGF0ZWQgdmFsdWU6ICR7Y2xhc3NOYW1lKHYpfWApXG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBzZXTOsTxULCBVIGV4dGVuZHMgQW5ub3RhdGVkPFQ+PiAozrE6IEFubm90YXRpb24sIHY6IFUpOiBVIHtcbiAgIHYuX1/OsSA9IM6xXG4gICByZXR1cm4gdlxufVxuXG5leHBvcnQgZnVuY3Rpb24gc2V0YWxszrE8VGFnIGV4dGVuZHMgVmFsdWVUYWcsIFQgZXh0ZW5kcyBWYWx1ZTxUYWc+PiAozrE6IEFubm90YXRpb24sIHY6IFQpOiBUIHtcbiAgIGlmIChhbm5vdGF0ZWQodikpIHtcbiAgICAgIHNldM6xKM6xLCB2KVxuICAgfVxuICAgdi5maWVsZFZhbHVlcygpLmZvckVhY2goKHY6IFBlcnNpc3RlbnQpOiB2b2lkID0+IHtcbiAgICAgIGlmICh2IGluc3RhbmNlb2YgVmFsdWUpIHtcbiAgICAgICAgIHNldGFsbM6xKM6xLCB2KVxuICAgICAgfVxuICAgfSlcbiAgIHJldHVybiB2XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBuZWdhdGVhbGzOsTxUYWcgZXh0ZW5kcyBWYWx1ZVRhZywgVCBleHRlbmRzIFZhbHVlPFRhZz4+ICh2OiBUKTogVCB7XG4gICBpZiAoYW5ub3RhdGVkKHYpKSB7XG4gICAgICBzZXTOsShhbm4ubmVnYXRlKHYuX1/OsSksIHYpXG4gICB9XG4gICB2LmZpZWxkVmFsdWVzKCkuZm9yRWFjaCgodjogUGVyc2lzdGVudCk6IHZvaWQgPT4ge1xuICAgICAgaWYgKHYgaW5zdGFuY2VvZiBWYWx1ZSkge1xuICAgICAgICAgbmVnYXRlYWxszrEodilcbiAgICAgIH1cbiAgIH0pXG4gICByZXR1cm4gdlxufVxuXG5leHBvcnQgZnVuY3Rpb24gam9pbs6xPFQsIFUgZXh0ZW5kcyBBbm5vdGF0ZWQ8VD4+ICjOsTogQW5ub3RhdGlvbiwgdjogVSk6IFUge1xuICAgdi5fX86xID0gYW5uLmpvaW4ozrEsIHYuX1/OsSlcbiAgIHJldHVybiB2XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtZWV0zrE8VCwgVSBleHRlbmRzIEFubm90YXRlZDxUPj4gKM6xOiBBbm5vdGF0aW9uLCB2OiBVKTogVSB7XG4gICB2Ll9fzrEgPSBhbm4ubWVldCjOsSwgdi5fX86xKVxuICAgcmV0dXJuIHZcbn1cblxuLy8gTWFrZSBhbiBhbm5vdGF0ZWQgbm9kZSwgZm9yIGEgY2xhc3MgdGhhdCBkb2Vzbid0IGFscmVhZHkgc3BlY2lmeSBzdGF0aWNhbGx5IHRoYXQgaXRzIGluc3RhbmNlcyBhcmUgYW5ub3RhdGVkLlxuZXhwb3J0IGZ1bmN0aW9uIGFubm90YXRlZEF0PFQgZXh0ZW5kcyBWYWx1ZT4gKGs6IElkLCBDOiBDbGFzczxUPiwgLi4udsyFOiBQZXJzaXN0ZW50W10pOiBBbm5vdGF0ZWQ8VD4ge1xuICAgY29uc3QgdjogVCA9IGF0KGssIEMsIC4uLnbMhSk7XG4gICAodiBhcyBhbnkpLl9fzrEgPSBfXG4gICByZXR1cm4gdiBhcyBBbm5vdGF0ZWQ8VD5cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIG51bSAodmFsOiBudW1iZXIpOiBBbm5vdGF0ZWQ8TnVtPiB7XG4gICByZXR1cm4gYW5ub3RhdGVkQXQozr0oKSwgTnVtLCB2YWwpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBzdHIgKHZhbDogc3RyaW5nKTogQW5ub3RhdGVkPFN0cj4ge1xuICAgcmV0dXJuIGFubm90YXRlZEF0KM69KCksIFN0ciwgdmFsKVxufVxuIiwiaW1wb3J0IHsgYWJzdXJkIH0gZnJvbSBcIi4vdXRpbC9Db3JlXCJcbmltcG9ydCB7IEFubm90YXRlZCwgYW5ub3RhdGVkQXQgfSBmcm9tIFwiLi9Bbm5vdGF0ZWRcIlxuaW1wb3J0IHsgaW5pdERhdGFUeXBlIH0gZnJvbSBcIi4vRGF0YVR5cGVcIlxuaW1wb3J0IHsgRGF0YVZhbHVlIH0gZnJvbSBcIi4vRGF0YVZhbHVlXCJcbmltcG9ydCB7IFBlcnNpc3RlbnQsIF8sIG1ha2UgfSBmcm9tIFwiLi9WYWx1ZVwiXG5pbXBvcnQgeyDOvSB9IGZyb20gXCIuL1ZlcnNpb25lZFwiXG5cbi8vIFNlZSBFbnYgZm9yIGNvbnZlbnRpb24gcmVnYXJkaW5nIGluc3RhbmNlIG1lbWJlcnMgb24gcmVmbGVjdGVkIGRhdGF0eXBlcy5cblxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIEJvb2wgZXh0ZW5kcyBEYXRhVmFsdWU8XCJCb29sXCI+IHtcbn1cblxuZXhwb3J0IGNsYXNzIFRydWUgZXh0ZW5kcyBCb29sIHtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHRydWVfICgpOiBBbm5vdGF0ZWQ8Qm9vbD4ge1xuICAgcmV0dXJuIGFubm90YXRlZEF0KM69KCksIFRydWUpXG59XG5cbmV4cG9ydCBjbGFzcyBGYWxzZSBleHRlbmRzIEJvb2wge1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZmFsc2VfICgpOiBBbm5vdGF0ZWQ8Qm9vbD4ge1xuICAgcmV0dXJuIGFubm90YXRlZEF0KM69KCksIEZhbHNlKVxufVxuXG5leHBvcnQgYWJzdHJhY3QgY2xhc3MgTGlzdDxUPiBleHRlbmRzIERhdGFWYWx1ZTxcIkxpc3RcIj4ge1xuICAgc3RhdGljIGZyb21BcnJheTxUIGV4dGVuZHMgUGVyc2lzdGVudD4gKHjMhTogVFtdKTogTGlzdDxUPiB7XG4gICAgICBsZXQgeMyFyrk6IExpc3Q8VD4gPSBuaWwoKVxuICAgICAgZm9yIChsZXQgbjogbnVtYmVyID0geMyFLmxlbmd0aCAtIDE7IG4gPj0gMDsgLS1uKSB7XG4gICAgICAgICB4zIXKuSA9IGNvbnMoeMyFW25dLCB4zIXKuSlcbiAgICAgIH1cbiAgICAgIHJldHVybiB4zIXKuVxuICAgfVxuXG4gICB0b0FycmF5ICgpOiBUW10ge1xuICAgICAgY29uc3QgeMyFOiBUW10gPSBbXVxuICAgICAgdGhpcy50b0FycmF5Xyh4zIUpXG4gICAgICByZXR1cm4geMyFXG4gICB9XG5cbiAgIHRvQXJyYXlfICh4zIU6IFRbXSk6IHZvaWQge1xuICAgICAgaWYgKENvbnMuaXModGhpcykpIHtcbiAgICAgICAgIHjMhS5wdXNoKHRoaXMuaGVhZClcbiAgICAgICAgIHRoaXMudGFpbC50b0FycmF5Xyh4zIUpXG4gICAgICB9IGVsc2VcbiAgICAgIGlmIChOaWwuaXModGhpcykpIHtcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICByZXR1cm4gYWJzdXJkKClcbiAgICAgIH1cbiAgIH1cbn1cblxuZXhwb3J0IGNsYXNzIE5pbDxUPiBleHRlbmRzIExpc3Q8VD4ge1xuICAgc3RhdGljIGlzPFQ+ICh4czogTGlzdDxUPik6IHhzIGlzIE5pbDxUPiB7XG4gICAgICByZXR1cm4geHMgaW5zdGFuY2VvZiBOaWxcbiAgIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIG5pbDxUPiAoKTogTGlzdDxUPiB7XG4gICByZXR1cm4gbWFrZShOaWwpIGFzIE5pbDxUPlxufVxuXG5leHBvcnQgY2xhc3MgQ29uczxUPiBleHRlbmRzIExpc3Q8VD4ge1xuICAgaGVhZDogVCA9IF9cbiAgIHRhaWw6IExpc3Q8VD4gPSBfXG5cbiAgIHN0YXRpYyBpczxUPiAoeHM6IExpc3Q8VD4pOiB4cyBpcyBDb25zPFQ+IHtcbiAgICAgIHJldHVybiB4cyBpbnN0YW5jZW9mIENvbnNcbiAgIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGNvbnM8VCBleHRlbmRzIFBlcnNpc3RlbnQ+IChoZWFkOiBULCB0YWlsOiBMaXN0PFQ+KTogQ29uczxUPiB7XG4gICByZXR1cm4gbWFrZShDb25zLCBoZWFkLCB0YWlsKSBhcyBDb25zPFQ+XG59XG5cbmV4cG9ydCBjbGFzcyBQYWlyPFQsIFU+IGV4dGVuZHMgRGF0YVZhbHVlPFwiUGFpclwiPiB7XG4gICBmc3Q6IFQgPSBfXG4gICBzbmQ6IFUgPSBfXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBwYWlyPFQgZXh0ZW5kcyBQZXJzaXN0ZW50LCBVIGV4dGVuZHMgUGVyc2lzdGVudD4gKGZzdDogVCwgc25kOiBVKTogUGFpcjxULCBVPiB7XG4gICByZXR1cm4gbWFrZShQYWlyLCBmc3QsIHNuZCkgYXMgUGFpcjxULCBVPlxufVxuXG5leHBvcnQgYWJzdHJhY3QgY2xhc3MgVHJlZTxUIGV4dGVuZHMgUGVyc2lzdGVudD4gZXh0ZW5kcyBEYXRhVmFsdWU8XCJUcmVlXCI+IHtcbiAgIHRvQXJyYXkgKCk6IFRbXSB7XG4gICAgICBjb25zdCB4zIU6IFRbXSA9IFtdXG4gICAgICB0aGlzLnRvQXJyYXlfKHjMhSlcbiAgICAgIHJldHVybiB4zIVcbiAgIH1cblxuICAgdG9BcnJheV8gKHjMhTogVFtdKTogdm9pZCB7XG4gICAgICBpZiAoTm9uRW1wdHkuaXModGhpcykpIHtcbiAgICAgICAgIHRoaXMubGVmdC50b0FycmF5Xyh4zIUpXG4gICAgICAgICB4zIUucHVzaCh0aGlzLnQpXG4gICAgICAgICB0aGlzLnJpZ2h0LnRvQXJyYXlfKHjMhSlcbiAgICAgIH0gZWxzZSBcbiAgICAgIGlmIChFbXB0eS5pcyh0aGlzKSkge1xuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHJldHVybiBhYnN1cmQoKVxuICAgICAgfVxuICAgfVxufVxuXG5leHBvcnQgY2xhc3MgRW1wdHk8VCBleHRlbmRzIFBlcnNpc3RlbnQ+IGV4dGVuZHMgVHJlZTxUPiB7XG4gICBzdGF0aWMgaXM8VCBleHRlbmRzIFBlcnNpc3RlbnQ+ICh0OiBUcmVlPFQ+KTogdCBpcyBFbXB0eTxUPiB7XG4gICAgICByZXR1cm4gdCBpbnN0YW5jZW9mIEVtcHR5XG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBlbXB0eTxUIGV4dGVuZHMgUGVyc2lzdGVudD4gKCk6IEVtcHR5PFQ+IHtcbiAgIHJldHVybiBtYWtlKEVtcHR5KSBhcyBFbXB0eTxUPlxufVxuXG5leHBvcnQgY2xhc3MgTm9uRW1wdHk8VCBleHRlbmRzIFBlcnNpc3RlbnQ+IGV4dGVuZHMgVHJlZTxUPiB7XG4gICBsZWZ0OiBUcmVlPFQ+ID0gX1xuICAgdDogVCA9IF9cbiAgIHJpZ2h0OiBUcmVlPFQ+ID0gX1xuXG4gICBzdGF0aWMgaXM8VCBleHRlbmRzIFBlcnNpc3RlbnQ+ICh0OiBUcmVlPFQ+KTogdCBpcyBOb25FbXB0eTxUPiB7XG4gICAgICByZXR1cm4gdCBpbnN0YW5jZW9mIE5vbkVtcHR5XG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBub25FbXB0eSA8VCBleHRlbmRzIFBlcnNpc3RlbnQ+IChsZWZ0OiBUcmVlPFQ+LCB0OiBULCByaWdodDogVHJlZTxUPik6IE5vbkVtcHR5PFQ+IHtcbiAgIHJldHVybiBtYWtlKE5vbkVtcHR5LCBsZWZ0LCB0LCByaWdodCkgYXMgTm9uRW1wdHk8VD5cbn1cblxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIE9wdGlvbjxUIGV4dGVuZHMgUGVyc2lzdGVudD4gZXh0ZW5kcyBEYXRhVmFsdWU8XCJPcHRpb25cIj4ge1xufVxuXG5leHBvcnQgY2xhc3MgTm9uZTxUIGV4dGVuZHMgUGVyc2lzdGVudD4gZXh0ZW5kcyBPcHRpb248VD4ge1xufVxuXG5leHBvcnQgY2xhc3MgU29tZTxUIGV4dGVuZHMgUGVyc2lzdGVudD4gZXh0ZW5kcyBPcHRpb248VD4ge1xuICAgdDogVCA9IF9cbn1cblxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIE9yZGVyaW5nIGV4dGVuZHMgRGF0YVZhbHVlPFwiT3JkZXJpbmdcIj4ge1xufVxuXG5leHBvcnQgY2xhc3MgTFQgZXh0ZW5kcyBPcmRlcmluZyB7XG59XG5cbmV4cG9ydCBjbGFzcyBHVCBleHRlbmRzIE9yZGVyaW5nIHtcbn1cblxuZXhwb3J0IGNsYXNzIEVRIGV4dGVuZHMgT3JkZXJpbmcge1xufVxuXG5pbml0RGF0YVR5cGUoQm9vbCwgW1RydWUsIEZhbHNlXSlcbmluaXREYXRhVHlwZShMaXN0LCBbTmlsLCBDb25zXSlcbmluaXREYXRhVHlwZShPcHRpb24sIFtTb21lLCBOb25lXSlcbmluaXREYXRhVHlwZShPcmRlcmluZywgW0xULCBHVCwgRVFdKVxuaW5pdERhdGFUeXBlKFBhaXIsIFtQYWlyXSlcbmluaXREYXRhVHlwZShUcmVlLCBbRW1wdHksIE5vbkVtcHR5XSlcbiIsImltcG9ydCB7IEFDbGFzcywgQ2xhc3MsIF9fbm9uTnVsbCwgYXNzZXJ0IH0gZnJvbSBcIi4vdXRpbC9Db3JlXCJcbmltcG9ydCB7IHN0ciB9IGZyb20gXCIuL0Fubm90YXRlZFwiXG5pbXBvcnQgeyBEYXRhRXhwbCwgRGF0YVZhbHVlIH0gZnJvbSBcIi4vRGF0YVZhbHVlXCJcbmltcG9ydCB7IERhdGFFbGltIH0gZnJvbSBcIi4vTWF0Y2hcIlxuaW1wb3J0IHsgTnVtLCBQcmltVmFsdWUsIFN0ciwgXywgZmllbGRzIH0gZnJvbSBcIi4vVmFsdWVcIlxuXG5leHBvcnQgY2xhc3MgUHJpbVR5cGUge1xuICAgbmFtZTogU3RyXG4gICBDOiBDbGFzczxQcmltVmFsdWU+XG5cbiAgIGNvbnN0cnVjdG9yIChuYW1lOiBTdHIsIEM6IENsYXNzPFByaW1WYWx1ZT4pIHtcbiAgICAgIHRoaXMubmFtZSA9IG5hbWVcbiAgICAgIHRoaXMuQyA9IENcbiAgIH1cbn1cblxuLy8gTmVpdGhlciBvZiB0aGVzZSBpcyBjdXJyZW50bHkgcmVmbGVjdGl2ZSBiZWNhdXNlIG9mIG5vbi1zdGFuZGFyZCBmaWVsZHMuXG5leHBvcnQgY2xhc3MgRGF0YVR5cGUge1xuICAgbmFtZTogU3RyXG4gICBlbGltQzogQ2xhc3M8RGF0YUVsaW0+ICAgICAgICAgICAgXG4gICBjdHJzOiBNYXA8c3RyaW5nLCBDdHI+ICAgICAgICAgICAgICAgICAvLyBmaWVsZHMgb2YgbXkgY29uc3RydWN0b3JzXG4gICBleHBsQ8yFOiBNYXA8c3RyaW5nLCBDbGFzczxEYXRhRXhwbD4+ICAgIC8vIFwiZXhwbGFuYXRpb25cIiBjbGFzcyBwZXIgY29uc3RydWN0b3JcblxuICAgY29uc3RydWN0b3IgKFxuICAgICAgbmFtZTogU3RyLFxuICAgICAgZWxpbUM6IENsYXNzPERhdGFFbGltPiwgXG4gICAgICBjdHJzOiBNYXA8c3RyaW5nLCBDdHI+LCBcbiAgICAgIGV4cGxDzIU6IE1hcDxzdHJpbmcsIENsYXNzPERhdGFFeHBsPj5cbiAgICkge1xuICAgICAgdGhpcy5uYW1lID0gbmFtZVxuICAgICAgdGhpcy5lbGltQyA9IGVsaW1DXG4gICAgICB0aGlzLmN0cnMgPSBjdHJzXG4gICAgICB0aGlzLmV4cGxDzIUgPSBleHBsQ8yFXG4gICB9XG59XG5cbi8vIENvbnN0cnVjdG9yIG9mIGEgZGF0YXR5cGUsIG5vdCB0byBiZSBjb25mdXNlZCB3aXRoIGFuIGluc3RhbmNlIG9mIHN1Y2ggYSB0aGluZyAoQ29uc3RyKSBvciBuYW1lIG9mIHN1Y2ggYSB0aGluZ1xuLy8gKExleC5DdHIpLiBGaWVsZHMgaGF2ZSBhIHRvdGFsIG9yZGVyaW5nIGdpdmVuIGJ5IHRoZSBvcmRlciBvZiBkZWZpbml0aW9uIGluIHRoZSBjb3JyZXNwb25kaW5nIGNsYXNzLlxuZXhwb3J0IGNsYXNzIEN0ciB7XG4gICBDOiBDbGFzczxEYXRhVmFsdWU+XG4gICBmzIU6IHN0cmluZ1tdXG5cbiAgIGNvbnN0cnVjdG9yIChDOiBDbGFzczxEYXRhVmFsdWU+LCBmzIU6IHN0cmluZ1tdKSB7XG4gICAgICB0aGlzLkMgPSBDXG4gICAgICB0aGlzLmbMhSA9IGbMhVxuICAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gY3RyRm9yIChjdHI6IFN0cik6IEN0ciB7XG4gICByZXR1cm4gY3RyVG9EYXRhVHlwZS5nZXQoY3RyLnZhbCkhLmN0cnMuZ2V0KGN0ci52YWwpIVxufVxuXG5leHBvcnQgZnVuY3Rpb24gYXJpdHkgKGN0cjogU3RyKTogbnVtYmVyIHtcbiAgIGFzc2VydChjdHJUb0RhdGFUeXBlLmhhcyhjdHIudmFsKSwgYE5vIHN1Y2ggY29uc3RydWN0b3I6IFwiJHtjdHIudmFsfVwiLmAsKVxuICAgcmV0dXJuIGN0ckZvcihjdHIpLmbMhS5sZW5ndGhcbn1cblxuLy8gUG9wdWxhdGVkIGJ5IGluaXREYXRhVHlwZXMoKS4gQ29uc3RydWN0b3JzIGFyZSBub3QgeWV0IGZpcnN0LWNsYXNzLlxuZXhwb3J0IGNvbnN0IHR5cGVzOiBNYXA8c3RyaW5nLCBEYXRhVHlwZSB8IFByaW1UeXBlPiA9IG5ldyBNYXBcbmV4cG9ydCBjb25zdCBjdHJUb0RhdGFUeXBlOiBNYXA8c3RyaW5nLCBEYXRhVHlwZT4gPSBuZXcgTWFwXG5leHBvcnQgY29uc3QgZWxpbVRvRGF0YVR5cGU6IE1hcDxzdHJpbmcsIERhdGFUeXBlPiA9IG5ldyBNYXBcbmV4cG9ydCBjb25zdCBlbGltU3VmZml4OiBzdHJpbmcgPSBcIkVsaW1cIlxuZXhwb3J0IGNvbnN0IGV4cGxTdWZmaXg6IHN0cmluZyA9IFwiRXhwbFwiXG5cbi8vIFNlZSBodHRwczovL3N0YWNrb3ZlcmZsb3cuY29tL3F1ZXN0aW9ucy8zMzYwNTc3NSBmb3IgdGhlIGR5bmFtaWMgY2xhc3MtbmFtaW5nIGlkaW9tLlxuZXhwb3J0IGZ1bmN0aW9uIGluaXREYXRhVHlwZTxUIGV4dGVuZHMgRGF0YVZhbHVlPiAoRDogQUNsYXNzPFQ+LCBDzIU6IENsYXNzPFQ+W10pIHtcbiAgIEPMhS5zb3J0KChDLCBDyrkpOiBudW1iZXIgPT4gQy5uYW1lLmxvY2FsZUNvbXBhcmUoQ8q5Lm5hbWUpKSAvLyBjb25zaXN0ZW50IHdpdGggU3RyLmxlcVxuICAgY29uc3QgY3RyczogW3N0cmluZywgQ3RyXVtdID0gQ8yFLm1hcChcbiAgICAgICAgICAgIChDOiBDbGFzczxUPik6IFtzdHJpbmcsIEN0cl0gPT4gW0MubmFtZSwgbmV3IEN0cihDLCBmaWVsZHMobmV3IEMpKV1cbiAgICAgICAgICksXG4gICAgICAgICBlbGltQ19uYW1lOiBzdHJpbmcgPSBELm5hbWUgKyBlbGltU3VmZml4LFxuICAgICAgICAgZWxpbUM6IENsYXNzPERhdGFFbGltPiA9IHtcbiAgICAgICAgICAgIFtlbGltQ19uYW1lXTogY2xhc3MgZXh0ZW5kcyBEYXRhRWxpbSB7XG4gICAgICAgICAgICAgICBjb25zdHJ1Y3RvciAoKSB7XG4gICAgICAgICAgICAgICAgICBzdXBlcigpXG4gICAgICAgICAgICAgICAgICAvLyBsZXhpY29ncmFwaGljYWwgb3JkZXIgaG9wZWZ1bGx5IHByZXNlcnZlZCBieSBnZXRPd25Qcm9wZXJ0eU5hbWVzKClcbiAgICAgICAgICAgICAgICAgIEPMhS5mb3JFYWNoKChDOiBDbGFzczxUPik6IHZvaWQgPT4ge1xuICAgICAgICAgICAgICAgICAgICAgKHRoaXMgYXMgYW55KVtDLm5hbWVdID0gX1xuICAgICAgICAgICAgICAgICAgfSlcbiAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH1cbiAgICAgICAgIH1bZWxpbUNfbmFtZV0sXG4gICAgICAgICBleHBsQ19uYW1lOiBzdHJpbmcgPSBELm5hbWUgKyBleHBsU3VmZml4LFxuICAgICAgICAgZXhwbEPMhTogW3N0cmluZywgQ2xhc3M8RGF0YUV4cGw+XVtdID0gY3Rycy5tYXAoKFtjyrksIGNdOiBbc3RyaW5nLCBDdHJdKSA9PiB7XG4gICAgICAgICAgICByZXR1cm4gW2PKuSwge1xuICAgICAgICAgICAgICAgW2V4cGxDX25hbWVdOiBjbGFzcyBleHRlbmRzIERhdGFFeHBsIHtcbiAgICAgICAgICAgICAgICAgIGNvbnN0cnVjdG9yICgpIHtcbiAgICAgICAgICAgICAgICAgICAgIHN1cGVyKClcbiAgICAgICAgICAgICAgICAgICAgIGMuZsyFLmZvckVhY2goKGY6IHN0cmluZyk6IHZvaWQgPT4ge1xuICAgICAgICAgICAgICAgICAgICAgICAgKHRoaXMgYXMgYW55KVtmXSA9IF9cbiAgICAgICAgICAgICAgICAgICAgIH0pXG4gICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICB9XG4gICAgICAgICAgICB9W2V4cGxDX25hbWVdXVxuICAgICAgICAgfSksXG4gICAgICAgICBkOiBEYXRhVHlwZSA9IG5ldyBEYXRhVHlwZShzdHIoRC5uYW1lKSwgZWxpbUMsIG5ldyBNYXAoY3RycyksIG5ldyBNYXAoZXhwbEPMhSkpXG4gICBDzIUuZm9yRWFjaCgoQzogQ2xhc3M8VD4pOiB2b2lkID0+IHtcbiAgICAgIGN0clRvRGF0YVR5cGUuc2V0KEMubmFtZSwgZClcbiAgIH0pXG4gICBlbGltVG9EYXRhVHlwZS5zZXQoZWxpbUNfbmFtZSwgZClcbiAgIHR5cGVzLnNldChkLm5hbWUudmFsLCBkKVxufVxuXG50eXBlcy5zZXQoTnVtLm5hbWUsIG5ldyBQcmltVHlwZShzdHIoTnVtLm5hbWUpLCBOdW0pKVxudHlwZXMuc2V0KFN0ci5uYW1lLCBuZXcgUHJpbVR5cGUoc3RyKFN0ci5uYW1lKSwgU3RyKSlcbiIsImltcG9ydCB7IHppcCB9IGZyb20gXCIuL3V0aWwvQXJyYXlcIlxuaW1wb3J0IHsgQW5ub3RhdGVkIH0gZnJvbSBcIi4vQW5ub3RhdGVkXCJcbmltcG9ydCB7IEV4cGwgfSBmcm9tIFwiLi9FeHBsVmFsdWVcIlxuaW1wb3J0IHsgRGF0YVZhbHVlVGFnLCBTdGF0ZSwgVmFsdWUsIGZpZWxkcyB9IGZyb20gXCIuL1ZhbHVlXCJcblxuLy8gVmFsdWUgb2YgYSBkYXRhdHlwZSBjb25zdHJ1Y3RvcjsgZmllbGRzIGFyZSBhbHdheXMgdXNlci1sZXZlbCB2YWx1ZXMgKGkuZS4gbm90IEVTNiBwcmltaXRpdmVzKS5cbmV4cG9ydCBjbGFzcyBEYXRhVmFsdWU8VGFnIGV4dGVuZHMgRGF0YVZhbHVlVGFnID0gRGF0YVZhbHVlVGFnPiBleHRlbmRzIFZhbHVlPFRhZz4ge1xuICAgX19leHBsOiBEYXRhRXhwbFxuXG4gICBmaWVsZFZhbHVlcyAoKTogVmFsdWVbXSB7XG4gICAgICByZXR1cm4gZmllbGRzKHRoaXMpLm1hcChrID0+ICh0aGlzIGFzIGFueSBhcyBTdGF0ZSlba10gYXMgVmFsdWUpXG4gICB9XG5cbiAgIGZpZWxkRXhwbFZhbHVlcygpOiBbRXhwbCwgQW5ub3RhdGVkPFZhbHVlPl1bXSB7XG4gICAgICByZXR1cm4gemlwKHRoaXMuX19leHBsLmZpZWxkVmFsdWVzKCksIHRoaXMuZmllbGRWYWx1ZXMoKSBhcyBBbm5vdGF0ZWQ8VmFsdWU+W10pXG4gICB9XG59XG5cbmV4cG9ydCBjbGFzcyBEYXRhRXhwbCBleHRlbmRzIERhdGFWYWx1ZTxcIkRhdGFFeHBsXCI+IHtcbiAgIGZpZWxkVmFsdWVzICgpOiBFeHBsW10ge1xuICAgICAgcmV0dXJuIGZpZWxkcyh0aGlzKS5tYXAoayA9PiAodGhpcyBhcyBhbnkgYXMgU3RhdGUpW2tdIGFzIEV4cGwpXG4gICB9XG59XG4iLCJpbXBvcnQgeyBhYnN1cmQgfSBmcm9tIFwiLi91dGlsL0NvcmVcIlxuaW1wb3J0IHsgQW5ub3RhdGVkIH0gZnJvbSBcIi4vQW5ub3RhdGVkXCJcbmltcG9ydCB7IERhdGFWYWx1ZSB9IGZyb20gXCIuL0RhdGFWYWx1ZVwiXG5pbXBvcnQgeyBTdHIsIFZhbHVlLCBfLCBtYWtlIH0gZnJvbSBcIi4vVmFsdWVcIlxuXG4vLyBJZGlvbSBpcyB0byBwZXJtaXQgaW5zdGFuY2UgbWV0aG9kcyBvbiByZWZsZWN0ZWQgZGF0YXR5cGVzLCBidXQgbm90IGhhdmUgdGhlbSB1c2UgcG9seW1vcnBoaXNtLlxuXG4vLyBFbnZpcm9ubWVudHMgYXJlIHNub2MgbGlzdHMuXG5leHBvcnQgYWJzdHJhY3QgY2xhc3MgRW52IGV4dGVuZHMgRGF0YVZhbHVlPFwiRW52XCI+IHtcbiAgIGdldCAoazogQW5ub3RhdGVkPFN0cj4pOiBBbm5vdGF0ZWQ8VmFsdWU+IHwgdW5kZWZpbmVkIHtcbiAgICAgIGlmICh0aGlzIGluc3RhbmNlb2YgRW1wdHlFbnYpIHtcbiAgICAgICAgIHJldHVybiB1bmRlZmluZWRcbiAgICAgIH0gZWxzZVxuICAgICAgaWYgKHRoaXMgaW5zdGFuY2VvZiBFeHRlbmRFbnYpIHtcbiAgICAgICAgIGlmICh0aGlzLmsudmFsID09PSBrLnZhbCkge1xuICAgICAgICAgICAgcmV0dXJuIHRoaXMudlxuICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHJldHVybiB0aGlzLs+BLmdldChrKVxuICAgICAgICAgfVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHJldHVybiBhYnN1cmQoKVxuICAgICAgfVxuICAgfVxuICAgXG4gICBoYXMgKGs6IEFubm90YXRlZDxTdHI+KTogYm9vbGVhbiB7XG4gICAgICByZXR1cm4gdGhpcy5nZXQoaykgIT09IHVuZGVmaW5lZFxuICAgfVxuXG4gICBzdGF0aWMgc2luZ2xldG9uIChrOiBBbm5vdGF0ZWQ8U3RyPiwgdjogQW5ub3RhdGVkPFZhbHVlPik6IEV4dGVuZEVudiB7XG4gICAgICByZXR1cm4gZXh0ZW5kRW52KGVtcHR5RW52KCksIGssIHYpXG4gICB9XG4gICBcbiAgIGNvbmNhdCAoz4E6IEVudik6IEVudiB7XG4gICAgICBpZiAoz4EgaW5zdGFuY2VvZiBFbXB0eUVudikge1xuICAgICAgICAgcmV0dXJuIHRoaXNcbiAgICAgIH0gZWxzZVxuICAgICAgaWYgKM+BIGluc3RhbmNlb2YgRXh0ZW5kRW52KSB7XG4gICAgICAgICByZXR1cm4gZXh0ZW5kRW52KHRoaXMuY29uY2F0KM+BLs+BKSwgz4Euaywgz4EudilcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICByZXR1cm4gYWJzdXJkKClcbiAgICAgIH1cbiAgIH1cbn1cblxuZXhwb3J0IGNsYXNzIEVtcHR5RW52IGV4dGVuZHMgRW52IHtcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGVtcHR5RW52ICgpOiBFbXB0eUVudiB7XG4gICByZXR1cm4gbWFrZShFbXB0eUVudilcbn1cblxuZXhwb3J0IGNsYXNzIEV4dGVuZEVudiBleHRlbmRzIEVudiB7XG4gICDPgTogRW52ID0gX1xuICAgazogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICB2OiBBbm5vdGF0ZWQ8VmFsdWU+ID0gX1xufVxuXG5leHBvcnQgZnVuY3Rpb24gZXh0ZW5kRW52ICjPgTogRW52LCBrOiBBbm5vdGF0ZWQ8U3RyPiwgdjogQW5ub3RhdGVkPFZhbHVlPik6IEV4dGVuZEVudiB7XG4gICByZXR1cm4gbWFrZShFeHRlbmRFbnYsIM+BLCBrLCB2KVxufVxuIiwiaW1wb3J0IHsgemlwIH0gZnJvbSBcIi4vdXRpbC9BcnJheVwiXG5pbXBvcnQgeyBfX25vbk51bGwsIGFic3VyZCwgYXMsIGFzc2VydCwgY2xhc3NOYW1lLCBlcnJvciB9IGZyb20gXCIuL3V0aWwvQ29yZVwiXG5pbXBvcnQgeyBhbm4gfSBmcm9tIFwiLi91dGlsL0xhdHRpY2VcIlxuaW1wb3J0IHsgQW5ub3RhdGVkLCBBbm5vdGF0ZWRDLCBhbm5vdGF0ZWRBdCwgam9pbs6xLCBtZWV0zrEsIG51bSwgc2V0zrEsIHN0ciB9IGZyb20gXCIuL0Fubm90YXRlZFwiXG5pbXBvcnQgeyBDb25zLCBMaXN0LCBOaWwsIGNvbnMsIG5pbCB9IGZyb20gXCIuL0Jhc2VUeXBlc1wiXG5pbXBvcnQgeyBEYXRhVHlwZSwgUHJpbVR5cGUsIGN0clRvRGF0YVR5cGUsIGluaXREYXRhVHlwZSwgdHlwZXMgfSBmcm9tIFwiLi9EYXRhVHlwZVwiXG5pbXBvcnQgeyBEYXRhVmFsdWUgfSBmcm9tIFwiLi9EYXRhVmFsdWVcIlxuaW1wb3J0IHsgRW52LCBlbXB0eUVudiwgZXh0ZW5kRW52IH0gZnJvbSBcIi4vRW52XCJcbmltcG9ydCB7IEV4cGwsIEV4cGxWYWx1ZSwgZXhwbFZhbHVlIH0gZnJvbSBcIi4vRXhwbFZhbHVlXCJcbmltcG9ydCB7IEV4cHIgfSBmcm9tIFwiLi9FeHByXCJcbmltcG9ydCB7IGdldCB9IGZyb20gXCIuL0Zpbml0ZU1hcFwiXG5pbXBvcnQgeyBFbGltLCBNYXRjaCwgZXZhbFRyaWUsIG1hdGNoX2J3ZCwgbWF0Y2hfZndkIH0gZnJvbSBcIi4vTWF0Y2hcIlxuaW1wb3J0IHsgVW5hcnlPcCwgQmluYXJ5T3AsIGJpbmFyeU9wcywgdW5hcnlPcHMgfSBmcm9tIFwiLi9QcmltaXRpdmVcIlxuaW1wb3J0IHsgSWQsIE51bSwgU3RyLCBWYWx1ZSwgXywgbWFrZSB9IGZyb20gXCIuL1ZhbHVlXCJcbmltcG9ydCB7IM69LCBhdCwgY29weUF0IH0gZnJvbSBcIi4vVmVyc2lvbmVkXCJcblxuZXhwb3J0IGVudW0gRGlyZWN0aW9uIHsgRndkLCBCd2QgfVxudHlwZSBEZWYgPSBFeHByLkRlZlxudHlwZSBSZWNEZWYgPSBFeHByLlJlY0RlZlxuXG5leHBvcnQgbW9kdWxlIEV2YWwge1xuXG4vLyDPgSBwbHVzIGJpbmRpbmdzIGluIM60IGFyZSBjbG9zaW5nIGZvciBmLlxuZXhwb3J0IGNsYXNzIENsb3N1cmUgZXh0ZW5kcyBBbm5vdGF0ZWRDKERhdGFWYWx1ZSk8XCJDbG9zdXJlXCI+IHtcbiAgIM+BOiBFbnYgPSBfIFxuICAgzrQ6IExpc3Q8UmVjRGVmPiA9IF9cbiAgIGY6IEVsaW08RXhwcj4gPSBfXG59XG5cbmZ1bmN0aW9uIGNsb3N1cmUgKGs6IElkLCDPgTogRW52LCDOtDogTGlzdDxSZWNEZWY+LCBmOiBFbGltPEV4cHI+KTogQ2xvc3VyZSB7XG4gICByZXR1cm4gYXQoaywgQ2xvc3VyZSwgz4EsIM60LCBmKVxufVxuXG4vLyBFbnZpcm9ubWVudHMgYXJlIHNub2MtbGlzdHMsIHNvIHRoaXMgKGluY29uc2VxdWVudGlhbGx5KSByZXZlcnNlcyBkZWNsYXJhdGlvbiBvcmRlci5cbmZ1bmN0aW9uIHJlY0RlZnMgKM60XzA6IExpc3Q8UmVjRGVmPiwgz4E6IEVudiwgzrQ6IExpc3Q8UmVjRGVmPik6IFtMaXN0PEV4cGwuUmVjRGVmPiwgRW52XSB7XG4gICBpZiAoQ29ucy5pcyjOtCkpIHtcbiAgICAgIGNvbnN0IGRlZjogUmVjRGVmID0gzrQuaGVhZCxcbiAgICAgICAgICAgIFvOtOKCnCwgz4FfZXh0XTogW0xpc3Q8RXhwbC5SZWNEZWY+LCBFbnZdID0gcmVjRGVmcyjOtF8wLCDPgSwgzrQudGFpbCksXG4gICAgICAgICAgICBmOiBDbG9zdXJlID0gY2xvc3VyZSjOvSgpLCDPgSwgzrRfMCwgZXZhbFRyaWUoZGVmLs+DKSlcbiAgICAgIHJldHVybiBbY29ucyhFeHBsLnJlY0RlZihkZWYueCwgZiksIM604oKcKSwgZXh0ZW5kRW52KM+BX2V4dCwgZGVmLngsIGYpXVxuICAgfSBlbHNlXG4gICBpZiAoTmlsLmlzKM60KSkge1xuICAgICAgcmV0dXJuIFtuaWwoKSwgZW1wdHlFbnYoKV1cbiAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gYWJzdXJkKClcbiAgIH1cbn1cblxuZnVuY3Rpb24gcmVjRGVmc18gKGRpcjogRGlyZWN0aW9uLCDOtDogTGlzdDxFeHBsLlJlY0RlZj4pOiB2b2lkIHtcbiAgIGlmIChDb25zLmlzKM60KSkge1xuICAgICAgemlwKM60LmhlYWQuZi7OtC50b0FycmF5KCksIM60LnRvQXJyYXkoKSkubWFwKChbZGVmLCBkZWbigpxdOiBbUmVjRGVmLCBFeHBsLlJlY0RlZl0pOiB2b2lkID0+IHtcbiAgICAgICAgIGFzc2VydChkZWYueC5lcShkZWbigpwueCkpXG4gICAgICAgICBpZiAoZGlyID09PSBEaXJlY3Rpb24uRndkKSB7XG4gICAgICAgICAgICBzZXTOsShkZWYueC5fX86xLCBkZWbigpwuZilcbiAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBqb2luzrEoZGVm4oKcLmYuX1/OsSwgZGVmLngpXG4gICAgICAgICB9XG4gICAgICB9KVxuICAgfSBlbHNlXG4gICBpZiAoTmlsLmlzKM60KSkge1xuICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBhYnN1cmQoKVxuICAgfVxufVxuXG4vLyBIZXJlIHdlIG11c3RuJ3QgaW52ZXJ0IGRlZmluaXRpb24gb3JkZXIuXG5mdW5jdGlvbiBkZWZzICjPgTogRW52LCBkZWbMhTogTGlzdDxEZWY+LCDPgV9leHQ6IEVudik6IFtMaXN0PEV4cGwuRGVmPiwgRW52XSB7XG4gICBpZiAoQ29ucy5pcyhkZWbMhSkpIHtcbiAgICAgIGNvbnN0IGRlZjogRGVmID0gZGVmzIUuaGVhZFxuICAgICAgaWYgKGRlZiBpbnN0YW5jZW9mIEV4cHIuTGV0KSB7XG4gICAgICAgICBjb25zdCB0djogRXhwbFZhbHVlID0gZXZhbF8oz4EuY29uY2F0KM+BX2V4dCksIGRlZi5lKSxcbiAgICAgICAgICAgICAgIFtkZWbMheKCnCwgz4FfZXh0yrldOiBbTGlzdDxFeHBsLkRlZj4sIEVudl0gPSBkZWZzKM+BLCBkZWbMhS50YWlsLCBleHRlbmRFbnYoz4FfZXh0LCBkZWYueCwgdHYudikpXG4gICAgICAgICByZXR1cm4gW2NvbnMoRXhwbC5sZXRfKGRlZi54LCB0diksIGRlZsyF4oKcKSwgz4FfZXh0yrldXG4gICAgICB9IGVsc2VcbiAgICAgIGlmIChkZWYgaW5zdGFuY2VvZiBFeHByLlByaW0pIHtcbiAgICAgICAgIC8vIGZpcnN0LWNsYXNzIHByaW1pdGl2ZXMgY3VycmVudGx5IGhhcHBlbiB0byBiZSB1bmFyeVxuICAgICAgICAgaWYgKHVuYXJ5T3BzLmhhcyhkZWYueC52YWwpKSB7XG4gICAgICAgICAgICBjb25zdCBvcDogQW5ub3RhdGVkPFVuYXJ5T3A+ID0gdW5hcnlPcHMuZ2V0KGRlZi54LnZhbCkhLFxuICAgICAgICAgICAgICAgICAgW2RlZsyF4oKcLCDPgV9leHTKuV06IFtMaXN0PEV4cGwuRGVmPiwgRW52XSA9IGRlZnMoz4EsIGRlZsyFLnRhaWwsIGV4dGVuZEVudijPgV9leHQsIGRlZi54LCBvcCkpXG4gICAgICAgICAgICByZXR1cm4gW2NvbnMoRXhwbC5wcmltKGRlZi54LCBvcCksIGRlZsyF4oKcKSwgz4FfZXh0yrldXG4gICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgcmV0dXJuIGVycm9yKGBObyBpbXBsZW1lbnRhdGlvbiBmb3VuZCBmb3IgcHJpbWl0aXZlIFwiJHtkZWYueC52YWx9XCIuYClcbiAgICAgICAgIH1cbiAgICAgIH0gZWxzZVxuICAgICAgaWYgKGRlZiBpbnN0YW5jZW9mIEV4cHIuTGV0UmVjKSB7XG4gICAgICAgICBjb25zdCBbzrQsIM+B4bWfXTogW0xpc3Q8RXhwbC5SZWNEZWY+LCBFbnZdID0gcmVjRGVmcyhkZWYuzrQsIM+BLmNvbmNhdCjPgV9leHQpLCBkZWYuzrQpLFxuICAgICAgICAgICAgICAgW2RlZsyF4oKcLCDPgV9leHTKuV06IFtMaXN0PEV4cGwuRGVmPiwgRW52XSA9IGRlZnMoz4EsIGRlZsyFLnRhaWwsIM+BX2V4dC5jb25jYXQoz4HhtZ8pKVxuICAgICAgICAgcmV0dXJuIFtjb25zKEV4cGwubGV0UmVjKM60KSwgZGVmzIXigpwpLCDPgV9leHTKuV1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICByZXR1cm4gYWJzdXJkKClcbiAgICAgIH1cbiAgIH0gZWxzZVxuICAgaWYgKE5pbC5pcyhkZWbMhSkpIHtcbiAgICAgIHJldHVybiBbbmlsKCksIM+BX2V4dF1cbiAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gYWJzdXJkKClcbiAgIH1cbn1cblxuZnVuY3Rpb24gZGVmc19md2QgKGRlZsyFOiBMaXN0PERlZj4sIGRlZsyF4oKcOiBMaXN0PEV4cGwuRGVmPik6IHZvaWQge1xuICAgemlwKGRlZsyFLnRvQXJyYXkoKSwgZGVmzIXigpwudG9BcnJheSgpKS5mb3JFYWNoKChbZGVmLCBkZWbigpxdOiBbRGVmLCBFeHBsLkRlZl0pID0+IHtcbiAgICAgIGlmIChkZWbigpwgaW5zdGFuY2VvZiBFeHBsLkxldCkgeyAgICAgICAgIFxuICAgICAgICAgZXZhbF9md2QoYXMoZGVmLCBFeHByLkxldCkuZSwgZGVm4oKcLnR2KVxuICAgICAgICAgbWVldM6xKGRlZuKCnC54Ll9fzrEsIGRlZuKCnC50di52KVxuICAgICAgfSBlbHNlXG4gICAgICBpZiAoZGVm4oKcIGluc3RhbmNlb2YgRXhwbC5QcmltKSB7XG4gICAgICAgICBzZXTOsShkZWbigpwueC5fX86xLCBkZWbigpwub3ApXG4gICAgICB9IGVsc2VcbiAgICAgIGlmIChkZWbigpwgaW5zdGFuY2VvZiBFeHBsLkxldFJlYykge1xuICAgICAgICAgcmVjRGVmc18oRGlyZWN0aW9uLkZ3ZCwgZGVm4oKcLs60KVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIGFic3VyZCgpXG4gICAgICB9XG4gICB9KVxufVxuXG5mdW5jdGlvbiBkZWZzX2J3ZCAoZGVmzIU6IExpc3Q8RGVmPiwgZGVmzIXigpw6IExpc3Q8RXhwbC5EZWY+KTogdm9pZCB7XG4gICB6aXAoZGVmzIUudG9BcnJheSgpLCBkZWbMheKCnC50b0FycmF5KCkpLnJldmVyc2UoKS5mb3JFYWNoKChbZGVmLCBkZWbigpxdOiBbRGVmLCBFeHBsLkRlZl0pID0+IHtcbiAgICAgIGlmIChkZWbigpwgaW5zdGFuY2VvZiBFeHBsLkxldCkge1xuICAgICAgICAgam9pbs6xKGRlZuKCnC50di52Ll9fzrEsIGRlZuKCnC54KVxuICAgICAgICAgZXZhbF9id2QoYXMoZGVmLCBFeHByLkxldCkuZSwgZGVm4oKcLnR2KVxuICAgICAgfSBlbHNlXG4gICAgICBpZiAoZGVm4oKcIGluc3RhbmNlb2YgRXhwbC5QcmltKSB7XG4gICAgICAgICBqb2luzrEoZGVm4oKcLm9wLl9fzrEsIGRlZuKCnC54KVxuICAgICAgfSBlbHNlXG4gICAgICBpZiAoZGVm4oKcIGluc3RhbmNlb2YgRXhwbC5MZXRSZWMpIHtcbiAgICAgICAgIHJlY0RlZnNfKERpcmVjdGlvbi5Cd2QsIGRlZuKCnC7OtClcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICBhYnN1cmQoKVxuICAgICAgfVxuICAgfSlcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGV2YWxfICjPgTogRW52LCBlOiBFeHByKTogRXhwbFZhbHVlIHtcbiAgIGlmIChlIGluc3RhbmNlb2YgRXhwci5Db25zdE51bSkge1xuICAgICAgcmV0dXJuIGV4cGxWYWx1ZShFeHBsLmVtcHR5KCksIG51bShlLnZhbC52YWwpKVxuICAgfSBlbHNlXG4gICBpZiAoZSBpbnN0YW5jZW9mIEV4cHIuQ29uc3RTdHIpIHtcbiAgICAgIHJldHVybiBleHBsVmFsdWUoRXhwbC5lbXB0eSgpLCBzdHIoZS52YWwudmFsKSlcbiAgIH0gZWxzZVxuICAgaWYgKGUgaW5zdGFuY2VvZiBFeHByLkZ1bikge1xuICAgICAgcmV0dXJuIGV4cGxWYWx1ZShFeHBsLmVtcHR5KCksIGNsb3N1cmUozr0oKSwgz4EsIG5pbCgpLCBldmFsVHJpZShlLs+DKSkpXG4gICB9IGVsc2VcbiAgIGlmIChlIGluc3RhbmNlb2YgRXhwci5Db25zdHIpIHtcbiAgICAgIGxldCB0dsyFOiBFeHBsVmFsdWVbXSA9IGUuYXJncy50b0FycmF5KCkubWFwKChlOiBFeHByKSA9PiBldmFsXyjPgSwgZSkpLFxuICAgICAgICAgIGM6IHN0cmluZyA9IGUuY3RyLnZhbCxcbiAgICAgICAgICBkOiBEYXRhVHlwZSA9IF9fbm9uTnVsbChjdHJUb0RhdGFUeXBlLmdldChjKSksXG4gICAgICAgICAgdjogQW5ub3RhdGVkPERhdGFWYWx1ZT4gPSBhbm5vdGF0ZWRBdCjOvSgpLCBkLmN0cnMuZ2V0KGMpIS5DLCAuLi50dsyFLm1hcCgoe3Z9KSA9PiB2KSlcbiAgICAgIHYuX19leHBsID0gbWFrZShkLmV4cGxDzIUuZ2V0KGMpISwgLi4udHbMhS5tYXAoKHt0fSkgPT4gdCkpXG4gICAgICByZXR1cm4gZXhwbFZhbHVlKEV4cGwuZW1wdHkoKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKGUgaW5zdGFuY2VvZiBFeHByLlF1b3RlKSB7XG4gICAgICByZXR1cm4gZXhwbFZhbHVlKEV4cGwucXVvdGUoKSwgY29weUF0KM69KCksIGUuZSkpXG4gICB9IGVsc2VcbiAgIGlmIChlIGluc3RhbmNlb2YgRXhwci5WYXIpIHtcbiAgICAgIGlmICjPgS5oYXMoZS54KSkge1xuICAgICAgICAgY29uc3QgdjogQW5ub3RhdGVkPFZhbHVlPiA9IM+BLmdldChlLngpIVxuICAgICAgICAgcmV0dXJuIGV4cGxWYWx1ZShFeHBsLnZhcl8oZS54LCB2KSwgY29weUF0KM69KCksIHYpKVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHJldHVybiBlcnJvcihgVmFyaWFibGUgXCIke2UueC52YWx9XCIgbm90IGZvdW5kLmApXG4gICAgICB9XG4gICB9IGVsc2VcbiAgIGlmIChlIGluc3RhbmNlb2YgRXhwci5BcHApIHtcbiAgICAgIGNvbnN0IFt0ZiwgdHVdOiBbRXhwbFZhbHVlLCBFeHBsVmFsdWVdID0gW2V2YWxfKM+BLCBlLmYpLCBldmFsXyjPgSwgZS5lKV0sXG4gICAgICAgICAgICBbdiwgdV06IFtWYWx1ZSwgVmFsdWVdID0gW3RmLnYsIHR1LnZdXG4gICAgICBpZiAodiBpbnN0YW5jZW9mIENsb3N1cmUpIHtcbiAgICAgICAgIGNvbnN0IFvOtCwgz4HhtZ9dOiBbTGlzdDxFeHBsLlJlY0RlZj4sIEVudl0gPSByZWNEZWZzKHYuzrQsIHYuz4EsIHYuzrQpLFxuICAgICAgICAgICAgICAgW8+ByrksIM6+zrpdOiBbRW52LCBNYXRjaDxFeHByPl0gPSB2LmYuYXBwbHkodSksXG4gICAgICAgICAgICAgICB0djogRXhwbFZhbHVlID0gZXZhbF8odi7PgS5jb25jYXQoz4HhtZ8uY29uY2F0KM+ByrkpKSwgzr7Oui7OuilcbiAgICAgICAgIHJldHVybiBleHBsVmFsdWUoRXhwbC5hcHAodGYsIHR1LCDOtCwgzr7OuiwgdHYpLCBjb3B5QXQozr0oKSwgdHYudikpXG4gICAgICB9IGVsc2UgXG4gICAgICBpZiAodiBpbnN0YW5jZW9mIFVuYXJ5T3ApIHtcbiAgICAgICAgIGlmICh1IGluc3RhbmNlb2YgTnVtIHx8IHUgaW5zdGFuY2VvZiBTdHIpIHtcbiAgICAgICAgICAgIHJldHVybiBleHBsVmFsdWUoRXhwbC51bmFyeUFwcCh0ZiwgdHUpLCB2Lm9wKHUpKVxuICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHJldHVybiBlcnJvcihgQXBwbHlpbmcgXCIke3YubmFtZX1cIiB0byBub24tcHJpbWl0aXZlIHZhbHVlLmAsIHUpXG4gICAgICAgICB9XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAgcmV0dXJuIGVycm9yKGBDYW5ub3QgYXBwbHkgJHtjbGFzc05hbWUodil9YClcbiAgICAgIH1cbiAgIH0gZWxzZVxuICAgLy8gQmluYXJ5IG9wZXJhdG9ycyBhcmUgKGN1cnJlbnRseSkgXCJzeW50YXhcIiwgcmF0aGVyIHRoYW4gZmlyc3QtY2xhc3MuXG4gICBpZiAoZSBpbnN0YW5jZW9mIEV4cHIuQmluYXJ5QXBwKSB7XG4gICAgICBpZiAoYmluYXJ5T3BzLmhhcyhlLm9wTmFtZS52YWwpKSB7XG4gICAgICAgICBjb25zdCBvcDogQmluYXJ5T3AgPSBiaW5hcnlPcHMuZ2V0KGUub3BOYW1lLnZhbCkhLFxuICAgICAgICAgICAgICAgW3R2MSwgdHYyXTogW0V4cGxWYWx1ZSwgRXhwbFZhbHVlXSA9IFtldmFsXyjPgSwgZS5lMSksIGV2YWxfKM+BLCBlLmUyKV0sXG4gICAgICAgICAgICAgICBbdjEsIHYyXTogW1ZhbHVlLCBWYWx1ZV0gPSBbdHYxLnYsIHR2Mi52XVxuICAgICAgICAgaWYgKCh2MSBpbnN0YW5jZW9mIE51bSB8fCB2MSBpbnN0YW5jZW9mIFN0cikgJiYgKHYyIGluc3RhbmNlb2YgTnVtIHx8IHYyIGluc3RhbmNlb2YgU3RyKSkge1xuICAgICAgICAgICAgICAgcmV0dXJuIGV4cGxWYWx1ZShFeHBsLmJpbmFyeUFwcCh0djEsIGUub3BOYW1lLCB0djIpLCBvcC5vcCh2MSwgdjIpKVxuICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIHJldHVybiBlcnJvcihgQXBwbHlpbmcgXCIke2Uub3BOYW1lfVwiIHRvIG5vbi1wcmltaXRpdmUgdmFsdWUuYCwgdjEsIHYyKVxuICAgICAgICAgfVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHJldHVybiBlcnJvcihgQmluYXJ5IHByaW1pdGl2ZSBcIiR7ZS5vcE5hbWUudmFsfVwiIG5vdCBmb3VuZC5gKVxuICAgICAgfVxuICAgfSBlbHNlXG4gICBpZiAoZSBpbnN0YW5jZW9mIEV4cHIuRGVmcykge1xuICAgICAgY29uc3QgW2RlZsyF4oKcLCDPgcq5XTogW0xpc3Q8RXhwbC5EZWY+LCBFbnZdID0gZGVmcyjPgSwgZS5kZWbMhSwgZW1wdHlFbnYoKSksXG4gICAgICAgICAgICB0djogRXhwbFZhbHVlID0gZXZhbF8oz4EuY29uY2F0KM+ByrkpLCBlLmUpXG4gICAgICByZXR1cm4gZXhwbFZhbHVlKEV4cGwuZGVmcyhkZWbMheKCnCwgdHYpLCBjb3B5QXQozr0oKSwgdHYudikpXG4gICB9IGVsc2VcbiAgIGlmIChlIGluc3RhbmNlb2YgRXhwci5NYXRjaEFzKSB7XG4gICAgICBjb25zdCB0dTogRXhwbFZhbHVlID0gZXZhbF8oz4EsIGUuZSksXG4gICAgICAgICAgICBbz4HKuSwgzr7Oul06IFtFbnYsIE1hdGNoPEV4cHI+XSA9IGV2YWxUcmllKGUuz4MpLmFwcGx5KHR1LnYpLFxuICAgICAgICAgICAgdHY6IEV4cGxWYWx1ZSA9IGV2YWxfKM+BLmNvbmNhdCjPgcq5KSwgzr7Oui7OuilcbiAgICAgIHJldHVybiBleHBsVmFsdWUoRXhwbC5tYXRjaEFzKHR1LCDOvs66LCB0diksIGNvcHlBdCjOvSgpLCB0di52KSlcbiAgIH0gZWxzZVxuICAgaWYgKGUgaW5zdGFuY2VvZiBFeHByLlR5cGVtYXRjaCkge1xuICAgICAgY29uc3QgdHU6IEV4cGxWYWx1ZSA9IGV2YWxfKM+BLCBlLmUpLFxuICAgICAgICAgICAgZDogRGF0YVR5cGUgfCBQcmltVHlwZSA9IGN0clRvRGF0YVR5cGUuZ2V0KGNsYXNzTmFtZSh0dS52KSkgfHwgdHlwZXMuZ2V0KGNsYXNzTmFtZSh0dS52KSkhLFxuICAgICAgICAgICAgZcq5OiBFeHByIHwgdW5kZWZpbmVkID0gZ2V0KGUuY2FzZXMsIGQubmFtZSlcbiAgICAgIGlmIChlyrkgPT09IHVuZGVmaW5lZCkge1xuICAgICAgICAgcmV0dXJuIGVycm9yKGBUeXBlY2FzZSBtaXNtYXRjaDogbm8gY2xhdXNlIGZvciAke2NsYXNzTmFtZSh0dS52KX0uYClcbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICBjb25zdCB0djogRXhwbFZhbHVlID0gZXZhbF8oz4EsIGXKuSlcbiAgICAgICAgIHJldHVybiBleHBsVmFsdWUoRXhwbC50eXBlbWF0Y2godHUsIGQubmFtZSwgdHYpLCBjb3B5QXQozr0oKSwgdHYudikpXG4gICAgICB9XG4gICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGFic3VyZChgVW5pbXBsZW1lbnRlZCBleHByZXNzaW9uIGZvcm06ICR7Y2xhc3NOYW1lKGUpfS5gKVxuICAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gZXZhbF9md2QgKGU6IEV4cHIsIHt0LCB2fTogRXhwbFZhbHVlKTogdm9pZCB7XG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuRW1wdHkpIHtcbiAgICAgIGlmICh2IGluc3RhbmNlb2YgTnVtIHx8IHYgaW5zdGFuY2VvZiBTdHIgfHwgdiBpbnN0YW5jZW9mIENsb3N1cmUpIHtcbiAgICAgICAgIHNldM6xKGUuX1/OsSwgdilcbiAgICAgIH0gZWxzZVxuICAgICAgaWYgKHYgaW5zdGFuY2VvZiBEYXRhVmFsdWUpIHtcbiAgICAgICAgIGNvbnN0IGXKuTogRXhwci5Db25zdHIgPSBhcyhlLCBFeHByLkNvbnN0cilcbiAgICAgICAgIHppcCh2LmZpZWxkRXhwbFZhbHVlcygpLCBlyrkuYXJncy50b0FycmF5KCkpLm1hcCgoW1t0LCB2XSwgZV0pID0+IGV2YWxfZndkKGUsIGV4cGxWYWx1ZSh0LCB2KSkpXG4gICAgICAgICBzZXTOsShlLl9fzrEsIHYpXG4gICAgICB9XG4gICB9IGVsc2VcbiAgIGlmICh0IGluc3RhbmNlb2YgRXhwbC5RdW90ZSkge1xuICAgICAgc2V0zrEoZS5fX86xLCB2KVxuICAgfSBlbHNlXG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuVmFyKSB7XG4gICAgICBzZXTOsShhbm4ubWVldChlLl9fzrEsIHQudi5fX86xKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLkFwcCkge1xuICAgICAgY29uc3QgZcq5OiBFeHByLkFwcCA9IGFzKGUsIEV4cHIuQXBwKVxuICAgICAgZXZhbF9md2QoZcq5LmYsIHQudGYpXG4gICAgICBldmFsX2Z3ZChlyrkuZSwgdC50dSlcbiAgICAgIHJlY0RlZnNfKERpcmVjdGlvbi5Gd2QsIHQuzrQpXG4gICAgICBldmFsX2Z3ZCh0Ls6+Ls66LCB0LnR2KVxuICAgICAgc2V0zrEoYW5uLm1lZXQodC50Zi52Ll9fzrEsIG1hdGNoX2Z3ZCh0Ls6+KSwgZS5fX86xLCB0LnR2LnYuX1/OsSksIHYpXG4gICB9IGVsc2VcbiAgIGlmICh0IGluc3RhbmNlb2YgRXhwbC5VbmFyeUFwcCkge1xuICAgICAgY29uc3QgZcq5OiBFeHByLkFwcCA9IGFzKGUsIEV4cHIuQXBwKVxuICAgICAgZXZhbF9md2QoZcq5LmYsIHQudGYpXG4gICAgICBldmFsX2Z3ZChlyrkuZSwgdC50dilcbiAgICAgIHNldM6xKGFubi5tZWV0KHQudGYudi5fX86xLCB0LnR2LnYuX1/OsSwgZS5fX86xKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLkJpbmFyeUFwcCkge1xuICAgICAgY29uc3QgZcq5OiBFeHByLkJpbmFyeUFwcCA9IGFzKGUsIEV4cHIuQmluYXJ5QXBwKVxuICAgICAgZXZhbF9md2QoZcq5LmUxLCB0LnR2MSlcbiAgICAgIGV2YWxfZndkKGXKuS5lMiwgdC50djIpXG4gICAgICBzZXTOsShhbm4ubWVldCh0LnR2MS52Ll9fzrEsIHQudHYyLnYuX1/OsSwgZS5fX86xKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLkRlZnMpIHtcbiAgICAgIGNvbnN0IGXKuTogRXhwci5EZWZzID0gYXMoZSwgRXhwci5EZWZzKVxuICAgICAgZGVmc19md2QoZcq5LmRlZsyFLCB0LmRlZsyFKVxuICAgICAgZXZhbF9md2QoZcq5LmUsIHQudHYpXG4gICAgICBzZXTOsShhbm4ubWVldChlLl9fzrEsIHQudHYudi5fX86xKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLk1hdGNoQXMpIHtcbiAgICAgIGNvbnN0IGXKuTogRXhwci5NYXRjaEFzID0gYXMoZSwgRXhwci5NYXRjaEFzKVxuICAgICAgZXZhbF9md2QoZcq5LmUsIHQudHUpXG4gICAgICBldmFsX2Z3ZCh0Ls6+Ls66LCB0LnR2KVxuICAgICAgc2V0zrEoYW5uLm1lZXQobWF0Y2hfZndkKHQuzr4pLCBlLl9fzrEsIHQudHYudi5fX86xKSwgdilcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLlR5cGVtYXRjaCkge1xuICAgICAgY29uc3QgZcq5OiBFeHByLlR5cGVtYXRjaCA9IGFzKGUsIEV4cHIuVHlwZW1hdGNoKVxuICAgICAgZXZhbF9md2QoZcq5LmUsIHQudHUpXG4gICAgICBldmFsX2Z3ZChnZXQoZcq5LmNhc2VzLCB0LmQpISwgdC50dilcbiAgICAgIHNldM6xKGFubi5tZWV0KGUuX1/OsSwgdC50di52Ll9fzrEpLCB2KVxuICAgfSBlbHNlIHtcbiAgICAgIGFic3VyZCgpXG4gICB9XG59XG5cbi8vIEF2b2lkIGV4Y2Vzc2l2ZSBqb2lucyB2aWEgYSBtZXJnaW5nIGltcGxlbWVudGF0aW9uOyByZXF1aXJlcyBhbGwgYW5ub3RhdGlvbnMgdG8gaGF2ZSBiZWVuIGNsZWFyZWQgZmlyc3QuXG5leHBvcnQgZnVuY3Rpb24gZXZhbF9id2QgKGU6IEV4cHIsIHt0LCB2fTogRXhwbFZhbHVlKTogdm9pZCB7XG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuRW1wdHkpIHtcbiAgICAgIGlmICh2IGluc3RhbmNlb2YgTnVtIHx8IHYgaW5zdGFuY2VvZiBTdHIgfHwgdiBpbnN0YW5jZW9mIENsb3N1cmUpIHtcbiAgICAgICAgIGpvaW7OsSh2Ll9fzrEsIGUpXG4gICAgICB9IGVsc2VcbiAgICAgIGlmICh2IGluc3RhbmNlb2YgRGF0YVZhbHVlKSB7XG4gICAgICAgICBjb25zdCBlyrk6IEV4cHIuQ29uc3RyID0gYXMoZSwgRXhwci5Db25zdHIpXG4gICAgICAgICAvLyByZXZlcnNlIG9yZGVyIGJ1dCBzaG91bGRuJ3QgbWF0dGVyIGluIGFic2VuY2Ugb2Ygc2lkZS1lZmZlY3RzOlxuICAgICAgICAgemlwKHYuZmllbGRFeHBsVmFsdWVzKCksIGXKuS5hcmdzLnRvQXJyYXkoKSkubWFwKChbW3QsIHZdLCBlXSkgPT4gZXZhbF9id2QoZSwgZXhwbFZhbHVlKHQsIHYpKSlcbiAgICAgICAgIGpvaW7OsSh2Ll9fzrEsIGUpXG4gICAgICB9IGVsc2Uge1xuICAgICAgICAgYWJzdXJkKClcbiAgICAgIH1cbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLlF1b3RlKSB7XG4gICAgICBqb2luzrEodi5fX86xLCBlKVxuICAgfSBlbHNlXG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuVmFyKSB7XG4gICAgICBqb2luzrEodi5fX86xLCB0LnYpXG4gICAgICBqb2luzrEodi5fX86xLCBlKVxuICAgfSBlbHNlXG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuQXBwKSB7XG4gICAgICBhc3NlcnQodC50Zi52IGluc3RhbmNlb2YgQ2xvc3VyZSlcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudHYudilcbiAgICAgIGV2YWxfYndkKHQuzr4uzrosIHQudHYpXG4gICAgICBtYXRjaF9id2QodC7Oviwgdi5fX86xKVxuICAgICAgcmVjRGVmc18oRGlyZWN0aW9uLkJ3ZCwgdC7OtClcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudGYudilcbiAgICAgIGNvbnN0IGXKuTogRXhwci5BcHAgPSBhcyhlLCBFeHByLkFwcClcbiAgICAgIGV2YWxfYndkKGXKuS5mLCB0LnRmKVxuICAgICAgZXZhbF9id2QoZcq5LmUsIHQudHUpXG4gICAgICBqb2luzrEodi5fX86xLCBlKVxuICAgfSBlbHNlXG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuVW5hcnlBcHApIHtcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudGYudilcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudHYudilcbiAgICAgIGNvbnN0IGXKuTogRXhwci5BcHAgPSBhcyhlLCBFeHByLkFwcClcbiAgICAgIGV2YWxfYndkKGXKuS5mLCB0LnRmKVxuICAgICAgZXZhbF9id2QoZcq5LmUsIHQudHYpXG4gICAgICBqb2luzrEodi5fX86xLCBlKVxuICAgfSBlbHNlXG4gICBpZiAodCBpbnN0YW5jZW9mIEV4cGwuQmluYXJ5QXBwKSB7XG4gICAgICBhc3NlcnQoYmluYXJ5T3BzLmhhcyh0Lm9wTmFtZS52YWwpKVxuICAgICAgam9pbs6xKHYuX1/OsSwgdC50djEudilcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudHYyLnYpXG4gICAgICBjb25zdCBlyrk6IEV4cHIuQmluYXJ5QXBwID0gYXMoZSwgRXhwci5CaW5hcnlBcHApXG4gICAgICBldmFsX2J3ZChlyrkuZTEsIHQudHYxKVxuICAgICAgZXZhbF9id2QoZcq5LmUyLCB0LnR2MilcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIGUpXG4gICB9IGVsc2VcbiAgIGlmICh0IGluc3RhbmNlb2YgRXhwbC5EZWZzKSB7XG4gICAgICBqb2luzrEodi5fX86xLCB0LnR2LnYpXG4gICAgICBjb25zdCBlyrk6IEV4cHIuRGVmcyA9IGFzKGUsIEV4cHIuRGVmcylcbiAgICAgIGV2YWxfYndkKGXKuS5lLCB0LnR2KVxuICAgICAgZGVmc19id2QoZcq5LmRlZsyFLCB0LmRlZsyFKVxuICAgICAgam9pbs6xKHYuX1/OsSwgZSlcbiAgIH0gZWxzZVxuICAgaWYgKHQgaW5zdGFuY2VvZiBFeHBsLk1hdGNoQXMpIHtcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudHYudilcbiAgICAgIGNvbnN0IGXKuTogRXhwci5NYXRjaEFzID0gYXMoZSwgRXhwci5NYXRjaEFzKVxuICAgICAgZXZhbF9id2QodC7Ovi7OuiwgdC50dilcbiAgICAgIG1hdGNoX2J3ZCh0Ls6+LCB2Ll9fzrEpXG4gICAgICBldmFsX2J3ZChlyrkuZSwgdC50dSlcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIGUpXG4gICB9IGVsc2VcbiAgIGlmICh0IGluc3RhbmNlb2YgRXhwbC5UeXBlbWF0Y2gpIHtcbiAgICAgIGpvaW7OsSh2Ll9fzrEsIHQudHYudilcbiAgICAgIGNvbnN0IGXKuTogRXhwci5UeXBlbWF0Y2ggPSBhcyhlLCBFeHByLlR5cGVtYXRjaClcbiAgICAgIGV2YWxfYndkKGdldChlyrkuY2FzZXMsIHQuZCkhLCB0LnR2KVxuICAgICAgZXZhbF9id2QoZcq5LmUsIHQudHUpXG4gICAgICBqb2luzrEodi5fX86xLCBlKVxuICAgfSBlbHNlIHtcbiAgICAgIGFic3VyZCgpXG4gICB9XG59XG5cbn1cblxuaW5pdERhdGFUeXBlKFxuICAgRXhwci5FeHByLFxuICAgW0V4cHIuQXBwLCBFeHByLkJpbmFyeUFwcCwgRXhwci5Db25zdE51bSwgRXhwci5Db25zdFN0ciwgRXhwci5Db25zdHIsIEV4cHIuRGVmcywgRXhwci5GdW4sIEV4cHIuTWF0Y2hBcywgRXhwci5RdW90ZSwgRXhwci5WYXJdXG4pXG4iLCJpbXBvcnQgeyBBbm5vdGF0ZWQgfSBmcm9tIFwiLi9Bbm5vdGF0ZWRcIlxuaW1wb3J0IHsgTGlzdCB9IGZyb20gXCIuL0Jhc2VUeXBlc1wiXG5pbXBvcnQgeyBEYXRhVmFsdWUgfSBmcm9tIFwiLi9EYXRhVmFsdWVcIlxuaW1wb3J0IHsgRXZhbCB9IGZyb20gXCIuL0V2YWxcIlxuaW1wb3J0IHsgRXhwciB9IGZyb20gXCIuL0V4cHJcIlxuaW1wb3J0IHsgTWF0Y2ggfSBmcm9tIFwiLi9NYXRjaFwiXG5pbXBvcnQgeyBVbmFyeU9wIH0gZnJvbSBcIi4vUHJpbWl0aXZlXCJcbmltcG9ydCB7IFN0ciwgVmFsdWUsIF8sIG1ha2UgfSBmcm9tIFwiLi9WYWx1ZVwiXG5pbXBvcnQgeyDOvSwgYXQgfSBmcm9tIFwiLi9WZXJzaW9uZWRcIlxuXG5leHBvcnQgdHlwZSBDbG9zdXJlID0gRXZhbC5DbG9zdXJlXG5leHBvcnQgdHlwZSBFeHBsID0gRXhwbC5FeHBsXG5cbmV4cG9ydCBjbGFzcyBFeHBsVmFsdWUgZXh0ZW5kcyBEYXRhVmFsdWU8XCJFeHBsVmFsdWVcIj4ge1xuICAgdDogRXhwbCA9IF9cbiAgIHY6IEFubm90YXRlZDxWYWx1ZT4gPSBfXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBleHBsVmFsdWUgKHQ6IEV4cGwsIHY6IEFubm90YXRlZDxWYWx1ZT4pOiBFeHBsVmFsdWUge1xuICAgcmV0dXJuIG1ha2UoRXhwbFZhbHVlLCB0LCB2KVxufVxuXG5leHBvcnQgbmFtZXNwYWNlIEV4cGwge1xuICAgZXhwb3J0IGFic3RyYWN0IGNsYXNzIEV4cGwgZXh0ZW5kcyBEYXRhVmFsdWU8XCJFeHBsXCI+IHtcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIEFwcCBleHRlbmRzIEV4cGwge1xuICAgICAgdGY6IEV4cGxWYWx1ZSA9IF9cbiAgICAgIHR1OiBFeHBsVmFsdWUgPSBfXG4gICAgICDOtDogTGlzdDxSZWNEZWY+ID0gXyAvLyBhZGRpdGlvbmFsIHJlY3Vyc2l2ZSBmdW5jdGlvbnMgYm91bmQgYXQgdGhpcyBzdGVwXG4gICAgICDOvjogTWF0Y2g8RXhwcj4gPSBfXG4gICAgICB0djogRXhwbFZhbHVlID0gX1xuICAgfVxuXG4gIGV4cG9ydCBmdW5jdGlvbiBhcHAgKHRmOiBFeHBsVmFsdWUsIHR1OiBFeHBsVmFsdWUsIM60OiBMaXN0PFJlY0RlZj4sIM6+OiBNYXRjaDxFeHByPiwgdHY6IEV4cGxWYWx1ZSk6IEFwcCB7XG4gICAgICByZXR1cm4gYXQozr0oKSwgQXBwLCB0ZiwgdHUsIM60LCDOviwgdHYpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBVbmFyeUFwcCBleHRlbmRzIEV4cGwge1xuICAgICAgdGY6IEV4cGxWYWx1ZSA9IF9cbiAgICAgIHR2OiBFeHBsVmFsdWUgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiB1bmFyeUFwcCAodGY6IEV4cGxWYWx1ZSwgdHY6IEV4cGxWYWx1ZSk6IFVuYXJ5QXBwIHtcbiAgICAgIHJldHVybiBhdCjOvSgpLCBVbmFyeUFwcCwgdGYsIHR2KVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgQmluYXJ5QXBwIGV4dGVuZHMgRXhwbCB7XG4gICAgICB0djE6IEV4cGxWYWx1ZSA9IF9cbiAgICAgIG9wTmFtZTogU3RyID0gX1xuICAgICAgdHYyOiBFeHBsVmFsdWUgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBiaW5hcnlBcHAgKHR2MTogRXhwbFZhbHVlLCBvcE5hbWU6IFN0ciwgdHYyOiBFeHBsVmFsdWUpOiBCaW5hcnlBcHAge1xuICAgICAgcmV0dXJuIGF0KM69KCksIEJpbmFyeUFwcCwgdHYxLCBvcE5hbWUsIHR2MilcbiAgIH1cblxuICAgZXhwb3J0IGFic3RyYWN0IGNsYXNzIERlZiBleHRlbmRzIERhdGFWYWx1ZTxcIkV4cGwuRGVmXCI+IHtcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIExldCBleHRlbmRzIERlZiB7XG4gICAgICB4OiBBbm5vdGF0ZWQ8U3RyPiA9IF9cbiAgICAgIHR2OiBFeHBsVmFsdWUgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBsZXRfICh4OiBBbm5vdGF0ZWQ8U3RyPiwgdHY6IEV4cGxWYWx1ZSk6IExldCB7XG4gICAgICByZXR1cm4gYXQozr0oKSwgTGV0LCB4LCB0dilcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIFByaW0gZXh0ZW5kcyBEZWYge1xuICAgICAgeDogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICAgICBvcDogQW5ub3RhdGVkPFVuYXJ5T3A+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gcHJpbSAoeDogQW5ub3RhdGVkPFN0cj4sIG9wOiBBbm5vdGF0ZWQ8VW5hcnlPcD4pOiBQcmltIHtcbiAgICAgIHJldHVybiBhdCjOvSgpLCBQcmltLCB4LCBvcClcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIFJlY0RlZiBleHRlbmRzIERhdGFWYWx1ZTxcIkV4cGwuUmVjRGVmXCI+IHtcbiAgICAgIHg6IEFubm90YXRlZDxTdHI+ID0gX1xuICAgICAgZjogQ2xvc3VyZSA9IF9cbiAgIH1cblxuICAgZXhwb3J0IGZ1bmN0aW9uIHJlY0RlZiAoeDogQW5ub3RhdGVkPFN0cj4sIGY6IENsb3N1cmUpOiBSZWNEZWYge1xuICAgICAgcmV0dXJuIGF0KM69KCksIFJlY0RlZiwgeCwgZilcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIExldFJlYyBleHRlbmRzIERlZiB7XG4gICAgICDOtDogTGlzdDxSZWNEZWY+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gbGV0UmVjICjOtDogTGlzdDxSZWNEZWY+KTogTGV0UmVjIHtcbiAgICAgIHJldHVybiBhdCjOvSgpLCBMZXRSZWMsIM60KVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgRGVmcyBleHRlbmRzIEV4cGwge1xuICAgICAgZGVmzIU6IExpc3Q8RGVmPiA9IF9cbiAgICAgIHR2OiBFeHBsVmFsdWUgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBkZWZzIChkZWbMhTogTGlzdDxEZWY+LCB0djogRXhwbFZhbHVlKTogRGVmcyB7XG4gICAgICByZXR1cm4gYXQozr0oKSwgRGVmcywgZGVmzIUsIHR2KVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgRW1wdHkgZXh0ZW5kcyBFeHBsIHtcbiAgIH1cblxuICAgZXhwb3J0IGZ1bmN0aW9uIGVtcHR5ICgpOiBFbXB0eSB7XG4gICAgICByZXR1cm4gYXQozr0oKSwgRW1wdHkpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBNYXRjaEFzIGV4dGVuZHMgRXhwbCB7XG4gICAgICB0dTogRXhwbFZhbHVlID0gX1xuICAgICAgzr46IE1hdGNoPEV4cHI+ID0gX1xuICAgICAgdHY6IEV4cGxWYWx1ZSA9IF9cbiAgIH1cblxuICAgZXhwb3J0IGZ1bmN0aW9uIG1hdGNoQXMgKHR1OiBFeHBsVmFsdWUsIM6+OiBNYXRjaDxFeHByPiwgdHY6IEV4cGxWYWx1ZSk6IE1hdGNoQXMge1xuICAgICAgcmV0dXJuIGF0KM69KCksIE1hdGNoQXMsIHR1LCDOviwgdHYpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBRdW90ZSBleHRlbmRzIEV4cGwge1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gcXVvdGUgKCk6IFF1b3RlIHtcbiAgICAgIHJldHVybiBhdCjOvSgpLCBRdW90ZSlcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIFR5cGVtYXRjaCBleHRlbmRzIEV4cGwge1xuICAgICAgdHU6IEV4cGxWYWx1ZSA9IF9cbiAgICAgIGQ6IFN0ciA9IF9cbiAgICAgIHR2OiBFeHBsVmFsdWUgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiB0eXBlbWF0Y2ggKHR1OiBFeHBsVmFsdWUsIGQ6IFN0ciwgdHY6IEV4cGxWYWx1ZSk6IFR5cGVtYXRjaCB7XG4gICAgICByZXR1cm4gYXQozr0oKSwgVHlwZW1hdGNoLCB0dSwgZCwgdHYpXG4gICB9XG5cbiAgIC8vIHYgaXMgdGhlIHJlc29sdmVkIHZhbHVlIG9mIHhcbiAgIGV4cG9ydCBjbGFzcyBWYXIgZXh0ZW5kcyBFeHBsIHtcbiAgICAgIHg6IFN0ciA9IF9cbiAgICAgIHY6IEFubm90YXRlZDxWYWx1ZT4gPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiB2YXJfICh4OiBTdHIsIHY6IEFubm90YXRlZDxWYWx1ZT4pOiBWYXIge1xuICAgICAgcmV0dXJuIGF0KM69KCksIFZhciwgeCwgdilcbiAgIH1cbn1cbiIsImltcG9ydCB7IGFic3VyZCwgZXJyb3IgfSBmcm9tIFwiLi91dGlsL0NvcmVcIlxuaW1wb3J0IHsgZXEgfSBmcm9tIFwiLi91dGlsL09yZFwiXG5pbXBvcnQgeyBBbm5vdGF0ZWQsIEFubm90YXRlZEMgfSBmcm9tIFwiLi9Bbm5vdGF0ZWRcIlxuaW1wb3J0IHsgTGlzdCB9IGZyb20gXCIuL0Jhc2VUeXBlc1wiXG5pbXBvcnQgeyBjdHJUb0RhdGFUeXBlIH0gZnJvbSBcIi4vRGF0YVR5cGVcIlxuaW1wb3J0IHsgRGF0YVZhbHVlIH0gZnJvbSBcIi4vRGF0YVZhbHVlXCJcbmltcG9ydCB7IEZpbml0ZU1hcCwgdW5pb25XaXRoIH0gZnJvbSBcIi4vRmluaXRlTWFwXCJcbmltcG9ydCB7IElkLCBOdW0sIFN0ciwgXywgbWFrZSB9IGZyb20gXCIuL1ZhbHVlXCJcbmltcG9ydCB7IM69LCBhdCB9IGZyb20gXCIuL1ZlcnNpb25lZFwiXG5cbi8vIENvbnN0YW50cyB1c2VkIGZvciBwYXJzaW5nLCBhbmQgYWxzbyBmb3IgdG9TdHJpbmcoKSBpbXBsZW1lbnRhdGlvbnMuXG5leHBvcnQgbmFtZXNwYWNlIHN0cmluZ3Mge1xuICAgZXhwb3J0IGNvbnN0IGFycm93OiBzdHJpbmcgPSBcIuKGklwiXG4gICBleHBvcnQgY29uc3QgYXM6IHN0cmluZyA9IFwiYXNcIlxuICAgZXhwb3J0IGNvbnN0IGJyYWNrZXRMOiBzdHJpbmcgPSBcIltcIlxuICAgZXhwb3J0IGNvbnN0IGJyYWNrZXRSOiBzdHJpbmcgPSBcIl1cIlxuICAgZXhwb3J0IGNvbnN0IGVxdWFsczogc3RyaW5nID0gXCI9XCJcbiAgIGV4cG9ydCBjb25zdCBmdW46IHN0cmluZyA9IFwiZnVuXCJcbiAgIGV4cG9ydCBjb25zdCBpbl86IHN0cmluZyA9IFwiaW5cIlxuICAgZXhwb3J0IGNvbnN0IGxldF86IHN0cmluZyA9IFwibGV0XCJcbiAgIGV4cG9ydCBjb25zdCBsZXRSZWM6IHN0cmluZyA9IFwibGV0cmVjXCJcbiAgIGV4cG9ydCBjb25zdCBtYXRjaDogc3RyaW5nID0gXCJtYXRjaFwiXG4gICBleHBvcnQgY29uc3QgcHJpbWl0aXZlOiBzdHJpbmcgPSBcInByaW1pdGl2ZVwiXG4gICBleHBvcnQgY29uc3QgcGFyZW5MOiBzdHJpbmcgPSBcIihcIlxuICAgZXhwb3J0IGNvbnN0IHBhcmVuUjogc3RyaW5nID0gXCIpXCJcbiAgIGV4cG9ydCBjb25zdCBxdW90ZXM6IHN0cmluZyA9ICdcIidcbiAgIGV4cG9ydCBjb25zdCB0eXBlbWF0Y2g6IHN0cmluZyA9IFwidHlwZW1hdGNoXCJcbn1cblxuZXhwb3J0IHR5cGUgRXhwciA9IEV4cHIuRXhwclxuZXhwb3J0IHR5cGUgQ29udCA9IEV4cHIuQ29udFxuXG5leHBvcnQgbmFtZXNwYWNlIEV4cHIge1xuICAgLy8gVXNlIHRvIGJlIGEgcGFyYW1ldGVyaXNlZCBjbGFzcyBidXQgd2UgY2FuIHNpbXBsaWZ5IHVzaW5nIG91ciBub21pbmFsIHR5cGUgaWRpb20uXG4gICBleHBvcnQgdHlwZSBDb250ID0gRXhwciB8IERhdGFWYWx1ZTxcIlRyaWVcIj5cblxuICAgLy8gVW5yZWxhdGVkIHRvIHRoZSBhbm5vdGF0aW9uIGxhdHRpY2UuIEV4cHIgY2FzZSBpbnRlbnRpb25hbGx5IG9ubHkgZGVmaW5lZCBmb3IgaGlnaGVyLW9yZGVyIChmdW5jdGlvbikgY2FzZS5cbiAgIGZ1bmN0aW9uIGpvaW48SyBleHRlbmRzIENvbnQ+ICjOujogSywgzrrKuTogSyk6IEsge1xuICAgICAgaWYgKM66IGluc3RhbmNlb2YgVHJpZS5UcmllICYmIM66yrkgaW5zdGFuY2VvZiBUcmllLlRyaWUpIHtcbiAgICAgICAgIHJldHVybiBUcmllLlRyaWUuam9pbjxLPijOuiwgzrrKuSkgYXMgS1xuICAgICAgfSBlbHNlXG4gICAgICBpZiAozrogaW5zdGFuY2VvZiBGdW4gJiYgzrrKuSBpbnN0YW5jZW9mIEZ1bikge1xuICAgICAgICAgcmV0dXJuIGZ1bijOvSgpLCBqb2luKM66Ls+DLCDOusq5Ls+DKSkgYXMgRXhwciBhcyBLXG4gICAgICB9IGVsc2Uge1xuICAgICAgICAgcmV0dXJuIGFic3VyZChcIlVuZGVmaW5lZCBqb2luLlwiLCDOuiwgzrrKuSlcbiAgICAgIH1cbiAgIH1cblxuICAgZXhwb3J0IGFic3RyYWN0IGNsYXNzIEV4cHIgZXh0ZW5kcyBBbm5vdGF0ZWRDKERhdGFWYWx1ZSk8XCJFeHByXCI+IHtcbiAgIH1cbiAgIFxuICAgZXhwb3J0IGNsYXNzIEFwcCBleHRlbmRzIEV4cHIge1xuICAgICAgZjogRXhwciA9IF9cbiAgICAgIGU6IEV4cHIgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBhcHAgKGs6IElkLCBmOiBFeHByLCBlOiBFeHByKTogQXBwIHtcbiAgICAgIHJldHVybiBhdChrLCBBcHAsIGYsIGUpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBCaW5hcnlBcHAgZXh0ZW5kcyBFeHByIHtcbiAgICAgIGUxOiBFeHByID0gX1xuICAgICAgb3BOYW1lOiBBbm5vdGF0ZWQ8U3RyPiA9IF9cbiAgICAgIGUyOiBFeHByID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gYmluYXJ5QXBwIChrOiBJZCwgZTE6IEV4cHIsIG9wTmFtZTogQW5ub3RhdGVkPFN0cj4sIGUyOiBFeHByKTogQmluYXJ5QXBwIHtcbiAgICAgIHJldHVybiBhdChrLCBCaW5hcnlBcHAsIGUxLCBvcE5hbWUsIGUyKVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgQ29uc3ROdW0gZXh0ZW5kcyBFeHByIHtcbiAgICAgIHZhbDogQW5ub3RhdGVkPE51bT4gPSBfXG4gICB9XG4gICBcbiAgIGV4cG9ydCBmdW5jdGlvbiBjb25zdE51bSAoazogSWQsIHZhbDogQW5ub3RhdGVkPE51bT4pOiBDb25zdE51bSB7XG4gICAgICByZXR1cm4gYXQoaywgQ29uc3ROdW0sIHZhbClcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIENvbnN0U3RyIGV4dGVuZHMgRXhwciB7XG4gICAgICB2YWw6IEFubm90YXRlZDxTdHI+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gY29uc3RTdHIgKGs6IElkLCB2YWw6IEFubm90YXRlZDxTdHI+KTogQ29uc3RTdHIge1xuICAgICAgcmV0dXJuIGF0KGssIENvbnN0U3RyLCB2YWwpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBDb25zdHIgZXh0ZW5kcyBFeHByIHtcbiAgICAgIGN0cjogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICAgICBhcmdzOiBMaXN0PEV4cHI+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gY29uc3RyIChrOiBJZCwgY3RyOiBBbm5vdGF0ZWQ8U3RyPiwgYXJnczogTGlzdDxFeHByPik6IENvbnN0ciB7XG4gICAgICByZXR1cm4gYXQoaywgQ29uc3RyLCBjdHIsIGFyZ3MpXG4gICB9XG5cbiAgIC8vIEJlY2F1c2UgbGV0L2xldHJlYyBubyBsb25nZXIgaGF2ZSBcImJvZGllc1wiLCB0aGVyZSdzIG5vIHJlYWwgbmVlZCBmb3IgdGhlbSB0byBiZSBzZXBhcmF0ZWx5IHZlcnNpb25lZDtcbiAgIC8vIHRoZSB2YXJpYWJsZXMgdGhleSBpbnRyb2R1Y2UgYXJlLlxuICAgZXhwb3J0IGNsYXNzIERlZiBleHRlbmRzIERhdGFWYWx1ZTxcIkV4cHIuRGVmXCI+IHtcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIExldCBleHRlbmRzIERlZiB7XG4gICAgICB4OiBBbm5vdGF0ZWQ8U3RyPiA9IF9cbiAgICAgIGU6IEV4cHIgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBsZXRfICh4OiBBbm5vdGF0ZWQ8U3RyPiwgZTogRXhwcik6IExldCB7XG4gICAgICByZXR1cm4gbWFrZShMZXQsIHgsIGUpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBQcmltIGV4dGVuZHMgRGVmIHtcbiAgICAgIHg6IEFubm90YXRlZDxTdHI+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gcHJpbSAoeDogQW5ub3RhdGVkPFN0cj4pOiBQcmltIHtcbiAgICAgIHJldHVybiBtYWtlKFByaW0sIHgpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBSZWNEZWYgZXh0ZW5kcyBEYXRhVmFsdWU8XCJSZWNEZWZcIj4ge1xuICAgICAgeDogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICAgICDPgzogVHJpZTxFeHByPiA9IF9cbiAgIH1cbiBcbiAgIGV4cG9ydCBmdW5jdGlvbiByZWNEZWYgKHg6IEFubm90YXRlZDxTdHI+LCDPgzogVHJpZTxFeHByPik6IFJlY0RlZiB7XG4gICAgICByZXR1cm4gbWFrZShSZWNEZWYsIHgsIM+DKVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgTGV0UmVjIGV4dGVuZHMgRGVmIHtcbiAgICAgIM60OiBMaXN0PFJlY0RlZj4gPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBsZXRSZWMgKM60OiBMaXN0PFJlY0RlZj4pOiBMZXRSZWMge1xuICAgICAgcmV0dXJuIG1ha2UoTGV0UmVjLCDOtClcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIERlZnMgZXh0ZW5kcyBFeHByIHtcbiAgICAgIGRlZsyFOiBMaXN0PERlZj4gPSBfXG4gICAgICBlOiBFeHByID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gZGVmcyAoazogSWQsIGRlZsyFOiBMaXN0PERlZj4sIGU6IEV4cHIpOiBEZWZzIHtcbiAgICAgIHJldHVybiBhdChrLCBEZWZzLCBkZWbMhSwgZSlcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIEZ1biBleHRlbmRzIEV4cHIge1xuICAgICAgz4M6IFRyaWU8RXhwcj4gPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBmdW4gKGs6IElkLCDPgzogVHJpZTxFeHByPik6IEZ1biB7XG4gICAgICByZXR1cm4gYXQoaywgRnVuLCDPgylcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIE1hdGNoQXMgZXh0ZW5kcyBFeHByIHtcbiAgICAgIGU6IEV4cHIgPSBfXG4gICAgICDPgzogVHJpZTxFeHByPiA9IF9cbiAgIH1cblxuICAgZXhwb3J0IGZ1bmN0aW9uIG1hdGNoQXMgKGs6IElkLCBlOiBFeHByLCDPgzogVHJpZTxFeHByPik6IE1hdGNoQXMge1xuICAgICAgcmV0dXJuIGF0KGssIE1hdGNoQXMsIGUsIM+DKVxuICAgfVxuXG4gICBleHBvcnQgY2xhc3MgUXVvdGUgZXh0ZW5kcyBFeHByIHtcbiAgICAgIGU6IEV4cHIgPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiBxdW90ZSAoazogSWQsIGU6IEV4cHIpOiBRdW90ZSB7XG4gICAgICByZXR1cm4gYXQoaywgUXVvdGUsIGUpXG4gICB9XG5cbiAgIGV4cG9ydCBjbGFzcyBUeXBlbWF0Y2ggZXh0ZW5kcyBFeHByIHtcbiAgICAgIGU6IEV4cHIgPSBfXG4gICAgICBjYXNlczogRmluaXRlTWFwPEV4cHI+ID0gX1xuICAgfVxuXG4gICBleHBvcnQgZnVuY3Rpb24gdHlwZW1hdGNoIChrOiBJZCwgZTogRXhwciwgY2FzZXM6IEZpbml0ZU1hcDxFeHByPik6IFR5cGVtYXRjaCB7XG4gICAgICByZXR1cm4gYXQoaywgVHlwZW1hdGNoLCBlLCBjYXNlcylcbiAgIH1cblxuICAgZXhwb3J0IGNsYXNzIFZhciBleHRlbmRzIEV4cHIge1xuICAgICAgeDogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICB9XG5cbiAgIGV4cG9ydCBmdW5jdGlvbiB2YXJfIChrOiBJZCwgeDogQW5ub3RhdGVkPFN0cj4pOiBWYXIge1xuICAgICAgcmV0dXJuIGF0KGssIFZhciwgeClcbiAgIH1cblxuICAgZXhwb3J0IHR5cGUgVHJpZTxLIGV4dGVuZHMgQ29udD4gPSBUcmllLlRyaWU8Sz5cblxuICAgZXhwb3J0IG5hbWVzcGFjZSBUcmllIHtcbiAgICAgIGV4cG9ydCBhYnN0cmFjdCBjbGFzcyBUcmllPEsgZXh0ZW5kcyBDb250PiBleHRlbmRzIERhdGFWYWx1ZTxcIlRyaWVcIj4ge1xuICAgICAgICAgc3RhdGljIGpvaW48SyBleHRlbmRzIENvbnQ+ICjPgzogVHJpZTxLPiwgz4Q6IFRyaWU8Sz4pOiBUcmllPEs+IHtcbiAgICAgICAgICAgIGlmIChWYXIuaXMoz4MpICYmIFZhci5pcyjPhCkgJiYgZXEoz4MueCwgz4QueCkpIHtcbiAgICAgICAgICAgICAgIHJldHVybiB2YXJfKM+DLngsIGpvaW4oz4MuzrosIM+ELs66KSlcbiAgICAgICAgICAgIH0gZWxzZVxuICAgICAgICAgICAgaWYgKENvbnN0ci5pcyjPgykgJiYgQ29uc3RyLmlzKM+EKSkge1xuICAgICAgICAgICAgICAgLy8gQm90aCBtYXBzICh3aGljaCBhcmUgbm9uLWVtcHR5KSBjYW4gKGluZHVjdGl2ZWx5KSBiZSBhc3N1bWVkIHRvIGhhdmUga2V5cyB0YWtlbiBmcm9tIHRoZSBcbiAgICAgICAgICAgICAgIC8vIHNhbWUgZGF0YXR5cGUuIEVuc3VyZSB0aGF0IGludmFyaWFudCBpcyBwcmVzZXJ2ZWQ6XG4gICAgICAgICAgICAgICBjb25zdCBjX8+DOiBzdHJpbmcgPSDPgy5jYXNlcy50b0FycmF5KClbMF0uZnN0LnZhbCxcbiAgICAgICAgICAgICAgICAgICAgIGNfz4Q6IHN0cmluZyA9IM+ELmNhc2VzLnRvQXJyYXkoKVswXS5mc3QudmFsXG4gICAgICAgICAgICAgICBpZiAoY3RyVG9EYXRhVHlwZS5nZXQoY1/PgykgIT09IGN0clRvRGF0YVR5cGUuZ2V0KGNfz4QpKSB7XG4gICAgICAgICAgICAgICAgICBlcnJvcihgJHtjX8+DfSBhbmQgJHtjX8+EfSBhcmUgY29uc3RydWN0b3JzIG9mIGRpZmZlcmVudCBkYXRhdHlwZXMuYClcbiAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgIHJldHVybiBjb25zdHIodW5pb25XaXRoKM+DLmNhc2VzLCDPhC5jYXNlcywgam9pbikpXG4gICAgICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgICAgcmV0dXJuIGFic3VyZChcIlVuZGVmaW5lZCBqb2luLlwiLCDPgywgz4QpXG4gICAgICAgICAgICB9XG4gICAgICAgICB9XG4gICAgICB9XG5cbiAgICAgIGV4cG9ydCBjbGFzcyBDb25zdHI8SyBleHRlbmRzIENvbnQ+IGV4dGVuZHMgVHJpZTxLPiB7XG4gICAgICAgICBjYXNlczogRmluaXRlTWFwPEs+ID0gX1xuXG4gICAgICAgICBzdGF0aWMgaXM8SyBleHRlbmRzIENvbnQ+ICjPgzogVHJpZTxLPik6IM+DIGlzIENvbnN0cjxLPiB7XG4gICAgICAgICAgICByZXR1cm4gz4MgaW5zdGFuY2VvZiBDb25zdHJcbiAgICAgICAgIH1cbiAgICAgIH1cblxuICAgICAgZXhwb3J0IGZ1bmN0aW9uIGNvbnN0cjxLIGV4dGVuZHMgQ29udD4gKGNhc2VzOiBGaW5pdGVNYXA8Sz4pOiBDb25zdHI8Sz4ge1xuICAgICAgICAgcmV0dXJuIG1ha2UoQ29uc3RyLCBjYXNlcykgYXMgQ29uc3RyPEs+XG4gICAgICB9XG5cbiAgICAgIC8vIFRPRE86IHVzZSBhbm5vdGF0aW9ucyBvbiB4LlxuICAgICAgZXhwb3J0IGNsYXNzIFZhcjxLIGV4dGVuZHMgQ29udD4gZXh0ZW5kcyBUcmllPEs+IHtcbiAgICAgICAgIHg6IEFubm90YXRlZDxTdHI+ID0gX1xuICAgICAgICAgzro6IEsgPSBfXG5cbiAgICAgICAgIHN0YXRpYyBpczxLIGV4dGVuZHMgQ29udD4gKM+DOiBUcmllPEs+KTogz4MgaXMgVmFyPEs+IHtcbiAgICAgICAgICAgIHJldHVybiDPgyBpbnN0YW5jZW9mIFZhclxuICAgICAgICAgfVxuICAgICAgfVxuXG4gICAgICBleHBvcnQgZnVuY3Rpb24gdmFyXzxLIGV4dGVuZHMgQ29udD4gKHg6IEFubm90YXRlZDxTdHI+LCDOujogSyk6IFZhcjxLPiB7XG4gICAgICAgICByZXR1cm4gbWFrZShWYXIsIHgsIM66KSBhcyBWYXI8Sz5cbiAgICAgIH1cbiAgIH1cbn1cbiIsImltcG9ydCB7IGFic3VyZCB9IGZyb20gXCIuL3V0aWwvQ29yZVwiXG5pbXBvcnQgeyBFbXB0eSwgTm9uRW1wdHksIFBhaXIsIFRyZWUsIGVtcHR5LCBub25FbXB0eSwgcGFpciB9IGZyb20gXCIuL0Jhc2VUeXBlc1wiXG5pbXBvcnQgeyBQZXJzaXN0ZW50LCBTdHIgfSBmcm9tIFwiLi9WYWx1ZVwiXG5cbi8vIFNpbXBsaWZ5IHRvIGtleXMgb2YgdHlwZSBTdHIgdW50aWwgd2UgaGF2ZSBzb21ldGhpbmcgbGlrZSB0eXBlIGNsYXNzZXMuXG5leHBvcnQgdHlwZSBGaW5pdGVNYXA8Vj4gPSBUcmVlPFBhaXI8U3RyLCBWPj5cblxuZXhwb3J0IGZ1bmN0aW9uIGdldCA8ViBleHRlbmRzIFBlcnNpc3RlbnQ+IChtOiBGaW5pdGVNYXA8Vj4sIGs6IFN0cik6IFYgfCB1bmRlZmluZWQge1xuICAgaWYgKE5vbkVtcHR5LmlzKG0pKSB7XG4gICAgICBpZiAoay5sZXEobS50LmZzdCkpIHtcbiAgICAgICAgIGlmIChtLnQuZnN0LmxlcShrKSkge1xuICAgICAgICAgICAgcmV0dXJuIG0udC5zbmRcbiAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICByZXR1cm4gZ2V0KG0ubGVmdCwgaylcbiAgICAgICAgIH1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICByZXR1cm4gZ2V0KG0ucmlnaHQsIGspXG4gICAgICB9XG4gICB9IGVsc2VcbiAgIGlmIChFbXB0eS5pcyhtKSkge1xuICAgICAgcmV0dXJuIHVuZGVmaW5lZFxuICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBhYnN1cmQoKVxuICAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gaW5zZXJ0IDxWIGV4dGVuZHMgUGVyc2lzdGVudD4gKG06IEZpbml0ZU1hcDxWPiwgazogU3RyLCB2OiBWKTogRmluaXRlTWFwPFY+IHtcbiAgIGlmIChOb25FbXB0eS5pcyhtKSkge1xuICAgICAgaWYgKGsubGVxKG0udC5mc3QpKSB7XG4gICAgICAgICBpZiAobS50LmZzdC5sZXEoaykpIHtcbiAgICAgICAgICAgIHJldHVybiBub25FbXB0eShtLmxlZnQsIHBhaXIoaywgdiksIG0ucmlnaHQpXG4gICAgICAgICB9IGVsc2Uge1xuICAgICAgICAgICAgcmV0dXJuIG5vbkVtcHR5KGluc2VydChtLmxlZnQsIGssIHYpLCBtLnQsIG0ucmlnaHQpXG4gICAgICAgICB9XG4gICAgICB9IGVsc2Uge1xuICAgICAgICAgcmV0dXJuIG5vbkVtcHR5KG0ubGVmdCwgbS50LCBpbnNlcnQobS5yaWdodCwgaywgdikpXG4gICAgICB9XG4gICB9IGVsc2VcbiAgIGlmIChFbXB0eS5pcyhtKSkge1xuICAgICAgcmV0dXJuIG5vbkVtcHR5KG0sIHBhaXIoaywgdiksIG0pXG4gICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGFic3VyZCgpXG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBzaW5nbGV0b24gPFYgZXh0ZW5kcyBQZXJzaXN0ZW50PiAoazogU3RyLCB2OiBWKTogRmluaXRlTWFwPFY+IHtcbiAgIHJldHVybiBpbnNlcnQoZW1wdHkoKSwgaywgdilcbn1cblxuLy8gVW5pb24gd2l0aCBhIGNvbWJpbmluZyBmdW5jdGlvbi4gQXZvaWQgcHJpbWVzIGluIHNpZ25hdHVyZTsgc2VlbXMgdG8gYmUgaW5jb21wYXRpYmxlIHdpdGggdmVyc2lvbiBcbi8vIG9mIHRzLWxvYWRlciB1c2VkIGJ5IFdyYXR0bGVyLlxuZXhwb3J0IGZ1bmN0aW9uIHVuaW9uV2l0aCA8ViBleHRlbmRzIFBlcnNpc3RlbnQsIFQgZXh0ZW5kcyBGaW5pdGVNYXA8Vj4+IChtMTogVCwgbTI6IFQsIGY6ICh2MTogViwgdjI6IFYpID0+IFYpOiBUIHtcbiAgIGlmIChOb25FbXB0eS5pcyhtMikpIHtcbiAgICAgIGNvbnN0IGs6IFN0ciA9IG0yLnQuZnN0LFxuICAgICAgICAgICAgdjogViA9IG0yLnQuc25kLFxuICAgICAgICAgICAgdsq5OiBWIHwgdW5kZWZpbmVkID0gZ2V0KG0xLCBrKSxcbiAgICAgICAgICAgIHU6IFYgPSB2yrkgPT09IHVuZGVmaW5lZCA/IHYgOiBmKHYsIHbKuSlcbiAgICAgIHJldHVybiB1bmlvbldpdGgoaW5zZXJ0KHVuaW9uV2l0aChtMSwgbTIubGVmdCwgZiksIGssIHUpLCBtMi5yaWdodCwgZikgYXMgVFxuICAgfSBlbHNlXG4gICBpZiAoRW1wdHkuaXMobTIpKSB7XG4gICAgICByZXR1cm4gbTFcbiAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gYWJzdXJkKClcbiAgIH1cbn1cbiIsImltcG9ydCB7IExpc3QgfSBmcm9tIFwiLi9CYXNlVHlwZXNcIlxuaW1wb3J0IHsgaW5pdERhdGFUeXBlIH0gZnJvbSBcIi4vRGF0YVR5cGVcIlxuaW1wb3J0IHsgRGF0YVZhbHVlIH0gZnJvbSBcIi4vRGF0YVZhbHVlXCJcbmltcG9ydCB7IE51bSwgU3RyLCBfIH0gZnJvbSBcIi4vVmFsdWVcIlxuXG4vLyBCYXNpYyBncmFwaGljYWwgZGF0YXR5cGVzLlxuXG5leHBvcnQgY2xhc3MgUmVjdCBleHRlbmRzIERhdGFWYWx1ZTxcIlJlY3RcIj4ge1xuICAgd2lkdGg6IE51bSA9IF9cbiAgIGhlaWdodDogTnVtID0gX1xufVxuXG5leHBvcnQgY2xhc3MgUG9pbnQgZXh0ZW5kcyBEYXRhVmFsdWU8XCJQb2ludFwiPiB7XG4gICB4OiBOdW0gPSBfXG4gICB5OiBOdW0gPSBfXG5cbiAgIHRvU3RyaW5nKCk6IHN0cmluZyB7XG4gICAgICByZXR1cm4gYFBvaW50KCR7dGhpcy54fSwke3RoaXMueX0pYFxuICAgfVxufVxuXG5leHBvcnQgdHlwZSBHcmFwaGljc0VsZW1lbnRUYWcgPSBcIkdyYXBoaWNcIiB8IFwiUG9seWxpbmVcIiB8IFwiUG9seWdvblwiIHwgXCJUZXh0XCIgfCBcIlRyYW5zbGF0ZVwiXG5cbmV4cG9ydCBhYnN0cmFjdCBjbGFzcyBHcmFwaGljc0VsZW1lbnQ8VGFnIGV4dGVuZHMgR3JhcGhpY3NFbGVtZW50VGFnID0gR3JhcGhpY3NFbGVtZW50VGFnPiBleHRlbmRzIERhdGFWYWx1ZTxUYWc+IHtcbn1cblxuZXhwb3J0IGNsYXNzIEdyYXBoaWMgZXh0ZW5kcyBHcmFwaGljc0VsZW1lbnQ8XCJHcmFwaGljXCI+IHtcbiAgIGdzOiBMaXN0PEdyYXBoaWNzRWxlbWVudD4gPSBfXG59XG5cbmV4cG9ydCBjbGFzcyBQb2x5bGluZSBleHRlbmRzIEdyYXBoaWNzRWxlbWVudDxcIlBvbHlsaW5lXCI+IHtcbiAgIHBvaW50czogTGlzdDxQb2ludD4gPSBfXG59XG5cbmV4cG9ydCBjbGFzcyBQb2x5Z29uIGV4dGVuZHMgR3JhcGhpY3NFbGVtZW50PFwiUG9seWdvblwiPiB7XG4gICBwb2ludHM6IExpc3Q8UG9pbnQ+ID0gX1xuICAgc3Ryb2tlOiBTdHIgPSBfXG4gICBmaWxsOiBTdHIgPSBfXG59XG5cbmV4cG9ydCBjbGFzcyBUZXh0IGV4dGVuZHMgR3JhcGhpY3NFbGVtZW50PFwiVGV4dFwiPiB7XG4gICB4OiBOdW0gPSBfXG4gICB5OiBOdW0gPSBfXG4gICBzdHI6IFN0ciA9IF9cbn1cblxuLy8gT21pdCBzY2FsaW5nLCByb3RhdGlvbiwgZXRjIGZvciBub3c7IHdvdWxkIHJlcXVpcmUgZXh0ZXJuYWxpc2F0aW9uIHRvIFNWRyB0byBoYW5kbGUgdGV4dCBwcm9wZXJseS5cbmV4cG9ydCBjbGFzcyBUcmFuc2xhdGUgZXh0ZW5kcyBHcmFwaGljc0VsZW1lbnQ8XCJUcmFuc2xhdGVcIj4ge1xuICAgeDogTnVtID0gX1xuICAgeTogTnVtID0gX1xuICAgZzogR3JhcGhpY3NFbGVtZW50ID0gX1xufVxuXG5pbml0RGF0YVR5cGUoR3JhcGhpY3NFbGVtZW50LCBbUG9seWdvbiwgUG9seWxpbmUsIFRleHQsIFRyYW5zbGF0ZSwgR3JhcGhpY10pXG5pbml0RGF0YVR5cGUoUG9pbnQsIFtQb2ludF0pXG5pbml0RGF0YVR5cGUoUmVjdCwgW1JlY3RdKVxuIiwiLy8gXCJ0eXBpbmdzXCIgcHJvcGVydHkgb2YgcGFja2FnZS5qc29uIHJlcXVpcmVzIGEgc2luZ2xlIFR5cGVTY3JpcHQgaW50ZXJmYWNlLlxuaW1wb3J0IHsgRXZhbCB9IGZyb20gXCIuL0V2YWxcIlxuaW1wb3J0IHsgcGFyc2VXaXRoSW1wb3J0cyB9IGZyb20gXCIuL01vZHVsZVwiXG5cbmV4cG9ydCB7IEV2YWwsIHBhcnNlV2l0aEltcG9ydHMgfVxuIiwiaW1wb3J0IHsgQ2xhc3MsIF9fbm9uTnVsbCwgYWJzdXJkLCBhc3NlcnQsIGNsYXNzTmFtZSwgZXJyb3IgfSBmcm9tIFwiLi91dGlsL0NvcmVcIlxuaW1wb3J0IHsgQW5ub3RhdGlvbiwgYW5uIH0gZnJvbSBcIi4vdXRpbC9MYXR0aWNlXCJcbmltcG9ydCB7IEFubm90YXRlZCwgYXNBbm5vdGF0ZWQsIHNldM6xIH0gZnJvbSBcIi4vQW5ub3RhdGVkXCJcbmltcG9ydCB7IExpc3QsIFBhaXIsIGNvbnMsIG5pbCB9IGZyb20gXCIuL0Jhc2VUeXBlc1wiXG5pbXBvcnQgeyBEYXRhVmFsdWUgfSBmcm9tIFwiLi9EYXRhVmFsdWVcIlxuaW1wb3J0IHsgRGF0YVR5cGUsIGN0clRvRGF0YVR5cGUsIGVsaW1Ub0RhdGFUeXBlIH0gZnJvbSBcIi4vRGF0YVR5cGVcIlxuaW1wb3J0IHsgRW52LCBlbXB0eUVudiB9IGZyb20gXCIuL0VudlwiXG5pbXBvcnQgeyBFeHByIH0gZnJvbSBcIi4vRXhwclwiXG5pbXBvcnQgeyBTdHIsIFZhbHVlLCBfLCBtYWtlIH0gZnJvbSBcIi4vVmFsdWVcIlxuXG5pbXBvcnQgQ29udCA9IEV4cHIuQ29udFxuaW1wb3J0IFRyaWUgPSBFeHByLlRyaWVcblxudHlwZSBSdW50aW1lQ29udCA9IEV4cHIgfCBEYXRhVmFsdWU8XCJFbGltXCI+XG5cbi8vIENvbmNlcHR1YWxseSAoc3ludGFjdGljKSB0cmllcyBtYXAgdG8gKHNlbWFudGljKSBlbGltIGZvcm1zLCBhbmQgZXhwcnMgbWFwIHRvIGV4cHJzOyBubyBlYXN5IHdheSB0byBcbi8vIGV4cHJlc3MgdGhpcyBpbiB0aGUgdHlwZSBzeXN0ZW0uXG5leHBvcnQgZnVuY3Rpb24gZXZhbFRyaWUgKM+DOiBUcmllPEV4cHI+KTogRWxpbTxFeHByPiB7XG4gICByZXR1cm4gZXZhbFRyaWVfKM+DKSBhcyBFbGltPEV4cHI+XG59XG5cbmZ1bmN0aW9uIGV2YWxUcmllXzxLIGV4dGVuZHMgQ29udD4gKM+DOiBUcmllPEs+KTogRWxpbSB7XG4gICBpZiAoVHJpZS5WYXIuaXMoz4MpKSB7XG4gICAgICByZXR1cm4gdmFyRWxpbSjPgy54LCBldmFsQ29udCjPgy7OuikpXG4gICB9IGVsc2VcbiAgIGlmIChUcmllLkNvbnN0ci5pcyjPgykpIHtcbiAgICAgIGNvbnN0IGNhc2VzOiBQYWlyPFN0ciwgSz5bXSA9IM+DLmNhc2VzLnRvQXJyYXkoKSxcbiAgICAgICAgICAgIGPMhTogc3RyaW5nW10gPSBjYXNlcy5tYXAoKHsgZnN0OiBjIH0pID0+IGMudmFsKSxcbiAgICAgICAgICAgIGQ6IERhdGFUeXBlID0gX19ub25OdWxsKGN0clRvRGF0YVR5cGUuZ2V0KGPMhVswXSkpLFxuICAgICAgICAgICAgY8yFyrk6IHN0cmluZ1tdID0gWy4uLmQuY3Rycy5rZXlzKCldLCAvLyBhbHNvIHNvcnRlZFxuICAgICAgICAgICAgZsyFOiBSdW50aW1lQ29udFtdID0gW11cbiAgICAgIGxldCBuOiBudW1iZXIgPSAwXG4gICAgICBmb3IgKGxldCBuyrk6IG51bWJlciA9IDA7IG7KuSA8IGPMhcq5Lmxlbmd0aDsgKytuyrkpIHtcbiAgICAgICAgIGlmIChjzIUuaW5jbHVkZXMoY8yFyrlbbsq5XSkpIHtcbiAgICAgICAgICAgIGbMhS5wdXNoKGV2YWxDb250KGNhc2VzW24rK10uc25kKSlcbiAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICBmzIUucHVzaCh1bmRlZmluZWQgYXMgYW55KVxuICAgICAgICAgfVxuICAgICAgfVxuICAgICAgYXNzZXJ0KG4gPT09IGNhc2VzLmxlbmd0aClcbiAgICAgIHJldHVybiBtYWtlKGQuZWxpbUMgYXMgQ2xhc3M8RGF0YUVsaW0+LCAuLi5mzIUpXG4gICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGFic3VyZCgpXG4gICB9XG59XG5cbmZ1bmN0aW9uIGV2YWxDb250PEsgZXh0ZW5kcyBDb250PiAozro6IEspOiBSdW50aW1lQ29udCB7XG4gICBpZiAozrogaW5zdGFuY2VvZiBUcmllLlRyaWUpIHtcbiAgICAgIGNvbnN0IM+DOiBUcmllPEs+ID0gzrpcbiAgICAgIHJldHVybiBldmFsVHJpZSjPgylcbiAgIH0gZWxzZVxuICAgaWYgKM66IGluc3RhbmNlb2YgRXhwci5FeHByKSB7XG4gICAgICByZXR1cm4gzrpcbiAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gYWJzdXJkKClcbiAgIH1cbn1cblxuLy8gUHJlb3JkZXIgdHJhdmVyc2FsIG9mIGFsbCBub2RlcyBpbiB0aGUgbWF0Y2hlZCBwcmVmaXguXG50eXBlIE1hdGNoUHJlZml4ID0gTGlzdDxBbm5vdGF0ZWQ8VmFsdWU+PlxuXG5leHBvcnQgY2xhc3MgTWF0Y2g8Sz4gZXh0ZW5kcyBEYXRhVmFsdWU8XCJNYXRjaFwiPiB7XG4gICB2zIU6IE1hdGNoUHJlZml4ID0gX1xuICAgzro6IEsgPSBfXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtYXRjaDxLIGV4dGVuZHMgUnVudGltZUNvbnQ+ICjOvjogTWF0Y2hQcmVmaXgsIM66OiBLKTogTWF0Y2g8Sz4ge1xuICAgcmV0dXJuIG1ha2UoTWF0Y2gsIM6+LCDOuikgYXMgTWF0Y2g8Sz5cbn1cblxuLy8gU2VlIEdpdEh1YiBpc3N1ZSAjMTI4LlxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIEVsaW08SyBleHRlbmRzIFJ1bnRpbWVDb250ID0gUnVudGltZUNvbnQ+IGV4dGVuZHMgRGF0YVZhbHVlPFwiRWxpbVwiPiB7XG4gICAvLyBjb3VsZCBoYXZlIGNhbGxlZCB0aGlzIFwibWF0Y2hcIiwgYnV0IGNvbmZsaWN0cyB3aXRoIGZhY3RvcnkgbWV0aG9kIG9mIHNhbWUgbmFtZVxuICAgYXBwbHkgKHY6IFZhbHVlKTogW0VudiwgTWF0Y2g8Sz5dIHtcbiAgICAgIHJldHVybiB0aGlzLmFwcGx5Xyh2LCBuaWwoKSlcbiAgIH1cblxuICAgYWJzdHJhY3QgYXBwbHlfICh2OiBWYWx1ZSwgzr46IE1hdGNoUHJlZml4KTogW0VudiwgTWF0Y2g8Sz5dXG59XG5cbi8vIFBhcnNlciBlbnN1cmVzIGNvbnN0cnVjdG9yIGNhbGxzIGFyZSBzYXR1cmF0ZWQuXG5mdW5jdGlvbiBtYXRjaEFyZ3MgKM66OiBSdW50aW1lQ29udCwgdsyFOiBWYWx1ZVtdLCB1zIU6IE1hdGNoUHJlZml4KTogW0VudiwgTWF0Y2g8UnVudGltZUNvbnQ+XSB7XG4gICBpZiAodsyFLmxlbmd0aCA9PT0gMCkge1xuICAgICAgcmV0dXJuIFtlbXB0eUVudigpLCBtYXRjaCh1zIUsIM66KV1cbiAgIH0gZWxzZSB7XG4gICAgICBjb25zdCBbdiwgLi4udsyFyrldID0gdsyFXG4gICAgICBpZiAozrogaW5zdGFuY2VvZiBFbGltKSB7XG4gICAgICAgICBjb25zdCBmOiBFbGltID0gzrosIC8vIFwidW5mb2xkXCIgSyBpbnRvIEVsaW08Sz5cbiAgICAgICAgICAgICAgIFvPgSwgzr5dOiBbRW52LCBNYXRjaDxSdW50aW1lQ29udD5dID0gZi5hcHBseV8odiwgdcyFKSxcbiAgICAgICAgICAgICAgIFvPgcq5LCDOvsq5XTogW0VudiwgTWF0Y2g8UnVudGltZUNvbnQ+XSA9IG1hdGNoQXJncyjOvi7OuiwgdsyFyrksIM6+LnbMhSlcbiAgICAgICAgIHJldHVybiBbz4EuY29uY2F0KM+ByrkpLCDOvsq5XVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHJldHVybiBhYnN1cmQoXCJUb28gbWFueSBhcmd1bWVudHMgdG8gY29uc3RydWN0b3IuXCIpXG4gICAgICB9XG4gICB9XG59XG5cbi8vIE5vIG5lZWQgdG8gcGFyYW1ldGVyaXNlIHRoZXNlIHR3byBjbGFzZXNlcyBvdmVyIHN1YnR5cGVzIG9mIFJ1bnRpbWVDb250IGJlY2F1c2Ugb25seSBldmVyIHVzZSB0aGVtIGF0IFJ1bnRpbWVDb250IFxuLy8gaXRzZWxmLiBDb25jcmV0ZSBpbnN0YW5jZXMgaGF2ZSBhIGZpZWxkIHBlciBjb25zdHJ1Y3RvciwgaW4gKmxleGljb2dyYXBoaWNhbCogb3JkZXIuXG5leHBvcnQgYWJzdHJhY3QgY2xhc3MgRGF0YUVsaW0gZXh0ZW5kcyBFbGltIHtcbiAgIGFwcGx5XyAodjogQW5ub3RhdGVkPFZhbHVlPiwgdcyFOiBNYXRjaFByZWZpeCk6IFtFbnYsIE1hdGNoPFJ1bnRpbWVDb250Pl0ge1xuICAgICAgY29uc3QgYzogc3RyaW5nID0gY2xhc3NOYW1lKHYpXG4gICAgICBpZiAodiBpbnN0YW5jZW9mIERhdGFWYWx1ZSkge1xuICAgICAgICAgY29uc3Qgzro6IFJ1bnRpbWVDb250ID0gKHRoaXMgYXMgYW55KVtjXSBhcyBSdW50aW1lQ29udFxuICAgICAgICAgaWYgKM66ICE9PSB1bmRlZmluZWQpIHtcbiAgICAgICAgICAgIGNvbnN0IHbMhTogQW5ub3RhdGVkPFZhbHVlPltdID0gKHYgYXMgRGF0YVZhbHVlKS5maWVsZFZhbHVlcygpLm1hcCh2ID0+IGFzQW5ub3RhdGVkKHYpKSxcbiAgICAgICAgICAgIFvPgSwgzr5dOiBbRW52LCBNYXRjaDxSdW50aW1lQ29udD5dID0gbWF0Y2hBcmdzKM66LCB2zIUsIHXMhSlcbiAgICAgICAgICAgIHJldHVybiBbz4EsIG1hdGNoKGNvbnModiwgzr4udsyFKSwgzr4uzropXVxuICAgICAgICAgfSBlbHNlIHtcbiAgICAgICAgICAgIGNvbnN0IGQ6IERhdGFUeXBlID0gZWxpbVRvRGF0YVR5cGUuZ2V0KGNsYXNzTmFtZSh0aGlzKSkhXG4gICAgICAgICAgICBpZiAoZC5jdHJzLmhhcyhjKSkge1xuICAgICAgICAgICAgICAgcmV0dXJuIGVycm9yKGBQYXR0ZXJuIG1pc21hdGNoOiAke2N9IGNhc2UgaXMgdW5kZWZpbmVkIGZvciAke2QubmFtZS52YWx9IGVsaW1pbmF0b3IuYClcbiAgICAgICAgICAgIH0gZWxzZSB7XG4gICAgICAgICAgICAgICByZXR1cm4gZXJyb3IoYFBhdHRlcm4gbWlzbWF0Y2g6IGZvdW5kICR7Y30sIGV4cGVjdGVkICR7ZC5uYW1lLnZhbH0uYClcbiAgICAgICAgICAgIH1cbiAgICAgICAgIH1cbiAgICAgIH0gZWxzZSB7XG4gICAgICAgICByZXR1cm4gZXJyb3IoYFBhdHRlcm4gbWlzbWF0Y2g6ICR7Y30gaXMgbm90IGEgZGF0YXR5cGUuYCwgdiwgdGhpcylcbiAgICAgIH1cbiAgIH1cbn1cblxuY2xhc3MgVmFyRWxpbSBleHRlbmRzIEVsaW0ge1xuICAgeDogQW5ub3RhdGVkPFN0cj4gPSBfXG4gICDOujogUnVudGltZUNvbnQgPSBfXG5cbiAgIGFwcGx5XyAodjogQW5ub3RhdGVkPFZhbHVlPiwgzr46IE1hdGNoUHJlZml4KTogW0VudiwgTWF0Y2g8UnVudGltZUNvbnQ+XSB7XG4gICAgICByZXR1cm4gW0Vudi5zaW5nbGV0b24odGhpcy54LCB2KSwgbWF0Y2gozr4sIHRoaXMuzropXVxuICAgfVxufVxuXG5mdW5jdGlvbiB2YXJFbGltPEsgZXh0ZW5kcyBSdW50aW1lQ29udD4gKHg6IEFubm90YXRlZDxTdHI+LCDOujogUnVudGltZUNvbnQpOiBWYXJFbGltIHtcbiAgIHJldHVybiBtYWtlKFZhckVsaW0sIHgsIM66KSBhcyBWYXJFbGltXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtYXRjaF9md2QgKM6+OiBNYXRjaDxFeHByPik6IEFubm90YXRpb24ge1xuICAgcmV0dXJuIM6+LnbMhS50b0FycmF5KCkucmVkdWNlKCjOsTogQW5ub3RhdGlvbiwgdjogQW5ub3RhdGVkPFZhbHVlPik6IEFubm90YXRpb24gPT4gYW5uLm1lZXQozrEsIHYuX1/OsSksIGFubi50b3ApXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtYXRjaF9id2QgKM6+OiBNYXRjaDxFeHByPiwgzrE6IEFubm90YXRpb24pOiB2b2lkIHtcbiAgIM6+LnbMhS50b0FycmF5KCkubWFwKHYgPT4gc2V0zrEozrEsIHYpKVxufVxuIiwiaW1wb3J0IHsgR3JhbW1hciwgUGFyc2VyIH0gZnJvbSBcIm5lYXJsZXlcIlxuaW1wb3J0IHsgX19ub25OdWxsLCBhcywgZXJyb3IgfSBmcm9tIFwiLi91dGlsL0NvcmVcIlxuaW1wb3J0IHsgc3RyIH0gZnJvbSBcIi4vQW5ub3RhdGVkXCJcbmltcG9ydCB7IExpc3QgfSBmcm9tIFwiLi9CYXNlVHlwZXNcIlxuaW1wb3J0IHsgRW52LCBFeHRlbmRFbnYsIGVtcHR5RW52IH0gZnJvbSBcIi4vRW52XCJcbmltcG9ydCB7IEV2YWwgfSBmcm9tIFwiLi9FdmFsXCJcbmltcG9ydCB7IEV4cHIgfSBmcm9tIFwiLi9FeHByXCJcbmltcG9ydCBcIi4vR3JhcGhpY3NcIiAvLyBmb3IgZGF0YXR5cGVzXG5pbXBvcnQgKiBhcyBncmFtbWFyIGZyb20gXCIuL1BhcnNlXCJcbmltcG9ydCB7IM69IH0gZnJvbSBcIi4vVmVyc2lvbmVkXCJcblxuLy8gS2luZGVyZ2FydGVuIG1vZHVsZXMuXG50eXBlIE1vZHVsZSA9IExpc3Q8RXhwci5EZWY+XG5cbi8vIERlZmluZSBhcyBjb25zdGFudHMgdG8gZW5mb3JjZSBzaGFyaW5nOyBjb3VsZCB1c2UgbWVtb2lzYXRpb24uXG5leHBvcnQgY29uc3QgbW9kdWxlX3ByZWx1ZGU6IE1vZHVsZSA9IGxvYWRNb2R1bGUoXCJwcmVsdWRlXCIpLFxuICAgICAgICAgICAgIG1vZHVsZV9ncmFwaGljczogTW9kdWxlID0gbG9hZE1vZHVsZShcImdyYXBoaWNzXCIpLFxuICAgICAgICAgICAgIG1vZHVsZV9yZW5kZXJEYXRhOiBNb2R1bGUgPSBsb2FkTW9kdWxlKFwicmVuZGVyRGF0YVwiKVxuXG5mdW5jdGlvbiBpbXBvcnRfIChtb2R1bGVzOiBNb2R1bGVbXSwgZTogRXhwcik6IEV4cHIge1xuICAgaWYgKG1vZHVsZXMubGVuZ3RoID09PSAwKSB7XG4gICAgICByZXR1cm4gZVxuICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBFeHByLmRlZnMozr0oKSwgbW9kdWxlc1swXSwgaW1wb3J0Xyhtb2R1bGVzLnNsaWNlKDEpLCBlKSlcbiAgIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGltcG9ydERlZmF1bHRzIChlOiBFeHByKTogRXhwciB7XG4gICByZXR1cm4gaW1wb3J0XyhbbW9kdWxlX3ByZWx1ZGVdLCBlKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gbG9hZFRlc3RGaWxlIChmb2xkZXI6IHN0cmluZywgZmlsZTogc3RyaW5nKTogc3RyaW5nIHtcbiAgIGxldCB0ZXh0OiBzdHJpbmdcbiAgIGNvbnN0IHhtbGh0dHA6IFhNTEh0dHBSZXF1ZXN0ID0gbmV3IFhNTEh0dHBSZXF1ZXN0XG4gICB4bWxodHRwLm9wZW4oXCJHRVRcIiwgXCIuL1wiICsgZm9sZGVyICsgXCIvXCIgKyBmaWxlICsgXCIubGNhbGNcIiwgZmFsc2UpXG4gICB4bWxodHRwLnNlbmQoKVxuICAgaWYgKHhtbGh0dHAuc3RhdHVzID09PSAyMDApIHtcbiAgICAgIHRleHQgPSB4bWxodHRwLnJlc3BvbnNlVGV4dFxuICAgfVxuICAgcmV0dXJuIF9fbm9uTnVsbCh0ZXh0ISlcbn1cblxuLy8gTm90IHN1cmUgaWYgTmVhcmxleSBjYW4gcGFyc2UgYXJiaXRyYXJ5IG5vbi10ZXJtaW5hbCwgYXMgb3Bwb3NlZCB0byByb290LlxuZXhwb3J0IGZ1bmN0aW9uIGxvYWRNb2R1bGUgKGZpbGU6IHN0cmluZyk6IE1vZHVsZSB7XG4gICBjb25zdCBmaWxlyrk6IHN0cmluZyA9IGxvYWRUZXN0RmlsZShcImxjYWxjL2xpYlwiLCBmaWxlKSArIFwiIGluIDBcIixcbiAgICAgICAgIGU6IEV4cHIuRGVmcyA9IGFzKHN1Y2Nlc3NmdWxQYXJzZShmaWxlyrkpLCBFeHByLkRlZnMpXG4gICByZXR1cm4gZS5kZWbMhVxufVxuXG5leHBvcnQgZnVuY3Rpb24gb3BlbiAoZmlsZTogc3RyaW5nKTogRXhwciB7XG4gICByZXR1cm4gb3BlbldpdGhJbXBvcnRzKGZpbGUsIFtdKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gb3BlbldpdGhJbXBvcnRzIChmaWxlOiBzdHJpbmcsIG1vZHVsZXM6IE1vZHVsZVtdKTogRXhwciB7XG4gICByZXR1cm4gcGFyc2VXaXRoSW1wb3J0cyhsb2FkVGVzdEZpbGUoXCJsY2FsYy9leGFtcGxlXCIsIGZpbGUpLCBtb2R1bGVzKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gb3BlbkRhdGFzZXRBcyAoZmlsZTogc3RyaW5nLCB4OiBzdHJpbmcpOiBFeHRlbmRFbnYge1xuICAgY29uc3QgZTogRXhwciA9IHBhcnNlV2l0aEltcG9ydHMobG9hZFRlc3RGaWxlKFwibGNhbGMvZGF0YXNldFwiLCBmaWxlKSwgW10pXG4gICByZXR1cm4gRW52LnNpbmdsZXRvbihzdHIoeCksIEV2YWwuZXZhbF8oZW1wdHlFbnYoKSwgZSkudilcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHBhcnNlV2l0aEltcG9ydHMgKHNyYzogc3RyaW5nLCBtb2R1bGVzOiBNb2R1bGVbXSk6IEV4cHIge1xuICAgcmV0dXJuIGltcG9ydERlZmF1bHRzKGltcG9ydF8obW9kdWxlcywgc3VjY2Vzc2Z1bFBhcnNlKHNyYykpKVxufVxuXG4vLyBodHRwczovL2dpdGh1Yi5jb20va2FjaC9uZWFybGV5L2lzc3Vlcy8yNzYjaXNzdWVjb21tZW50LTMyNDE2MjIzNFxuZXhwb3J0IGZ1bmN0aW9uIHN1Y2Nlc3NmdWxQYXJzZSAoc3RyOiBzdHJpbmcpOiBFeHByIHtcbiAgIGNvbnN0IHJlc3VsdHM6IGFueVtdID0gbmV3IFBhcnNlcihHcmFtbWFyLmZyb21Db21waWxlZChncmFtbWFyKSkuZmVlZChzdHIpLnJlc3VsdHNcbiAgIGlmIChyZXN1bHRzLmxlbmd0aCA+IDEpIHtcbiAgICAgIGVycm9yKFwiQW1iaWd1b3VzIHBhcnNlLlwiKVxuICAgfVxuICAgcmV0dXJuIHJlc3VsdHNbMF1cbn1cbiIsIi8vIEdlbmVyYXRlZCBhdXRvbWF0aWNhbGx5IGJ5IG5lYXJsZXksIHZlcnNpb24gMi4xNi4wXG4vLyBodHRwOi8vZ2l0aHViLmNvbS9IYXJkbWF0aDEyMy9uZWFybGV5XG4vLyBCeXBhc3NlcyBUUzYxMzMuIEFsbG93IGRlY2xhcmVkIGJ1dCB1bnVzZWQgZnVuY3Rpb25zLlxuLy8gQHRzLWlnbm9yZVxuZnVuY3Rpb24gaWQoZDogYW55W10pOiBhbnkgeyByZXR1cm4gZFswXTsgfVxuZGVjbGFyZSB2YXIgd2hpdGVzcGFjZTogYW55O1xuZGVjbGFyZSB2YXIgc2luZ2xlTGluZUNvbW1lbnQ6IGFueTtcbmRlY2xhcmUgdmFyIGlkZW50OiBhbnk7XG5kZWNsYXJlIHZhciBzdHJpbmc6IGFueTtcbmRlY2xhcmUgdmFyIG51bWJlcjogYW55O1xuZGVjbGFyZSB2YXIgY29tcGFyZU9wOiBhbnk7XG5kZWNsYXJlIHZhciBleHBvbmVudE9wOiBhbnk7XG5kZWNsYXJlIHZhciBwcm9kdWN0T3A6IGFueTtcbmRlY2xhcmUgdmFyIHN1bU9wOiBhbnk7XG5cbmNvbnN0IG1vbyA9IHJlcXVpcmUoJ21vbycpXG5jb25zdCBsZXhlciA9IG1vby5jb21waWxlKHtcbiAgIGlkZW50OiB7XG4gICAgICBtYXRjaDogL1thLXpBLVpfXVswLTlhLXpBLVpfXSonKi8sIC8vIGdyZWVkeVxuICAgICAgdHlwZTogbW9vLmtleXdvcmRzKHtcbiAgICAgICAga2V5d29yZDogW1wiYXNcIiwgXCJtYXRjaFwiLCBcImZ1blwiLCBcImluXCIsIFwibGV0XCIsIFwibGV0cmVjXCIsIFwicHJpbWl0aXZlXCIsIFwidHlwZW1hdGNoXCJdLFxuICAgICAgfSlcbiAgIH0sXG4gICB3aGl0ZXNwYWNlOiB7XG4gICAgICBtYXRjaDogL1sgXFxmXFx0XFxyXFxuXSsvLFxuICAgICAgbGluZUJyZWFrczogdHJ1ZVxuICAgfSxcbiAgIHNpbmdsZUxpbmVDb21tZW50OiAvXFwvXFwvLiokLyxcbiAgIC8vIEpTT04gZ3JhbW1hciBmb3IgbnVtYmVycywgaHR0cHM6Ly90b29scy5pZXRmLm9yZy9odG1sL3JmYzcxNTkuaHRtbCNzZWN0aW9uLTYuXG4gICAvLyBTZWVtcyBNb28gcmVxdWlyZXMgdXMgdG8gdXNlIG5vbi1jYXB0dXJpbmcgZ3JvdXBzICg/OilcbiAgIG51bWJlcjogL1xcLT8oPzowfFsxLTldWzAtOV0qKSg/OlxcLlswLTldKyk/KD86W2V8RV1bLXwrXT9bMC05XSspPy8sXG4gICBzdHJpbmc6IC9cIig/OlxcXFxbXCJcXFxcXXxbXlxcblwiXFxcXF0pKlwiLyxcbiAgIC8vIG5vdCBxdWl0ZSBzdXJlIHdoeSBJIGNhbid0IHVzZSBsaXRlcmFscyBoZXJlOlxuICAgc3VtT3A6IC9cXC18XFwrXFwrfFxcKy8sXG4gICBleHBvbmVudE9wOiAvXFwqXFwqLyxcbiAgIHByb2R1Y3RPcDogL1xcKnxcXC8vLCAvLyBtdXN0IGNvbWUgYWZ0ZXIgZXhwb25lbnRPcFxuICAgY29tcGFyZU9wOiAvPT09fD09fDw9PXw8PXw8fD49PXw+PXw+LyxcbiAgIHN5bWJvbDogW1wiKFwiLCBcIilcIiwgXCI9XCIsIFwi4oaSXCIsIFwiO1wiLCBcIntcIiwgXCJ9XCIsIFwiLFwiLCBcIltcIiwgXCJdXCIsIFwiLi4uXCJdLCAvLyBtdXN0IGNvbWUgYWZ0ZXIgY29tcGFyZU9wXG59KVxuXG5cbmltcG9ydCB7IF9fY2hlY2ssIGFzc2VydCwgZXJyb3IgfSBmcm9tIFwiLi91dGlsL0NvcmVcIlxuaW1wb3J0IHsgbnVtLCBzdHIgfSBmcm9tIFwiLi9Bbm5vdGF0ZWRcIlxuaW1wb3J0IHsgQ29ucywgTGlzdCwgTmlsLCBQYWlyLCBuaWwgfSBmcm9tIFwiLi9CYXNlVHlwZXNcIlxuaW1wb3J0IHsgYXJpdHksIHR5cGVzIH0gZnJvbSBcIi4vRGF0YVR5cGVcIlxuaW1wb3J0IHsgRXhwciB9IGZyb20gXCIuL0V4cHJcIlxuaW1wb3J0IHsgc2luZ2xldG9uLCB1bmlvbldpdGggfSBmcm9tIFwiLi9GaW5pdGVNYXBcIlxuaW1wb3J0IHsgU3RyIH0gZnJvbSBcIi4vVmFsdWVcIlxuaW1wb3J0IHsgzr0gfSBmcm9tIFwiLi9WZXJzaW9uZWRcIlxuXG5pbXBvcnQgQ29udCA9IEV4cHIuQ29udFxuaW1wb3J0IFRyaWUgPSBFeHByLlRyaWVcblxuLy8gQ29uc3RydWN0b3JzIG11c3Qgc3RhcnQgd2l0aCBhbiB1cHBlcmNhc2UgbGV0dGVyLCBhIGxhIEhhc2tlbGwuIFdpbGwgZml4IHRoaXMgYXMgcGFydCBvZiBpc3N1ZSAjNDkuXG5mdW5jdGlvbiBpc0N0ciAoc3RyOiBzdHJpbmcpOiBib29sZWFuIHtcbiAgIGNvbnN0IGNoOiBzdHJpbmcgPSBzdHIuY2hhckF0KDApXG4gICByZXR1cm4gY2ggPT09IGNoLnRvVXBwZXJDYXNlKCkgJiYgY2ggIT09IGNoLnRvTG93ZXJDYXNlKClcbn1cblxudHlwZSBNa0NvbnQgPSAozro6IENvbnQpID0+IENvbnRcblxuZnVuY3Rpb24gY29tcG9zZSAobWtfzroxOiBNa0NvbnQsIG1rX866MjogTWtDb250KTogTWtDb250IHtcbiAgIHJldHVybiAozro6IENvbnQpID0+IG1rX866MShta1/OujIozropKVxufVxuXG5leHBvcnQgaW50ZXJmYWNlIFRva2VuIHsgdmFsdWU6IGFueTsgW2tleTogc3RyaW5nXTogYW55IH07XG5cbmV4cG9ydCBpbnRlcmZhY2UgTGV4ZXIge1xuICByZXNldDogKGNodW5rOiBzdHJpbmcsIGluZm86IGFueSkgPT4gdm9pZDtcbiAgbmV4dDogKCkgPT4gVG9rZW4gfCB1bmRlZmluZWQ7XG4gIHNhdmU6ICgpID0+IGFueTtcbiAgZm9ybWF0RXJyb3I6ICh0b2tlbjogVG9rZW4pID0+IHN0cmluZztcbiAgaGFzOiAodG9rZW5UeXBlOiBzdHJpbmcpID0+IGJvb2xlYW5cbn07XG5cbmV4cG9ydCBpbnRlcmZhY2UgTmVhcmxleVJ1bGUge1xuICBuYW1lOiBzdHJpbmc7XG4gIHN5bWJvbHM6IE5lYXJsZXlTeW1ib2xbXTtcbiAgcG9zdHByb2Nlc3M/OiAoZDogYW55W10sIGxvYz86IG51bWJlciwgcmVqZWN0Pzoge30pID0+IGFueVxufTtcblxuZXhwb3J0IHR5cGUgTmVhcmxleVN5bWJvbCA9IHN0cmluZyB8IHsgbGl0ZXJhbDogYW55IH0gfCB7IHRlc3Q6ICh0b2tlbjogYW55KSA9PiBib29sZWFuIH07XG5cbmV4cG9ydCB2YXIgTGV4ZXI6IExleGVyIHwgdW5kZWZpbmVkID0gbGV4ZXI7XG5cbmV4cG9ydCB2YXIgUGFyc2VyUnVsZXM6IE5lYXJsZXlSdWxlW10gPSBbXG4gICAge1wibmFtZVwiOiBcInJvb3RFeHByXCIsIFwic3ltYm9sc1wiOiBbXCJfXCIsIFwiZXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZV0pID0+IGV9LFxuICAgIHtcIm5hbWVcIjogXCJyb290RXhwclwiLCBcInN5bWJvbHNcIjogW1wiZXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcIl8kZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogWyhsZXhlci5oYXMoXCJ3aGl0ZXNwYWNlXCIpID8ge3R5cGU6IFwid2hpdGVzcGFjZVwifSA6IHdoaXRlc3BhY2UpXX0sXG4gICAge1wibmFtZVwiOiBcIl8kZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogWyhsZXhlci5oYXMoXCJzaW5nbGVMaW5lQ29tbWVudFwiKSA/IHt0eXBlOiBcInNpbmdsZUxpbmVDb21tZW50XCJ9IDogc2luZ2xlTGluZUNvbW1lbnQpXX0sXG4gICAge1wibmFtZVwiOiBcIl8kZWJuZiQxXCIsIFwic3ltYm9sc1wiOiBbXCJfJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIl19LFxuICAgIHtcIm5hbWVcIjogXCJfJGVibmYkMSRzdWJleHByZXNzaW9uJDJcIiwgXCJzeW1ib2xzXCI6IFsobGV4ZXIuaGFzKFwid2hpdGVzcGFjZVwiKSA/IHt0eXBlOiBcIndoaXRlc3BhY2VcIn0gOiB3aGl0ZXNwYWNlKV19LFxuICAgIHtcIm5hbWVcIjogXCJfJGVibmYkMSRzdWJleHByZXNzaW9uJDJcIiwgXCJzeW1ib2xzXCI6IFsobGV4ZXIuaGFzKFwic2luZ2xlTGluZUNvbW1lbnRcIikgPyB7dHlwZTogXCJzaW5nbGVMaW5lQ29tbWVudFwifSA6IHNpbmdsZUxpbmVDb21tZW50KV19LFxuICAgIHtcIm5hbWVcIjogXCJfJGVibmYkMVwiLCBcInN5bWJvbHNcIjogW1wiXyRlYm5mJDFcIiwgXCJfJGVibmYkMSRzdWJleHByZXNzaW9uJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogKGQpID0+IGRbMF0uY29uY2F0KFtkWzFdXSl9LFxuICAgIHtcIm5hbWVcIjogXCJfXCIsIFwic3ltYm9sc1wiOiBbXCJfJGVibmYkMVwiXX0sXG4gICAge1wibmFtZVwiOiBcImV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcImNvbXBhcmVFeHByXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZXhwclwiLCBcInN5bWJvbHNcIjogW1wiZGVmczFcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJleHByXCIsIFwic3ltYm9sc1wiOiBbXCJmdW5cIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJleHByXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaEFzXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZXhwclwiLCBcInN5bWJvbHNcIjogW1widHlwZW1hdGNoXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZGVmczEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJpblwifV19LFxuICAgIHtcIm5hbWVcIjogXCJkZWZzMSRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW1wiZGVmczEkbWFjcm9jYWxsJDJcIl19LFxuICAgIHtcIm5hbWVcIjogXCJkZWZzMSRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZGVmczEkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJkZWZzMSRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZGVmczEkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImRlZnMxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJkZWZzMSRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiXX0sXG4gICAge1wibmFtZVwiOiBcImRlZnMxXCIsIFwic3ltYm9sc1wiOiBbXCJkZWZMaXN0XCIsIFwiZGVmczEkbWFjcm9jYWxsJDFcIiwgXCJleHByXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbZGVmcywgLCBlXSkgPT4gRXhwci5kZWZzKM69KCksIGRlZnMsIGUpfSxcbiAgICB7XCJuYW1lXCI6IFwiY29tcGFyZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcImNvbXBhcmVFeHByXCIsIFwiY29tcGFyZU9wXCIsIFwic3VtRXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2UxLCBvcCwgZTJdKSA9PiBFeHByLmJpbmFyeUFwcCjOvSgpLCBlMSwgc3RyKG9wKSwgZTIpfSxcbiAgICB7XCJuYW1lXCI6IFwiY29tcGFyZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcInN1bUV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJzdW1FeHByXCIsIFwic3ltYm9sc1wiOiBbXCJzdW1FeHByXCIsIFwic3VtT3BcIiwgXCJwcm9kdWN0RXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2UxLCBvcCwgZTJdKSA9PiBFeHByLmJpbmFyeUFwcCjOvSgpLCBlMSwgc3RyKG9wKSwgZTIpfSxcbiAgICB7XCJuYW1lXCI6IFwic3VtRXhwclwiLCBcInN5bWJvbHNcIjogW1wicHJvZHVjdEV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJwcm9kdWN0RXhwclwiLCBcInN5bWJvbHNcIjogW1wicHJvZHVjdEV4cHJcIiwgXCJwcm9kdWN0T3BcIiwgXCJleHBvbmVudEV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogKFtlMSwgb3AsIGUyXSkgPT4gRXhwci5iaW5hcnlBcHAozr0oKSwgZTEsIHN0cihvcCksIGUyKX0sXG4gICAge1wibmFtZVwiOiBcInByb2R1Y3RFeHByXCIsIFwic3ltYm9sc1wiOiBbXCJleHBvbmVudEV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJleHBvbmVudEV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcImV4cG9uZW50RXhwclwiLCBcImV4cG9uZW50T3BcIiwgXCJhcHBDaGFpblwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2UxLCBvcCwgZTJdKSA9PiBFeHByLmJpbmFyeUFwcCjOvSgpLCBlMSwgc3RyKG9wKSwgZTIpfSxcbiAgICB7XCJuYW1lXCI6IFwiZXhwb25lbnRFeHByXCIsIFwic3ltYm9sc1wiOiBbXCJhcHBDaGFpblwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImFwcENoYWluXCIsIFwic3ltYm9sc1wiOiBbXCJzaW1wbGVFeHByXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiYXBwQ2hhaW5cIiwgXCJzeW1ib2xzXCI6IFtcImFwcENoYWluXCIsIFwic2ltcGxlRXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2UxLCBlMl0pID0+IEV4cHIuYXBwKM69KCksIGUxLCBlMil9LFxuICAgIHtcIm5hbWVcIjogXCJzaW1wbGVFeHByXCIsIFwic3ltYm9sc1wiOiBbXCJ2YXJpYWJsZVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInNpbXBsZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcInN0cmluZ1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInNpbXBsZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcIm51bWJlclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInNpbXBsZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcInBhcmVudGhFeHByXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwic2ltcGxlRXhwclwiLCBcInN5bWJvbHNcIjogW1wicGFpclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInNpbXBsZUV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJzaW1wbGVFeHByXCIsIFwic3ltYm9sc1wiOiBbXCJjb25zdHJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJ2YXJpYWJsZVwiLCBcInN5bWJvbHNcIjogW1widmFyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeF0pID0+IEV4cHIudmFyXyjOvSgpLCB4KX0sXG4gICAge1wibmFtZVwiOiBcInZhciRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogWyhsZXhlci5oYXMoXCJpZGVudFwiKSA/IHt0eXBlOiBcImlkZW50XCJ9IDogaWRlbnQpXX0sXG4gICAge1wibmFtZVwiOiBcInZhciRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1widmFyJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwidmFyJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJ2YXIkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInZhclwiLCBcInN5bWJvbHNcIjogW1widmFyJG1hY3JvY2FsbCQxXCJdLCBcInBvc3Rwcm9jZXNzXCI6ICAoW1t4XV0sIF8sIHJlamVjdCkgPT4ge1xuICAgICAgICAgICBpZiAoaXNDdHIoeC52YWx1ZSkpIHtcbiAgICAgICAgICAgICAgcmV0dXJuIHJlamVjdFxuICAgICAgICAgICB9XG4gICAgICAgICAgIHJldHVybiBzdHIoeC52YWx1ZSkgXG4gICAgICAgIH0gfSxcbiAgICB7XCJuYW1lXCI6IFwic3RyaW5nJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbKGxleGVyLmhhcyhcInN0cmluZ1wiKSA/IHt0eXBlOiBcInN0cmluZ1wifSA6IHN0cmluZyldfSxcbiAgICB7XCJuYW1lXCI6IFwic3RyaW5nJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJzdHJpbmckbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJzdHJpbmckbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInN0cmluZyRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwic3RyaW5nXCIsIFwic3ltYm9sc1wiOiBbXCJzdHJpbmckbWFjcm9jYWxsJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKFtbbGl0XV0pID0+IEV4cHIuY29uc3RTdHIozr0oKSwgc3RyKChsaXQudmFsdWUgYXMgc3RyaW5nKS5zbGljZSgxLCAtMSkpKX0sXG4gICAge1wibmFtZVwiOiBcIm51bWJlciRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogWyhsZXhlci5oYXMoXCJudW1iZXJcIikgPyB7dHlwZTogXCJudW1iZXJcIn0gOiBudW1iZXIpXX0sXG4gICAge1wibmFtZVwiOiBcIm51bWJlciRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibnVtYmVyJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibnVtYmVyJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJudW1iZXIkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcIm51bWJlclwiLCBcInN5bWJvbHNcIjogW1wibnVtYmVyJG1hY3JvY2FsbCQxXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbW2xpdF1dKSA9PiBFeHByLmNvbnN0TnVtKM69KCksIG51bShuZXcgTnVtYmVyKGxpdC52YWx1ZSBhcyBzdHJpbmcpLnZhbHVlT2YoKSkpfSxcbiAgICB7XCJuYW1lXCI6IFwicGFyZW50aEV4cHIkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIoXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInBhcmVudGhFeHByJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwYXJlbnRoRXhwciRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInBhcmVudGhFeHByJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwYXJlbnRoRXhwciRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwicGFyZW50aEV4cHIkbWFjcm9jYWxsJDRcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIpXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInBhcmVudGhFeHByJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJwYXJlbnRoRXhwciRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInBhcmVudGhFeHByJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJwYXJlbnRoRXhwciRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwicGFyZW50aEV4cHJcIiwgXCJzeW1ib2xzXCI6IFtcInBhcmVudGhFeHByJG1hY3JvY2FsbCQxXCIsIFwiZXhwclwiLCBcInBhcmVudGhFeHByJG1hY3JvY2FsbCQzXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCBlLF0pID0+IGV9LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiKFwifV19LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicGFpciRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wicGFpciRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwicGFpciRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIixcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwicGFpciRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wicGFpciRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInBhaXIkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcInBhaXIkbWFjcm9jYWxsJDRcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInBhaXIkbWFjcm9jYWxsJDZcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIpXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInBhaXIkbWFjcm9jYWxsJDVcIiwgXCJzeW1ib2xzXCI6IFtcInBhaXIkbWFjcm9jYWxsJDZcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyJG1hY3JvY2FsbCQ1XCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyJG1hY3JvY2FsbCQ2XCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyXCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyJG1hY3JvY2FsbCQxXCIsIFwiZXhwclwiLCBcInBhaXIkbWFjcm9jYWxsJDNcIiwgXCJleHByXCIsIFwicGFpciRtYWNyb2NhbGwkNVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZTEsICwgZTIsXSkgPT4gRXhwci5jb25zdHIozr0oKSwgc3RyKFBhaXIubmFtZSksIExpc3QuZnJvbUFycmF5KFtlMSwgZTJdKSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0JG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiW1wifV19LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0JG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0JG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdCRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibGlzdCRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdCRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIl1cIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdCRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wibGlzdCRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxpc3QkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3QkbWFjcm9jYWxsJDRcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImxpc3RcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3QkbWFjcm9jYWxsJDFcIiwgXCJsaXN0T3B0XCIsIFwibGlzdCRtYWNyb2NhbGwkM1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZSwgXSkgPT4gZX0sXG4gICAge1wibmFtZVwiOiBcImNvbnN0clwiLCBcInN5bWJvbHNcIjogW1wiY3RyXCIsIFwiYXJnc1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAgKFtjLCBlzIVdLCBfLCByZWplY3QpID0+IHtcbiAgICAgICAgICAgYXNzZXJ0KGMgaW5zdGFuY2VvZiBTdHIpXG4gICAgICAgICAgIGlmIChhcml0eShjKSAhPT0gZcyFLmxlbmd0aCkge1xuICAgICAgICAgICAgICByZXR1cm4gcmVqZWN0XG4gICAgICAgICAgIH1cbiAgICAgICAgICAgcmV0dXJuIEV4cHIuY29uc3RyKM69KCksIGMsIExpc3QuZnJvbUFycmF5KGXMhSkpXG4gICAgICAgIH0gfSxcbiAgICB7XCJuYW1lXCI6IFwiY3RyJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbKGxleGVyLmhhcyhcImlkZW50XCIpID8ge3R5cGU6IFwiaWRlbnRcIn0gOiBpZGVudCldfSxcbiAgICB7XCJuYW1lXCI6IFwiY3RyJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJjdHIkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJjdHIkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImN0ciRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwiY3RyXCIsIFwic3ltYm9sc1wiOiBbXCJjdHIkbWFjcm9jYWxsJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogIChbW3hdXSwgXywgcmVqZWN0KSA9PiB7XG4gICAgICAgICAgIGlmICghaXNDdHIoeC52YWx1ZSkpIHtcbiAgICAgICAgICAgICAgcmV0dXJuIHJlamVjdFxuICAgICAgICAgICB9XG4gICAgICAgICAgIHJldHVybiBzdHIoeC52YWx1ZSlcbiAgICAgICAgfSB9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzXCIsIFwic3ltYm9sc1wiOiBbXSwgXCJwb3N0cHJvY2Vzc1wiOiAoKSA9PiBbXX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3MkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIoXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3MkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImFyZ3MkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJGVibmYkMVwiLCBcInN5bWJvbHNcIjogW119LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIsXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3MkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiYXJncyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiYXJncyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImFyZ3MkZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogW1wiYXJncyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwiZXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZV0pID0+IGV9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJGVibmYkMVwiLCBcInN5bWJvbHNcIjogW1wiYXJncyRlYm5mJDFcIiwgXCJhcmdzJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKGQpID0+IGRbMF0uY29uY2F0KFtkWzFdXSl9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJG1hY3JvY2FsbCQ0XCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiKVwifV19LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzJG1hY3JvY2FsbCQ0XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiYXJncyRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wiYXJncyRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc1wiLCBcInN5bWJvbHNcIjogW1wiYXJncyRtYWNyb2NhbGwkMVwiLCBcImV4cHJcIiwgXCJhcmdzJGVibmYkMVwiLCBcImFyZ3MkbWFjcm9jYWxsJDNcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIGUsIGVzLF0pID0+IFtlLCAuLi5lc119LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJ0eXBlbWF0Y2hcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZW1hdGNoJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDJcIl19LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVtYXRjaCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInR5cGVtYXRjaCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1widHlwZW1hdGNoJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVtYXRjaCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiXX0sXG4gICAge1wibmFtZVwiOiBcInR5cGVtYXRjaCRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcImFzXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInR5cGVtYXRjaCRtYWNyb2NhbGwkMyRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW1widHlwZW1hdGNoJG1hY3JvY2FsbCQ0XCJdfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZW1hdGNoJG1hY3JvY2FsbCQzJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDMkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVtYXRjaCRtYWNyb2NhbGwkMyRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZW1hdGNoJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDMkbWFjcm9jYWxsJDFcIl19LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbWF0Y2hcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVtYXRjaCRtYWNyb2NhbGwkMVwiLCBcImV4cHJcIiwgXCJ0eXBlbWF0Y2gkbWFjcm9jYWxsJDNcIiwgXCJ0eXBlTWF0Y2hlc1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZSwgLCBtXSkgPT4gRXhwci50eXBlbWF0Y2gozr0oKSwgZSwgbSl9LFxuICAgIHtcIm5hbWVcIjogXCJkZWZMaXN0JGVibmYkMVwiLCBcInN5bWJvbHNcIjogW119LFxuICAgIHtcIm5hbWVcIjogXCJkZWZMaXN0JGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCI7XCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImRlZkxpc3QkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZGVmTGlzdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZGVmTGlzdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJkZWZMaXN0JGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImRlZkxpc3QkZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogW1wiZGVmTGlzdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwiZGVmXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCBkZWZdKSA9PiBkZWZ9LFxuICAgIHtcIm5hbWVcIjogXCJkZWZMaXN0JGVibmYkMVwiLCBcInN5bWJvbHNcIjogW1wiZGVmTGlzdCRlYm5mJDFcIiwgXCJkZWZMaXN0JGVibmYkMSRzdWJleHByZXNzaW9uJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKGQpID0+IGRbMF0uY29uY2F0KFtkWzFdXSl9LFxuICAgIHtcIm5hbWVcIjogXCJkZWZMaXN0XCIsIFwic3ltYm9sc1wiOiBbXCJkZWZcIiwgXCJkZWZMaXN0JGVibmYkMVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2RlZiwgZGVmc10pID0+IExpc3QuZnJvbUFycmF5KFtkZWYsIC4uLmRlZnNdKX0sXG4gICAge1wibmFtZVwiOiBcImRlZlwiLCBcInN5bWJvbHNcIjogW1wibGV0XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZGVmXCIsIFwic3ltYm9sc1wiOiBbXCJsZXRyZWNcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJkZWZcIiwgXCJzeW1ib2xzXCI6IFtcInByaW1cIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJsZXQkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJsZXRcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGV0JG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbXCJsZXQkbWFjcm9jYWxsJDJcIl19LFxuICAgIHtcIm5hbWVcIjogXCJsZXQkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxldCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxldCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibGV0JG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJsZXQkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxldCRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiXX0sXG4gICAge1wibmFtZVwiOiBcImxldCRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIj1cIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGV0JG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJsZXQkbWFjcm9jYWxsJDRcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJsZXQkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImxldCRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGV0XCIsIFwic3ltYm9sc1wiOiBbXCJsZXQkbWFjcm9jYWxsJDFcIiwgXCJ2YXJcIiwgXCJsZXQkbWFjcm9jYWxsJDNcIiwgXCJleHByXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCB4LCAsIGVdKSA9PiBFeHByLmxldF8oeCwgZSl9LFxuICAgIHtcIm5hbWVcIjogXCJsZXRyZWMkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJsZXRyZWNcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGV0cmVjJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbXCJsZXRyZWMkbWFjcm9jYWxsJDJcIl19LFxuICAgIHtcIm5hbWVcIjogXCJsZXRyZWMkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxldHJlYyRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxldHJlYyRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibGV0cmVjJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJsZXRyZWMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxldHJlYyRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiXX0sXG4gICAge1wibmFtZVwiOiBcImxldHJlYyRlYm5mJDFcIiwgXCJzeW1ib2xzXCI6IFtdfSxcbiAgICB7XCJuYW1lXCI6IFwibGV0cmVjJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCI7XCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImxldHJlYyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsZXRyZWMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxldHJlYyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsZXRyZWMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGV0cmVjJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxldHJlYyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwicmVjRGVmXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCByZWNEZWZdKSA9PiByZWNEZWZ9LFxuICAgIHtcIm5hbWVcIjogXCJsZXRyZWMkZWJuZiQxXCIsIFwic3ltYm9sc1wiOiBbXCJsZXRyZWMkZWJuZiQxXCIsIFwibGV0cmVjJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKGQpID0+IGRbMF0uY29uY2F0KFtkWzFdXSl9LFxuICAgIHtcIm5hbWVcIjogXCJsZXRyZWNcIiwgXCJzeW1ib2xzXCI6IFtcImxldHJlYyRtYWNyb2NhbGwkMVwiLCBcInJlY0RlZlwiLCBcImxldHJlYyRlYm5mJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIHJlY0RlZiwgzrRdKSA9PiBFeHByLmxldFJlYyhMaXN0LmZyb21BcnJheShbcmVjRGVmLCAuLi7OtF0pKX0sXG4gICAge1wibmFtZVwiOiBcInByaW0kbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJwcmltaXRpdmVcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwicHJpbSRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW1wicHJpbSRtYWNyb2NhbGwkMlwiXX0sXG4gICAge1wibmFtZVwiOiBcInByaW0kbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInByaW0kbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJwcmltJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwcmltJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJwcmltJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwcmltJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCJdfSxcbiAgICB7XCJuYW1lXCI6IFwicHJpbVwiLCBcInN5bWJvbHNcIjogW1wicHJpbSRtYWNyb2NhbGwkMVwiLCBcInZhclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgeF0pID0+IEV4cHIucHJpbSh4KX0sXG4gICAge1wibmFtZVwiOiBcInJlY0RlZiRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcImZ1blwifV19LFxuICAgIHtcIm5hbWVcIjogXCJyZWNEZWYkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFtcInJlY0RlZiRtYWNyb2NhbGwkMlwiXX0sXG4gICAge1wibmFtZVwiOiBcInJlY0RlZiRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wicmVjRGVmJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicmVjRGVmJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJyZWNEZWYkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInJlY0RlZiRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wicmVjRGVmJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCJdfSxcbiAgICB7XCJuYW1lXCI6IFwicmVjRGVmXCIsIFwic3ltYm9sc1wiOiBbXCJyZWNEZWYkbWFjcm9jYWxsJDFcIiwgXCJ2YXJcIiwgXCJtYXRjaGVzXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCBmLCDPg10pID0+IEV4cHIucmVjRGVmKGYsIM+DKX0sXG4gICAge1wibmFtZVwiOiBcImZ1biRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcImZ1blwifV19LFxuICAgIHtcIm5hbWVcIjogXCJmdW4kbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFtcImZ1biRtYWNyb2NhbGwkMlwiXX0sXG4gICAge1wibmFtZVwiOiBcImZ1biRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZnVuJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiZnVuJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJmdW4kbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImZ1biRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZnVuJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQxXCJdfSxcbiAgICB7XCJuYW1lXCI6IFwiZnVuXCIsIFwic3ltYm9sc1wiOiBbXCJmdW4kbWFjcm9jYWxsJDFcIiwgXCJtYXRjaGVzXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCDPg10pID0+IEV4cHIuZnVuKM69KCksIM+DKX0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoQXMkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJtYXRjaFwifV19LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaEFzJG1hY3JvY2FsbCQxJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaEFzJG1hY3JvY2FsbCQyXCJdfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hBcyRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hBcyRtYWNyb2NhbGwkMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoQXMkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoQXMkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoQXMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoQXMkbWFjcm9jYWxsJDEkbWFjcm9jYWxsJDFcIl19LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaEFzJG1hY3JvY2FsbCQ0XCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiYXNcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hBcyRtYWNyb2NhbGwkMyRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hBcyRtYWNyb2NhbGwkNFwiXX0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoQXMkbWFjcm9jYWxsJDMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoQXMkbWFjcm9jYWxsJDMkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaEFzJG1hY3JvY2FsbCQzJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaEFzJG1hY3JvY2FsbCQzJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaEFzJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaEFzJG1hY3JvY2FsbCQzJG1hY3JvY2FsbCQxXCJdfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hBc1wiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hBcyRtYWNyb2NhbGwkMVwiLCBcImV4cHJcIiwgXCJtYXRjaEFzJG1hY3JvY2FsbCQzXCIsIFwibWF0Y2hlc1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZSwgLCDPg10pID0+IEV4cHIubWF0Y2hBcyjOvSgpLCBlLCDPgyl9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaGVzXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXMkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJ7XCJ9XX0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoZXMkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaGVzJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaGVzJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaGVzJGVibmYkMVwiLCBcInN5bWJvbHNcIjogW119LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCI7XCJ9XX0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hlcyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hlcyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJtYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hlcyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwibWF0Y2hcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIG1dKSA9PiBtfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hlcyRlYm5mJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoZXMkZWJuZiQxXCIsIFwibWF0Y2hlcyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChkKSA9PiBkWzBdLmNvbmNhdChbZFsxXV0pfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hlcyRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIn1cIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2hlcyRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wibWF0Y2hlcyRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXMkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoZXMkbWFjcm9jYWxsJDRcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoZXNcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoZXMkbWFjcm9jYWxsJDFcIiwgXCJtYXRjaFwiLCBcIm1hdGNoZXMkZWJuZiQxXCIsIFwibWF0Y2hlcyRtYWNyb2NhbGwkM1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgbSwgbXMsXSkgPT4gW20sIC4uLm1zXS5yZWR1Y2UoVHJpZS5UcmllLmpvaW4pfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2gkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCLihpJcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2gkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibWF0Y2gkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcIm1hdGNoJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJtYXRjaFwiLCBcInN5bWJvbHNcIjogW1wicGF0dGVyblwiLCBcIm1hdGNoJG1hY3JvY2FsbCQxXCIsIFwiZXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW21rX866LCAsIGVdKSA9PiBta1/OuihlKX0sXG4gICAge1wibmFtZVwiOiBcIm1hdGNoXCIsIFwic3ltYm9sc1wiOiBbXCJwYXR0ZXJuXCIsIFwibWF0Y2hlc1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW21rX866MSwgz4NdKSA9PiBta1/OujEoRXhwci5mdW4ozr0oKSwgz4MpKX0sXG4gICAge1wibmFtZVwiOiBcInR5cGVNYXRjaGVzXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlTWF0Y2hcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlTWF0Y2hlcyRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIntcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaGVzJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaGVzJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlTWF0Y2hlcyRlYm5mJDFcIiwgXCJzeW1ib2xzXCI6IFtdfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIjtcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1widHlwZU1hdGNoZXMkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInR5cGVNYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInR5cGVNYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaGVzJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDFcIiwgXCJ0eXBlTWF0Y2hcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIG1dKSA9PiBtfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkZWJuZiQxXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlTWF0Y2hlcyRlYm5mJDFcIiwgXCJ0eXBlTWF0Y2hlcyRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChkKSA9PiBkWzBdLmNvbmNhdChbZFsxXV0pfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXMkbWFjcm9jYWxsJDRcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCJ9XCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInR5cGVNYXRjaGVzJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlTWF0Y2hlcyRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInR5cGVNYXRjaGVzJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlTWF0Y2hlcyRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoZXNcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaGVzJG1hY3JvY2FsbCQxXCIsIFwidHlwZU1hdGNoXCIsIFwidHlwZU1hdGNoZXMkZWJuZiQxXCIsIFwidHlwZU1hdGNoZXMkbWFjcm9jYWxsJDNcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIG0sIG1zLF0pID0+IFttLCAuLi5tc10ucmVkdWNlKChtMSwgbTIpID0+IHVuaW9uV2l0aChtMSwgbTIsIChlOiBFeHByLCBlyrk6IEV4cHIpOiBFeHByID0+IGVycm9yKFwiT3ZlcmxhcHBpbmcgdHlwZWNhc2UgYnJhbmNoZXMuXCIpKSl9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlTWF0Y2gkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCLihpJcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlTWF0Y2gkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlTWF0Y2gkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVNYXRjaCRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZU1hdGNoXCIsIFwic3ltYm9sc1wiOiBbXCJ0eXBlbmFtZVwiLCBcInR5cGVNYXRjaCRtYWNyb2NhbGwkMVwiLCBcImV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogIChbeCwgLCBlXSkgPT4ge1xuICAgICAgICAgICBhc3NlcnQoeCBpbnN0YW5jZW9mIFN0cilcbiAgICAgICAgICAgaWYgKCF0eXBlcy5oYXMoeC52YWwpKSB7XG4gICAgICAgICAgICAgIGVycm9yKGBUeXBlIG5hbWUgJHt4LnZhbH0gbm90IGZvdW5kLmApXG4gICAgICAgICAgIH1cbiAgICAgICAgICAgcmV0dXJuIHNpbmdsZXRvbih4LCBlKVxuICAgICAgICB9IH0sXG4gICAge1wibmFtZVwiOiBcInR5cGVuYW1lJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbKGxleGVyLmhhcyhcImlkZW50XCIpID8ge3R5cGU6IFwiaWRlbnRcIn0gOiBpZGVudCldfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZW5hbWUkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVuYW1lJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwidHlwZW5hbWUkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInR5cGVuYW1lJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJ0eXBlbmFtZVwiLCBcInN5bWJvbHNcIjogW1widHlwZW5hbWUkbWFjcm9jYWxsJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKFtbeF1dKSA9PiBzdHIoeC52YWx1ZSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0T3B0XCIsIFwic3ltYm9sc1wiOiBbXSwgXCJwb3N0cHJvY2Vzc1wiOiAoKSA9PiBFeHByLmNvbnN0cijOvSgpLCBzdHIoTmlsLm5hbWUpLCBuaWwoKSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0T3B0JGVibmYkMVwiLCBcInN5bWJvbHNcIjogW119LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0T3B0JGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIsXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImxpc3RPcHQkZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibGlzdE9wdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdE9wdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0T3B0JGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImxpc3RPcHQkZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiLCBcInN5bWJvbHNcIjogW1wibGlzdE9wdCRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwiZXhwclwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgZV0pID0+IGV9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0T3B0JGVibmYkMVwiLCBcInN5bWJvbHNcIjogW1wibGlzdE9wdCRlYm5mJDFcIiwgXCJsaXN0T3B0JGVibmYkMSRzdWJleHByZXNzaW9uJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKGQpID0+IGRbMF0uY29uY2F0KFtkWzFdXSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0T3B0XCIsIFwic3ltYm9sc1wiOiBbXCJleHByXCIsIFwibGlzdE9wdCRlYm5mJDFcIiwgXCJsaXN0UmVzdE9wdFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW2UsIGVzLCBlyrldKSA9PiBbZSwgLi4uZXMsIGXKuV0ucmV2ZXJzZSgpLnJlZHVjZSgoZcyFLCBlKSA9PiBFeHByLmNvbnN0cijOvSgpLCBzdHIoQ29ucy5uYW1lKSwgTGlzdC5mcm9tQXJyYXkoW2UsIGXMhV0pKSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdFwiLCBcInN5bWJvbHNcIjogW10sIFwicG9zdHByb2Nlc3NcIjogKCkgPT4gRXhwci5jb25zdHIozr0oKSwgc3RyKE5pbC5uYW1lKSwgbmlsKCkpfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHQkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIsXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImxpc3RSZXN0T3B0JG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0UmVzdE9wdCRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxpc3RSZXN0T3B0JG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0UmVzdE9wdCRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHQkbWFjcm9jYWxsJDRcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIuLi5cIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHQkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RSZXN0T3B0JG1hY3JvY2FsbCQ0XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHQkbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RSZXN0T3B0JG1hY3JvY2FsbCQ0XCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdFwiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHQkbWFjcm9jYWxsJDFcIiwgXCJsaXN0UmVzdE9wdCRtYWNyb2NhbGwkM1wiLCBcImV4cHJcIl0sIFwicG9zdHByb2Nlc3NcIjogKFssICwgZV0pID0+IGV9LFxuICAgIHtcIm5hbWVcIjogXCJwYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXCJ2YXJpYWJsZV9wYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wicGFpcl9wYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wibGlzdF9wYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wiY29uc3RyX3BhdHRlcm5cIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJ2YXJpYWJsZV9wYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXCJ2YXJcIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4XSkgPT4gKM66OiBDb250KSA9PiBUcmllLnZhcl8oeCwgzropfSxcbiAgICB7XCJuYW1lXCI6IFwicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiKFwifV19LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDRcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIsXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQ0XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDRcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkNlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIilcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQ1XCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDZcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDVcIiwgXCJzeW1ib2xzXCI6IFtcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkNlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwicGFpcl9wYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXCJwYWlyX3BhdHRlcm4kbWFjcm9jYWxsJDFcIiwgXCJwYXR0ZXJuXCIsIFwicGFpcl9wYXR0ZXJuJG1hY3JvY2FsbCQzXCIsIFwicGF0dGVyblwiLCBcInBhaXJfcGF0dGVybiRtYWNyb2NhbGwkNVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgbWtfzroxLCAsIG1rX866MiwgLF0pID0+ICjOujogQ29udCkgPT4gVHJpZS5jb25zdHIoc2luZ2xldG9uKHN0cihQYWlyLm5hbWUpLCBjb21wb3NlKG1rX866MSwgbWtfzroyKSjOuikpKX0sXG4gICAge1wibmFtZVwiOiBcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkMlwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIltcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdF9wYXR0ZXJuJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0X3BhdHRlcm4kbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0X3BhdHRlcm4kbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdF9wYXR0ZXJuJG1hY3JvY2FsbCQ0XCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiXVwifV19LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0X3BhdHRlcm4kbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkNFwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wibGlzdF9wYXR0ZXJuJG1hY3JvY2FsbCQ0XCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0X3BhdHRlcm5cIiwgXCJzeW1ib2xzXCI6IFtcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkMVwiLCBcImxpc3RPcHRfcGF0dGVyblwiLCBcImxpc3RfcGF0dGVybiRtYWNyb2NhbGwkM1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgbWtfzrosIF0pID0+IG1rX866fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdE9wdF9wYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXSwgXCJwb3N0cHJvY2Vzc1wiOiAoKSA9PiAozro6IENvbnQpID0+IFRyaWUuY29uc3RyKHNpbmdsZXRvbihzdHIoTmlsLm5hbWUpLCDOuikpfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdE9wdF9wYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0MV9wYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdDFfcGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wicGF0dGVyblwiLCBcImxpc3RSZXN0T3B0X3BhdHRlcm5cIl0sIFwicG9zdHByb2Nlc3NcIjogKFtta1/OujEsIG1rX866Ml0pID0+ICjOujogQ29udCkgPT4gVHJpZS5jb25zdHIoc2luZ2xldG9uKHN0cihDb25zLm5hbWUpLCBjb21wb3NlKG1rX866MSwgbWtfzroyKSjOuikpKX0sXG4gICAge1wibmFtZVwiOiBcImxpc3RSZXN0T3B0X3BhdHRlcm5cIiwgXCJzeW1ib2xzXCI6IFtdLCBcInBvc3Rwcm9jZXNzXCI6ICgpID0+ICjOujogQ29udCkgPT4gVHJpZS5jb25zdHIoc2luZ2xldG9uKHN0cihOaWwubmFtZSksIM66KSl9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiLFwifV19LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIi4uLlwifV19LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQ0XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkM1wiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkMVwiLCBcImxpc3RSZXN0T3B0X3BhdHRlcm4kbWFjcm9jYWxsJDNcIiwgXCJwYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbLCAsIG1rX866XSkgPT4gbWtfzrp9LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQ2XCIsIFwic3ltYm9sc1wiOiBbe1wibGl0ZXJhbFwiOlwiLFwifV19LFxuICAgIHtcIm5hbWVcIjogXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQ1XCIsIFwic3ltYm9sc1wiOiBbXCJsaXN0UmVzdE9wdF9wYXR0ZXJuJG1hY3JvY2FsbCQ2XCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkNVwiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkNlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwibGlzdFJlc3RPcHRfcGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wibGlzdFJlc3RPcHRfcGF0dGVybiRtYWNyb2NhbGwkNVwiLCBcImxpc3QxX3BhdHRlcm5cIl0sIFwicG9zdHByb2Nlc3NcIjogKFssIG1rX866XSkgPT4gbWtfzrp9LFxuICAgIHtcIm5hbWVcIjogXCJjb25zdHJfcGF0dGVyblwiLCBcInN5bWJvbHNcIjogW1wiY3RyXCIsIFwiYXJnc19wYXR0ZXJuXCJdLCBcInBvc3Rwcm9jZXNzXCI6ICAoW2MsIG1rX866c10sIF8sIHJlamVjdCkgPT4ge1xuICAgICAgICAgICBhc3NlcnQoYyBpbnN0YW5jZW9mIFN0cilcbiAgICAgICAgICAgaWYgKGFyaXR5KGMpICE9PSBta1/OunMubGVuZ3RoKSB7XG4gICAgICAgICAgICAgIHJldHVybiByZWplY3RcbiAgICAgICAgICAgfVxuICAgICAgICAgICByZXR1cm4gKM66OiBDb250KSA9PiBUcmllLmNvbnN0cihzaW5nbGV0b24oYywgbWtfzrpzLnJlZHVjZShjb21wb3NlLCAozro6IENvbnQpID0+IM66KSjOuikpKVxuICAgICAgICB9IH0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVyblwiLCBcInN5bWJvbHNcIjogW10sIFwicG9zdHByb2Nlc3NcIjogKCkgPT4gW119LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzX3BhdHRlcm4kbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIoXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiYXJnc19wYXR0ZXJuJG1hY3JvY2FsbCQyXCJdLCBcInBvc3Rwcm9jZXNzXCI6IGlkfSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc19wYXR0ZXJuJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzX3BhdHRlcm4kbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRlYm5mJDFcIiwgXCJzeW1ib2xzXCI6IFtdfSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc19wYXR0ZXJuJGVibmYkMSRzdWJleHByZXNzaW9uJDEkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFt7XCJsaXRlcmFsXCI6XCIsXCJ9XX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzX3BhdHRlcm4kZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzX3BhdHRlcm4kZWJuZiQxJHN1YmV4cHJlc3Npb24kMSRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc19wYXR0ZXJuJGVibmYkMSRzdWJleHByZXNzaW9uJDFcIiwgXCJzeW1ib2xzXCI6IFtcImFyZ3NfcGF0dGVybiRlYm5mJDEkc3ViZXhwcmVzc2lvbiQxJG1hY3JvY2FsbCQxXCIsIFwicGF0dGVyblwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgbWtfzrpdKSA9PiBta1/Oun0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRlYm5mJDFcIiwgXCJzeW1ib2xzXCI6IFtcImFyZ3NfcGF0dGVybiRlYm5mJDFcIiwgXCJhcmdzX3BhdHRlcm4kZWJuZiQxJHN1YmV4cHJlc3Npb24kMVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoZCkgPT4gZFswXS5jb25jYXQoW2RbMV1dKX0sXG4gICAge1wibmFtZVwiOiBcImFyZ3NfcGF0dGVybiRtYWNyb2NhbGwkNFwiLCBcInN5bWJvbHNcIjogW3tcImxpdGVyYWxcIjpcIilcIn1dfSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc19wYXR0ZXJuJG1hY3JvY2FsbCQzXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzX3BhdHRlcm4kbWFjcm9jYWxsJDRcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJhcmdzX3BhdHRlcm4kbWFjcm9jYWxsJDNcIiwgXCJzeW1ib2xzXCI6IFtcImFyZ3NfcGF0dGVybiRtYWNyb2NhbGwkNFwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwiYXJnc19wYXR0ZXJuXCIsIFwic3ltYm9sc1wiOiBbXCJhcmdzX3BhdHRlcm4kbWFjcm9jYWxsJDFcIiwgXCJwYXR0ZXJuXCIsIFwiYXJnc19wYXR0ZXJuJGVibmYkMVwiLCBcImFyZ3NfcGF0dGVybiRtYWNyb2NhbGwkM1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoWywgbWtfzrosIG1rX866cyxdKSA9PiBbbWtfzrosIC4uLm1rX866c119LFxuICAgIHtcIm5hbWVcIjogXCJjb21wYXJlT3AkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFsobGV4ZXIuaGFzKFwiY29tcGFyZU9wXCIpID8ge3R5cGU6IFwiY29tcGFyZU9wXCJ9IDogY29tcGFyZU9wKV19LFxuICAgIHtcIm5hbWVcIjogXCJjb21wYXJlT3AkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImNvbXBhcmVPcCRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImNvbXBhcmVPcCRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiY29tcGFyZU9wJG1hY3JvY2FsbCQyXCIsIFwiX1wiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW3gsIF0pID0+IHh9LFxuICAgIHtcIm5hbWVcIjogXCJjb21wYXJlT3BcIiwgXCJzeW1ib2xzXCI6IFtcImNvbXBhcmVPcCRtYWNyb2NhbGwkMVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW1t4XV0pID0+IHgudmFsdWV9LFxuICAgIHtcIm5hbWVcIjogXCJleHBvbmVudE9wJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbKGxleGVyLmhhcyhcImV4cG9uZW50T3BcIikgPyB7dHlwZTogXCJleHBvbmVudE9wXCJ9IDogZXhwb25lbnRPcCldfSxcbiAgICB7XCJuYW1lXCI6IFwiZXhwb25lbnRPcCRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wiZXhwb25lbnRPcCRtYWNyb2NhbGwkMlwiXSwgXCJwb3N0cHJvY2Vzc1wiOiBpZH0sXG4gICAge1wibmFtZVwiOiBcImV4cG9uZW50T3AkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcImV4cG9uZW50T3AkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcImV4cG9uZW50T3BcIiwgXCJzeW1ib2xzXCI6IFtcImV4cG9uZW50T3AkbWFjcm9jYWxsJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKFtbeF1dKSA9PiB4LnZhbHVlfSxcbiAgICB7XCJuYW1lXCI6IFwicHJvZHVjdE9wJG1hY3JvY2FsbCQyXCIsIFwic3ltYm9sc1wiOiBbKGxleGVyLmhhcyhcInByb2R1Y3RPcFwiKSA/IHt0eXBlOiBcInByb2R1Y3RPcFwifSA6IHByb2R1Y3RPcCldfSxcbiAgICB7XCJuYW1lXCI6IFwicHJvZHVjdE9wJG1hY3JvY2FsbCQxXCIsIFwic3ltYm9sc1wiOiBbXCJwcm9kdWN0T3AkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJwcm9kdWN0T3AkbWFjcm9jYWxsJDFcIiwgXCJzeW1ib2xzXCI6IFtcInByb2R1Y3RPcCRtYWNyb2NhbGwkMlwiLCBcIl9cIl0sIFwicG9zdHByb2Nlc3NcIjogKFt4LCBdKSA9PiB4fSxcbiAgICB7XCJuYW1lXCI6IFwicHJvZHVjdE9wXCIsIFwic3ltYm9sc1wiOiBbXCJwcm9kdWN0T3AkbWFjcm9jYWxsJDFcIl0sIFwicG9zdHByb2Nlc3NcIjogKFtbeF1dKSA9PiB4LnZhbHVlfSxcbiAgICB7XCJuYW1lXCI6IFwic3VtT3AkbWFjcm9jYWxsJDJcIiwgXCJzeW1ib2xzXCI6IFsobGV4ZXIuaGFzKFwic3VtT3BcIikgPyB7dHlwZTogXCJzdW1PcFwifSA6IHN1bU9wKV19LFxuICAgIHtcIm5hbWVcIjogXCJzdW1PcCRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wic3VtT3AkbWFjcm9jYWxsJDJcIl0sIFwicG9zdHByb2Nlc3NcIjogaWR9LFxuICAgIHtcIm5hbWVcIjogXCJzdW1PcCRtYWNyb2NhbGwkMVwiLCBcInN5bWJvbHNcIjogW1wic3VtT3AkbWFjcm9jYWxsJDJcIiwgXCJfXCJdLCBcInBvc3Rwcm9jZXNzXCI6IChbeCwgXSkgPT4geH0sXG4gICAge1wibmFtZVwiOiBcInN1bU9wXCIsIFwic3ltYm9sc1wiOiBbXCJzdW1PcCRtYWNyb2NhbGwkMVwiXSwgXCJwb3N0cHJvY2Vzc1wiOiAoW1t4XV0pID0+IHgudmFsdWV9XG5dO1xuXG5leHBvcnQgdmFyIFBhcnNlclN0YXJ0OiBzdHJpbmcgPSBcInJvb3RFeHByXCI7XG4iLCJpbXBvcnQgeyBhcywgYXNzZXJ0IH0gZnJvbSBcIi4vdXRpbC9Db3JlXCJcbmltcG9ydCB7IEFubm90YXRlZCwgYXNBbm5vdGF0ZWQsIGFubm90YXRlZEF0LCBudW0sIHN0ciB9IGZyb20gXCIuL0Fubm90YXRlZFwiXG5pbXBvcnQgeyBCb29sLCB0cnVlXywgZmFsc2VfIH0gZnJvbSBcIi4vQmFzZVR5cGVzXCJcbmltcG9ydCB7IE51bSwgUHJpbU9wVGFnLCBQcmltVmFsdWUsIFN0ciwgXywgVmFsdWUgfSBmcm9tIFwiLi9WYWx1ZVwiXG5pbXBvcnQgeyDOvSB9IGZyb20gXCIuL1ZlcnNpb25lZFwiXG5cbnR5cGUgVW5hcnk8VCwgVj4gPSAoeDogVCkgPT4gQW5ub3RhdGVkPFY+XG50eXBlIEJpbmFyeTxULCBVLCBWPiA9ICh4OiBULCB5OiBVKSA9PiBBbm5vdGF0ZWQ8Vj5cblxuLy8gSW4gdGhlIGZvbGxvd2luZyB0d28gY2xhc3Nlcywgd2Ugc3RvcmUgdGhlIG9wZXJhdGlvbiB3aXRob3V0IGdlbmVyaWMgdHlwZSBwYXJhbWV0ZXJzLCBhcyBmaWVsZHMgY2FuJ3Rcbi8vIGhhdmUgcG9seW1vcnBoaWMgdHlwZS4gVGhlbiBhY2Nlc3MgdGhlIG9wZXJhdGlvbiB2aWEgYSBtZXRob2QgYW5kIHJlaW5zdGF0ZSB0aGUgcG9seW1vcnBoaXNtIHZpYSBhIGNhc3QuXG5cbmV4cG9ydCBjbGFzcyBQcmltT3A8VGFnIGV4dGVuZHMgUHJpbU9wVGFnPiBleHRlbmRzIFZhbHVlPFRhZz4ge1xuICAgbmFtZTogc3RyaW5nID0gX1xufVxuXG5leHBvcnQgY2xhc3MgVW5hcnlPcCBleHRlbmRzIFByaW1PcDxcIlVuYXJ5T3BcIj4ge1xuICAgb3A6IFVuYXJ5PFByaW1WYWx1ZSwgVmFsdWU+ID0gX1xufVxuXG5leHBvcnQgY2xhc3MgQmluYXJ5T3AgZXh0ZW5kcyBQcmltT3A8XCJCaW5hcnlPcFwiPiB7XG4gICBvcDogQmluYXJ5PFByaW1WYWx1ZSwgUHJpbVZhbHVlLCBWYWx1ZT4gPSBfXG59XG5cbmNvbnN0IGNlaWxpbmcgPSAoeDogTnVtKTogQW5ub3RhdGVkPE51bT4gPT4gbnVtKE1hdGguY2VpbCh4LnZhbCkpXG4vLyBVc2VkIHRvIHRha2UgYXJiaXRyYXJ5IHZhbHVlIGFzIGFkZGl0aW9uYWwgYXJndW1lbnQsIGJ1dCBub3cgcHJpbWl0aXZlcyBoYXZlIHByaW1pdGl2ZSBhcmd1bWVudHMuXG5jb25zdCBlcnJvciA9IChtZXNzYWdlOiBTdHIpOiBBbm5vdGF0ZWQ8VmFsdWU+ID0+IGFzc2VydChmYWxzZSwgXCJMYW1iZGFDYWxjIGVycm9yOlxcblwiICsgbWVzc2FnZS52YWwpXG5jb25zdCBmbG9vciA9ICh4OiBOdW0pOiBBbm5vdGF0ZWQ8TnVtPiA9PiBudW0oTWF0aC5mbG9vcih4LnZhbCkpXG5jb25zdCBsb2cgPSAoeDogTnVtKTogQW5ub3RhdGVkPE51bT4gPT4gbnVtKE1hdGgubG9nKGFzKHgsIE51bSkudmFsKSlcbmNvbnN0IG51bVRvU3RyID0gKHg6IE51bSk6IEFubm90YXRlZDxTdHI+ID0+IHN0cih4LnZhbC50b1N0cmluZygpKVxuY29uc3QgdHJhY2UgPSAodjogTnVtIHwgU3RyKTogQW5ub3RhdGVkPFZhbHVlPiA9PiB7IGNvbnNvbGUubG9nKHYpOyByZXR1cm4gYXNBbm5vdGF0ZWQodikgfVxuLy8gTm8gbG9uZ2VyIHN1cHBvcnQgb3ZlcmxvYWRlZCBmdW5jdGlvbnMsIHNpbmNlIHRoZSBwYXR0ZXJuLW1hdGNoaW5nIHNlbWFudGljcyBpcyBub24tdHJpdmlhbDsgbWlnaHQgcmVxdWlyZSB0eXBlY2FzZS5cbi8vIElmIHdlIHdhbnQgaW50ZWdlciBkaXZpc2lvbiwgYXBwYXJlbnRseSB+fih4IC8geSkgd2lsbCByb3VuZCBpbiB0aGUgcmlnaHQgZGlyZWN0aW9uLlxuY29uc3QgZGl2ID0gKHg6IE51bSwgeTogTnVtKTogQW5ub3RhdGVkPE51bT4gPT4gbnVtKGFzKHgsIE51bSkudmFsIC8gYXMoeSwgTnVtKS52YWwpXG5jb25zdCBjb25jYXQgPSAoeDogU3RyLCB5OiBTdHIpOiBBbm5vdGF0ZWQ8U3RyPiA9PiBzdHIoYXMoeCwgU3RyKS52YWwgKyBhcyh5LCBTdHIpLnZhbClcbmNvbnN0IGVxdWFsSW50ID0gKHg6IE51bSwgeTogTnVtKTogQW5ub3RhdGVkPEJvb2w+ID0+IGFzKHgsIE51bSkudmFsID09PSBhcyh5LCBOdW0pLnZhbCA/IHRydWVfKCkgOiBmYWxzZV8oKVxuY29uc3QgZXF1YWxTdHIgPSAoeDogU3RyLCB5OiBTdHIpOiBBbm5vdGF0ZWQ8Qm9vbD4gPT4gYXMoeCwgU3RyKS52YWwgPT09IGFzKHksIFN0cikudmFsID8gdHJ1ZV8oKSA6IGZhbHNlXygpXG5jb25zdCBncmVhdGVyRXFJbnQgPSAoeDogTnVtLCB5OiBOdW0pOiBBbm5vdGF0ZWQ8Qm9vbD4gPT4gYXMoeCwgTnVtKS52YWwgPj0gYXMoeSwgTnVtKS52YWwgPyB0cnVlXygpIDogZmFsc2VfKClcbi8vIFN0cmluZyBjb21wYXJpc29uIGRlbGVnYXRlcyB0byBjZW50cmFsIGltcGxlbWVudGF0aW9uIGZvciBjb25zaXN0ZW5jeS5cbmNvbnN0IGdyZWF0ZXJFcVN0ciA9ICh4OiBTdHIsIHk6IFN0cik6IEFubm90YXRlZDxCb29sPiA9PiBhcyh4LCBTdHIpLmdlcShhcyh5LCBTdHIpKSA/IHRydWVfKCkgOiBmYWxzZV8oKVxuY29uc3QgZ3JlYXRlckludCA9ICh4OiBOdW0sIHk6IE51bSk6IEFubm90YXRlZDxCb29sPiA9PiBhcyh4LCBOdW0pLnZhbCA+IGFzKHksIE51bSkudmFsID8gdHJ1ZV8oKSA6IGZhbHNlXygpXG5jb25zdCBsZXNzRXFJbnQgPSAoeDogTnVtLCB5OiBOdW0pOiBBbm5vdGF0ZWQ8Qm9vbD4gPT4gYXMoeCwgTnVtKS52YWwgPD0gYXMoeSwgTnVtKS52YWwgPyB0cnVlXygpIDogZmFsc2VfKClcbmNvbnN0IGxlc3NFcVN0ciA9ICh4OiBTdHIsIHk6IFN0cik6IEFubm90YXRlZDxCb29sPiA9PiBhcyh4LCBTdHIpLmxlcShhcyh5LCBTdHIpKSA/IHRydWVfKCkgOiBmYWxzZV8oKVxuY29uc3QgbGVzc0ludCA9ICh4OiBOdW0sIHk6IE51bSk6IEFubm90YXRlZDxCb29sPiA9PiBhcyh4LCBOdW0pLnZhbCA8IGFzKHksIE51bSkudmFsID8gdHJ1ZV8oKSA6IGZhbHNlXygpXG5jb25zdCBtaW51cyA9ICh4OiBOdW0sIHk6IE51bSk6IEFubm90YXRlZDxOdW0+ID0+IG51bShhcyh4LCBOdW0pLnZhbCAtIGFzKHksIE51bSkudmFsKVxuY29uc3QgcGx1cyA9ICh4OiBOdW0sIHk6IE51bSk6IEFubm90YXRlZDxOdW0+ID0+IG51bShhcyh4LCBOdW0pLnZhbCArIGFzKHksIE51bSkudmFsKVxuY29uc3QgcG93ID0gKHg6IE51bSwgeTogTnVtKTogQW5ub3RhdGVkPE51bT4gPT4gbnVtKGFzKHgsIE51bSkudmFsICoqIGFzKHksIE51bSkudmFsKVxuY29uc3QgdGltZXMgPSAoeDogTnVtLCB5OiBOdW0pOiBBbm5vdGF0ZWQ8TnVtPiA9PiBudW0oYXMoeCwgTnVtKS52YWwgKiBhcyh5LCBOdW0pLnZhbClcblxuLy8gQ29udmVuaWVuY2UgbWV0aG9kcyBmb3IgYnVpbGRpbmcgdGhlIG1hcHMuIEV4cG9ydCB0byBhbGxvdyBvdGhlciBtb2R1bGVzIHRvIHByb3ZpZGUgb3BlcmF0aW9ucy5cbmV4cG9ydCBmdW5jdGlvbiB1bmFyeV88VCBleHRlbmRzIFByaW1WYWx1ZSwgViBleHRlbmRzIFZhbHVlPiAob3A6IFVuYXJ5PFQsIFY+KTogQW5ub3RhdGVkPFVuYXJ5T3A+IHtcbiAgIHJldHVybiBhbm5vdGF0ZWRBdCjOvSgpLCBVbmFyeU9wLCBvcC5uYW1lLCBvcClcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGJpbmFyeV88VCBleHRlbmRzIFByaW1WYWx1ZSwgVSBleHRlbmRzIFByaW1WYWx1ZSwgViBleHRlbmRzIFZhbHVlPiAob3A6IEJpbmFyeTxULCBVLCBWPik6IEFubm90YXRlZDxCaW5hcnlPcD4ge1xuICAgcmV0dXJuIGFubm90YXRlZEF0KM69KCksIEJpbmFyeU9wLCBvcC5uYW1lLCBvcClcbn1cblxuLy8gUHJpbWl0aXZlcyB3aXRoIGlkZW50aWZpZXJzIGFzIG5hbWVzIGFyZSB1bmFyeSBhbmQgZmlyc3QtY2xhc3MuXG5leHBvcnQgY29uc3QgdW5hcnlPcHM6IE1hcDxzdHJpbmcsIEFubm90YXRlZDxVbmFyeU9wPj4gPSBuZXcgTWFwKFtcbiAgIFtjZWlsaW5nLm5hbWUsIHVuYXJ5XyhjZWlsaW5nKV0sXG4gICBbZXJyb3IubmFtZSwgdW5hcnlfKGVycm9yKV0sXG4gICBbZmxvb3IubmFtZSwgdW5hcnlfKGZsb29yKV0sXG4gICBbbG9nLm5hbWUsIHVuYXJ5Xyhsb2cpXSxcbiAgIFtudW1Ub1N0ci5uYW1lLCB1bmFyeV8obnVtVG9TdHIpXSxcbiAgIFt0cmFjZS5uYW1lLCB1bmFyeV8odHJhY2UpXVxuXSlcbiAgIFxuZXhwb3J0IGNvbnN0IGJpbmFyeU9wczogTWFwPHN0cmluZywgQW5ub3RhdGVkPEJpbmFyeU9wPj4gPSBuZXcgTWFwKFtcbiAgIFtcIi1cIiwgYmluYXJ5XyhtaW51cyldLFxuICAgW1wiK1wiLCBiaW5hcnlfKHBsdXMpXSxcbiAgIFtcIipcIiwgYmluYXJ5Xyh0aW1lcyldLFxuICAgW1wiKipcIiwgYmluYXJ5Xyhwb3cpXSxcbiAgIFtcIi9cIiwgYmluYXJ5XyhkaXYpXSxcbiAgIFtcIj09XCIsIGJpbmFyeV8oZXF1YWxJbnQpXSxcbiAgIFtcIj09PVwiLCBiaW5hcnlfKGVxdWFsU3RyKV0sXG4gICBbXCI+XCIsIGJpbmFyeV8oZ3JlYXRlckludCldLFxuICAgW1wiPj1cIiwgYmluYXJ5XyhncmVhdGVyRXFJbnQpXSxcbiAgIFtcIj49PVwiLCBiaW5hcnlfKGdyZWF0ZXJFcVN0cildLFxuICAgW1wiPFwiLCBiaW5hcnlfKGxlc3NJbnQpXSxcbiAgIFtcIjw9XCIsIGJpbmFyeV8obGVzc0VxSW50KV0sXG4gICBbXCI8PT1cIiwgYmluYXJ5XyhsZXNzRXFTdHIpXSxcbiAgIFtcIisrXCIsIGJpbmFyeV8oY29uY2F0KV1cbl0pXG4iLCJpbXBvcnQgeyBDbGFzcywgYXNzZXJ0IH0gZnJvbSBcIi4vdXRpbC9Db3JlXCJcbmltcG9ydCB7IE9yZCB9IGZyb20gXCIuL3V0aWwvT3JkXCJcblxuLy8gVXNlIHRvIGluaXRpYWxpc2UgZmllbGRzIGZvciByZWZsZWN0aW9uLCB3aXRob3V0IHJlcXVpcmluZyBjb25zdHJ1Y3RvcnMuXG5leHBvcnQgY29uc3QgXzogYW55ID0gdW5kZWZpbmVkIFxuXG4vLyBTb21ld2hhdCBwZXJ2ZXJzZSB0byBkbyB0aGlzLCBidXQgbmVlZCBzb21lIHR5cGUgc2FmZXR5IVxuZXhwb3J0IHR5cGUgRGF0YVZhbHVlVGFnID1cbiAgIFwiR3JhcGhpY1wiIHwgXCJQb2x5bGluZVwiIHwgXCJQb2x5Z29uXCIgfCBcIlRleHRcIiB8IFxuICAgXCJCb29sXCIgfCBcIkNsb3N1cmVcIiB8IFwiRGF0YUV4cGxcIiB8IFwiRWxpbVwiIHwgXCJNYXRjaFwiIHwgXCJFbnZcIiB8IFwiRXhwbFwiIHwgXCJFeHBsLkRlZlwiIHwgXCJFeHBsLlJlY0RlZlwiIHwgXCJFeHBsVmFsdWVcIiB8IFwiRXhwclwiIHwgXCJFeHByLkRlZlwiIHwgIFxuICAgXCJUcmFuc2xhdGVcIiB8IFwiTGlzdFwiIHwgXCJPcHRpb25cIiB8IFwiT3JkZXJpbmdcIiB8IFwiUGFpclwiIHwgXCJQbHVnXCIgfCBcIlBvaW50XCIgfCBcIlJlY0RlZlwiIHwgXCJSZWN0XCIgfCBcIlRyZWVcIiB8IFwiVG9rZW5cIiB8IFwiVHJpZVwiXG5leHBvcnQgdHlwZSBMZXhlbWVUYWcgPSBcIldoaXRlc3BhY2VcIiB8IFwiU2luZ2xlTGluZUNvbW1lbnRcIiB8IFwiT3BlcmF0b3JcIlxuZXhwb3J0IHR5cGUgUHJpbU9wVGFnID0gXCJVbmFyeU9wXCIgfCBcIkJpbmFyeU9wXCJcbmV4cG9ydCB0eXBlIFZhbHVlVGFnID0gRGF0YVZhbHVlVGFnIHwgTGV4ZW1lVGFnIHwgUHJpbU9wVGFnIHwgXCJJZFwiIHwgXCJOdW1cIiB8IFwiU3RyXCJcblxuLy8gVmFsdWUgaW4gdGhlIG1ldGFsYW5ndWFnZS4gTm9taW5hbCBpZGlvbSBicmVha3MgZG93biBoZXJlIGluIHJlcXVpcmluZyB1c2Ugb2YgXCJhbnlcIi5cbmV4cG9ydCBjbGFzcyBWYWx1ZTxUYWcgZXh0ZW5kcyBWYWx1ZVRhZyA9IFZhbHVlVGFnPiB7XG4gICByZWFkb25seSBfX3RhZzogVGFnXG5cbiAgIGZpZWxkVmFsdWVzICgpOiBQZXJzaXN0ZW50W10ge1xuICAgICAgcmV0dXJuIGZpZWxkcyh0aGlzKS5tYXAoayA9PiAodGhpcyBhcyBhbnkgYXMgU3RhdGUpW2tdKVxuICAgfVxufVxuXG4vLyBBZGRyZXNzIG9yIGxvY2F0aW9uIG9mIHBlcnNpc3RlbnQgb2JqZWN0LlxuZXhwb3J0IGFic3RyYWN0IGNsYXNzIElkIGV4dGVuZHMgVmFsdWU8XCJJZFwiPiB7XG59XG5cbmNsYXNzIEZ1bmN0aW9uSWQgZXh0ZW5kcyBJZCB7XG4gICBmOiBGdW5jdGlvbiA9IF9cblxuICAgZ2V0IGFyZ3MgKCk6IFBlcnNpc3RlbnRbXSB7XG4gICAgICByZXR1cm4gW11cbiAgIH1cbn1cblxuZnVuY3Rpb24gZnVuY3Rpb25JZCAoZjogRnVuY3Rpb24pOiBGdW5jdGlvbklkIHtcbiAgIHJldHVybiBtYWtlKEZ1bmN0aW9uSWQsIGYpXG59XG5cbmNsYXNzIEFwcGxpY2F0aW9uSWQgZXh0ZW5kcyBJZCB7XG4gICBrOiBNZW1vSWQgPSBfXG4gICB2OiBQZXJzaXN0ZW50ID0gX1xuXG4gICBnZXQgYXJncyAoKTogUGVyc2lzdGVudFtdIHtcbiAgICAgIGNvbnN0IHbMhTogUGVyc2lzdGVudFtdID0gdGhpcy5rLmFyZ3NcbiAgICAgIHbMhS5wdXNoKHRoaXMudilcbiAgICAgIHJldHVybiB2zIVcbiAgIH1cbn1cblxuZXhwb3J0IHR5cGUgTWVtb0lkID0gRnVuY3Rpb25JZCB8IEFwcGxpY2F0aW9uSWRcblxuZnVuY3Rpb24gYXBwbGljYXRpb25JZCAoazogTWVtb0lkLCB2OiBQZXJzaXN0ZW50KTogQXBwbGljYXRpb25JZCB7XG4gICByZXR1cm4gbWFrZShBcHBsaWNhdGlvbklkLCBrLCB2KVxufVxuXG5leHBvcnQgY2xhc3MgVGFnZ2VkSWQ8VCBleHRlbmRzIElkLCBUYWcgZXh0ZW5kcyBzdHJpbmc+IGV4dGVuZHMgSWQge1xuICAgazogVCA9IF9cbiAgIHRhZzogVGFnID0gX1xufVxuXG5leHBvcnQgZnVuY3Rpb24gdGFnZ2VkSWQ8VCBleHRlbmRzIElkLCBUYWcgZXh0ZW5kcyBzdHJpbmc+IChrOiBULCB0YWc6IFRhZyk6IFRhZ2dlZElkPFQsIFRhZz4ge1xuICAgcmV0dXJuIG1ha2UoVGFnZ2VkSWQsIGssIHRhZykgYXMgVGFnZ2VkSWQ8VCwgVGFnPlxufVxuXG5leHBvcnQgZnVuY3Rpb24gbWVtb0lkIChmOiBGdW5jdGlvbiwgdsyFOiBJQXJndW1lbnRzKTogTWVtb0lkIHtcbiAgIGNvbnN0IGbKuTogRnVuY3Rpb25JZCA9IGZ1bmN0aW9uSWQoZilcbiAgIGxldCBrOiBNZW1vSWQgPSBmyrlcbiAgIGZvciAobGV0IHYgb2YgdsyFKSB7XG4gICAgICBrID0gYXBwbGljYXRpb25JZChrLCB2KVxuICAgfVxuICAgcmV0dXJuIGtcbn1cblxuLy8gRnVuY3Rpb25zIGFyZSBwZXJzaXN0ZW50IHRvIHN1cHBvcnQgcHJpbWl0aXZlcy4gUHJpbWl0aXZlIGRhdGF0eXBlcyBsaWtlIE51bSBhbmQgU3RyIGNvbnRhaW5cbi8vIEVTNiBwcmltaXRpdmVzIGxpa2UgbnVtYmVyIGFuZCBzdHJpbmcsIHdoaWNoIGFyZSAoY3VycmVudGx5KSBcInBlcnNpc3RlbnRcIiBmb3IgaW50ZXJuaW5nIHB1cnBvc2VzXG4vLyBidXQgYXJlIG5vdCBcInZhbHVlc1wiIGJlY2F1c2UgdGhleSBhcmUgbm90IG9ic2VydmFibGUgdG8gdXNlciBjb2RlLlxuZXhwb3J0IHR5cGUgUGVyc2lzdGVudCA9IFZhbHVlIHwgc3RyaW5nIHwgbnVtYmVyIHwgRnVuY3Rpb25cblxuZXhwb3J0IHR5cGUgUHJpbVZhbHVlID0gTnVtIHwgU3RyXG5cbmV4cG9ydCBjbGFzcyBOdW0gZXh0ZW5kcyBWYWx1ZTxcIk51bVwiPiB7XG4gICB2YWw6IG51bWJlciA9IF9cblxuICAgdG9TdHJpbmcgKCk6IHN0cmluZyB7XG4gICAgICByZXR1cm4gdGhpcy52YWwudG9TdHJpbmcoKVxuICAgfVxufVxuXG5leHBvcnQgY2xhc3MgU3RyIGV4dGVuZHMgVmFsdWU8XCJTdHJcIj4gaW1wbGVtZW50cyBPcmQ8U3RyPiB7XG4gICB2YWw6IHN0cmluZyA9IF9cblxuICAgdG9TdHJpbmcgKCk6IHN0cmluZyB7XG4gICAgICByZXR1cm4gYFwiJHt0aGlzLnZhbH1cImBcbiAgIH1cblxuICAgbGVxIChzdHI6IFN0cik6IGJvb2xlYW4ge1xuICAgICAgcmV0dXJuIHRoaXMudmFsLmxvY2FsZUNvbXBhcmUoc3RyLnZhbCkgPD0gMFxuICAgfVxuXG4gICBlcSAoc3RyOiBTdHIpOiBib29sZWFuIHtcbiAgICAgIHJldHVybiB0aGlzLnZhbC5sb2NhbGVDb21wYXJlKHN0ci52YWwpID09PSAwXG4gICB9XG5cbiAgIGdlcSAoc3RyOiBTdHIpOiBib29sZWFuIHtcbiAgICAgIHJldHVybiB0aGlzLnZhbC5sb2NhbGVDb21wYXJlKHN0ci52YWwpID49IDBcbiAgIH1cbn1cblxuLy8gRHluYW1pYyBpbnRlcmZhY2UgdG8gYSB2YWx1ZSBvYmplY3QuXG5leHBvcnQgaW50ZXJmYWNlIFN0YXRlIHtcbiAgIFtwcm9wOiBzdHJpbmddOiBQZXJzaXN0ZW50XG59XG5cbi8vIEN1cnJpZWQgbWFwIGZyb20gY29uc3RydWN0b3JzIGFuZCBhcmd1bWVudHMgdG8gY2FjaGVkIHZhbHVlczsgY3VycmllZCBiZWNhdXNlIGNvbXBvc2l0ZSBrZXlzIHdvdWxkIFxuLy8gcmVxdWlyZSBlaXRoZXIgY3VzdG9tIGVxdWFsaXR5LCB3aGljaCBpc24ndCBwb3NzaWJsZSB3aXRoIEVTNiBtYXBzLCBvciBpbnRlcm5pbmcsIHdoaWNoIHdvdWxkIGVzc2VudGlhbGx5XG4vLyBpbnZvbHZlIHRoZSBzYW1lIG1lbW9pc2F0aW9uIGxvZ2ljLlxudHlwZSBNZW1vVGFibGUgPSBNYXA8UGVyc2lzdGVudCwgUGVyc2lzdGVudCB8IE1hcDxQZXJzaXN0ZW50LCBPYmplY3Q+PiAvLyBhcHByb3hpbWF0ZSByZWN1cnNpdmUgdHlwZVxuXG4vLyBIYXNoLWNvbnNlZCBjb25zdHJ1Y3RvcnMgYXJlIGludmFyaWFudCBhY3Jvc3Mgd29ybGRzLCB3aGVyZWFzIGZ1bmN0aW9ucyBhcmUgbm90LlxuY29uc3QgX19jdHJNZW1vOiBNZW1vVGFibGUgPSBuZXcgTWFwXG5cbmZ1bmN0aW9uIGxvb2t1cEFyZzxUIGV4dGVuZHMgUGVyc2lzdGVudD4gKGY6IE1lbW9pc2FibGU8VD4sIG06IE1lbW9UYWJsZSwgdsyFOiBQZXJzaXN0ZW50W10sIG46IG51bWJlcik6IFBlcnNpc3RlbnQgfCBNYXA8UGVyc2lzdGVudCwgT2JqZWN0PiB7XG4gICAvLyBmb3IgbWVtb2lzYXRpb24gcHVycG9zZXMsIHRyZWF0IGYncyBrZXkgYXMgYXJndW1lbnQgLTFcbiAgIGNvbnN0IGs6IFBlcnNpc3RlbnQgPSBuID09PSAtMSA/IGYua2V5IDogdsyFW25dXG4gICBsZXQgdjogUGVyc2lzdGVudCB8IE1hcDxQZXJzaXN0ZW50LCBPYmplY3Q+IHwgdW5kZWZpbmVkID0gbS5nZXQoaylcbiAgIGlmICh2ID09PSB1bmRlZmluZWQpIHtcbiAgICAgIGlmIChuID09PSB2zIUubGVuZ3RoIC0gMSkge1xuICAgICAgICAgdiA9IGYuY2FsbCh2zIUpXG4gICAgICAgICB2ID0gdiEgLy8gVFMgY29uZnVzZWQ7IHRoaW5rcyB2IGNhbiBiZSB1bmRlZmluZWQgaGVyZVxuICAgICAgfSBlbHNlIHtcbiAgICAgICAgIHYgPSBuZXcgTWFwXG4gICAgICB9XG4gICAgICBtLnNldChrLCB2KVxuICAgfVxuICAgcmV0dXJuIHZcbn1cblxuLy8gVW5pZnkgbWVtby1mdW5jdGlvbnMgYW5kIGludGVybmVkIGNsYXNzZXMuXG5pbnRlcmZhY2UgTWVtb2lzYWJsZTxUIGV4dGVuZHMgUGVyc2lzdGVudD4ge1xuICAga2V5OiBQZXJzaXN0ZW50XG4gICBjYWxsIChhcmdzOiBQZXJzaXN0ZW50W10pOiBUXG59XG5cbmNsYXNzIE1lbW9DdHI8VCBleHRlbmRzIFZhbHVlPiBpbXBsZW1lbnRzIE1lbW9pc2FibGU8VD4ge1xuICAgQzogQ2xhc3M8VD5cblxuICAgY29uc3RydWN0b3IgKEM6IENsYXNzPFQ+KSB7XG4gICAgICB0aGlzLkMgPSBDXG4gICB9XG5cbiAgIGdldCBrZXkgKCk6IFBlcnNpc3RlbnQge1xuICAgICAgcmV0dXJuIHRoaXMuQ1xuICAgfSBcblxuICAgY2FsbCAodsyFOiBQZXJzaXN0ZW50W10pOiBUIHtcbiAgICAgIGNvbnN0IG86IFQgPSBuZXcgdGhpcy5DXG4gICAgICBjb25zdHJ1Y3QobywgdsyFKVxuICAgICAgT2JqZWN0LmZyZWV6ZShvKSBcbiAgICAgIHJldHVybiBvXG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtZW1vQ2FsbDxUIGV4dGVuZHMgUGVyc2lzdGVudD4gKG1lbW86IE1lbW9UYWJsZSwgZjogTWVtb2lzYWJsZTxUPiwgdsyFOiBQZXJzaXN0ZW50W10pOiBUIHtcbiAgIGxldCB2OiBQZXJzaXN0ZW50IHwgTWFwPFBlcnNpc3RlbnQsIE9iamVjdD4gPSBsb29rdXBBcmcoZiwgbWVtbywgdsyFLCAtMSlcbiAgIGZvciAobGV0IG46IG51bWJlciA9IDA7IG4gPCB2zIUubGVuZ3RoOyArK24pIHtcbiAgICAgIC8vIHNpbmNlIHRoZXJlIGFyZSBtb3JlIGFyZ3VtZW50cywgdGhlIGxhc3QgdiB3YXMgYSAocG9zc2libHkgbmVzdGVkKSBtYXBcbiAgICAgIHYgPSBsb29rdXBBcmcoZiwgdiBhcyBNZW1vVGFibGUsIHbMhSwgbilcbiAgIH1cbiAgIHJldHVybiB2IGFzIFRcbn1cblxuLy8gRXhwZXJpbWVudGVkIHdpdGggZGljdGlvbmFyeS1iYXNlZCBjb25zdHJ1Y3Rpb24gcGF0dGVybjsgZWxpbWluYXRlcyBmaWVsZCBvcmRlciBtaXNtYXRjaCBhcyBhIHBvc3NpYmxlXG4vLyBzb3VyY2Ugb2YgZXJyb3IsIGJ1dCB0aGUgYmVuZWZpdCBpcyB2ZXJ5IHNtYWxsIGFuZCBkb2Vzbid0IHJlYWxseSBzdWl0IHRoZSBtZW1vaXNhdGlvbiBwYXR0ZXJuLlxuZXhwb3J0IGZ1bmN0aW9uIG1ha2U8VCBleHRlbmRzIFZhbHVlPiAoQzogQ2xhc3M8VD4sIC4uLnbMhTogUGVyc2lzdGVudFtdKTogVCB7XG4gICByZXR1cm4gbWVtb0NhbGwoX19jdHJNZW1vLCBuZXcgTWVtb0N0cihDKSwgdsyFKVxufVxuXG4vLyBEZXBlbmRzIGhlYXZpbHkgb24gKDEpIGdldE93blByb3BlcnR5TmFtZXMoKSByZXR1cm5pbmcgZmllbGRzIGluIGRlZmluaXRpb24tb3JkZXI7IGFuZCAoMilcbi8vIGNvbnN0cnVjdG9yIGZ1bmN0aW9ucyBzdXBwbHlpbmcgYXJndW1lbnRzIGluIHRoZSBzYW1lIG9yZGVyLlxuZXhwb3J0IGZ1bmN0aW9uIGNvbnN0cnVjdDxUIGV4dGVuZHMgVmFsdWU+ICh0Z3Q6IFQsIHbMhTogUGVyc2lzdGVudFtdKTogVCB7XG4gICBjb25zdCB0Z3TKuTogU3RhdGUgPSB0Z3QgYXMgYW55IGFzIFN0YXRlLFxuICAgICAgICAgZsyFOiBzdHJpbmdbXSA9IGZpZWxkcyh0Z3QpXG4gICBhc3NlcnQoZsyFLmxlbmd0aCA9PT0gdsyFLmxlbmd0aClcbiAgIGxldCBuOiBudW1iZXIgPSAwXG4gICBmzIUuZm9yRWFjaCgoZjogc3RyaW5nKTogdm9pZCA9PiB7XG4gICAgICB0Z3TKuVtmXSA9IHbMhVtuKytdXG4gICB9KVxuICAgcmV0dXJuIHRndFxufVxuXG4vLyBFeGNsdWRlIG1ldGFkYXRhIGFjY29yZGluZyB0byBvdXIgY29udmVudGlvbi5cbmV4cG9ydCBmdW5jdGlvbiBpc0ZpZWxkIChwcm9wOiBzdHJpbmcpOiBib29sZWFuIHtcbiAgIHJldHVybiAhcHJvcC5zdGFydHNXaXRoKFwiX19cIilcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGZpZWxkcyAodjogVmFsdWUpOiBzdHJpbmdbXSB7XG4gICByZXR1cm4gT2JqZWN0LmdldE93blByb3BlcnR5TmFtZXModikuZmlsdGVyKGlzRmllbGQpXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBtZXRhZGF0YUZpZWxkcyAodjogVmFsdWUpOiBzdHJpbmdbXSB7XG4gICByZXR1cm4gT2JqZWN0LmdldE93blByb3BlcnR5TmFtZXModikuZmlsdGVyKGYgPT4gIWlzRmllbGQoZikgJiYgZiAhPT0gXCJfX2lkXCIpXG59XG4iLCJpbXBvcnQgeyBDbGFzcywgX19ub25OdWxsLCBjbGFzc09mLCBub3RZZXRJbXBsZW1lbnRlZCB9IGZyb20gXCIuL3V0aWwvQ29yZVwiXG5pbXBvcnQgeyBJZCwgUGVyc2lzdGVudCwgVmFsdWUsIF8sIGNvbnN0cnVjdCwgbWFrZSwgbWV0YWRhdGFGaWVsZHMgfSBmcm9tIFwiLi9WYWx1ZVwiXG5cbi8vIFZlcnNpb25lZCBvYmplY3RzIGFyZSBwZXJzaXN0ZW50IG9iamVjdHMgdGhhdCBoYXZlIHN0YXRlIHRoYXQgdmFyaWVzIGFjcm9zcyB3b3JsZHMuIEludGVyZmFjZSBiZWNhdXNlIHRoZSBcbi8vIHNhbWUgZGF0YXR5cGUgY2FuIGJlIGludGVybmVkIGluIHNvbWUgY29udGV4dHMgYW5kIHZlcnNpb25lZCBpbiBvdGhlcnMuXG5leHBvcnQgdHlwZSBWZXJzaW9uZWQ8VD4gPSBWZXJzaW9uZWRfICYgVFxuXG5leHBvcnQgaW50ZXJmYWNlIFZlcnNpb25lZF8ge1xuICAgX19pZDogSWRcbn1cblxuLy8gRm9yIHZlcnNpb25lZCBvYmplY3RzIHRoZSBtYXAgaXMgbm90IGN1cnJpZWQgYnV0IHRha2VzIGFuIChpbnRlcm5lZCkgY29tcG9zaXRlIGtleS5cbnR5cGUgVmVyc2lvbmVkVmFsdWVzID0gTWFwPElkLCBWZXJzaW9uZWQ8VmFsdWU+PlxuY29uc3QgX192ZXJzaW9uZWQ6IFZlcnNpb25lZFZhbHVlcyA9IG5ldyBNYXBcblxuLy8gVGhlIChwb3NzaWJseSBhbHJlYWR5IGV4dGFudCkgdmVyc2lvbmVkIG9iamVjdCB1bmlxdWVseSBpZGVudGlmaWVkIGJ5IGEgbWVtby1rZXkuXG5leHBvcnQgZnVuY3Rpb24gYXQ8VCBleHRlbmRzIFZhbHVlPiAoazogSWQsIEM6IENsYXNzPFQ+LCAuLi52zIU6IFBlcnNpc3RlbnRbXSk6IFZlcnNpb25lZDxUPiB7XG4gICBsZXQgdjogVmVyc2lvbmVkPFZhbHVlPiB8IHVuZGVmaW5lZCA9IF9fdmVyc2lvbmVkLmdldChrKVxuICAgaWYgKHYgPT09IHVuZGVmaW5lZCkge1xuICAgICAgY29uc3QgdjogVCA9IG5ldyBDXG4gICAgICBPYmplY3QuZGVmaW5lUHJvcGVydHkodiwgXCJfX2lkXCIsIHtcbiAgICAgICAgIHZhbHVlOiBrLFxuICAgICAgICAgZW51bWVyYWJsZTogZmFsc2VcbiAgICAgIH0pXG4gICAgICBjb25zdCB2yrk6IFZlcnNpb25lZDxUPiA9IHYgYXMgVmVyc2lvbmVkPFQ+XG4gICAgICBfX3ZlcnNpb25lZC5zZXQoaywgdsq5KVxuICAgICAgcmV0dXJuIGNvbnN0cnVjdCh2yrksIHbMhSlcbiAgIH0gZWxzZVxuICAgaWYgKHYgaW5zdGFuY2VvZiBDKSB7IFxuICAgICAgcmV0dXJuIGNvbnN0cnVjdCh2LCB2zIUpIC8vIGhtbSwgVFMgdGhpbmtzIHYgaXMgdmVyc2lvbmVkIGhlcmUgLSB3aHk/XG4gICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIHJlY2xhc3NpZnkodiwgQylcbiAgIH1cbn1cblxuLy8gU2hvdWxkIGVtdWxhdGUgdGhlIHBvc3Qtc3RhdGUgb2YgXCJuZXcgQ1wiLiBQcm9iYWJseSBuZWVkIHRvIHdvcnJ5IGFib3V0IGhvdyB0aGlzIHdvcmtzIHdpdGggaW5oZXJpdGVkIHByb3BlcnRpZXMuXG5mdW5jdGlvbiByZWNsYXNzaWZ5PFQgZXh0ZW5kcyBWYWx1ZT4gKHY6IFZlcnNpb25lZDxWYWx1ZT4sIGN0cjogQ2xhc3M8VD4pOiBWZXJzaW9uZWQ8VD4ge1xuICAgcmV0dXJuIG5vdFlldEltcGxlbWVudGVkKClcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGNvcHlBdDxUIGV4dGVuZHMgVmFsdWU+IChrOiBJZCwgdjogVCk6IFZlcnNpb25lZDxUPiB7XG4gICBjb25zdCB2yrk6IFZlcnNpb25lZDxUPiA9IGF0KGssIGNsYXNzT2YodiksIC4uLnYuZmllbGRWYWx1ZXMoKSlcbiAgIG1ldGFkYXRhRmllbGRzKHYpLmZvckVhY2goKHByb3A6IHN0cmluZykgPT4ge1xuICAgICAgKHbKuSBhcyBhbnkpW3Byb3BdID0gKHYgYXMgYW55KVtwcm9wXVxuICAgfSlcbiAgIHJldHVybiB2yrlcbn1cblxuLy8gQSBtZW1vIGtleSB3aGljaCBpcyBzb3VyY2VkIGV4dGVybmFsbHkgdG8gdGhlIHN5c3RlbS4gKFRoZSBuYW1lIFwiRXh0ZXJuYWxcIiBpcyBhbHJlYWR5IHRha2VuLilcbmV4cG9ydCBjbGFzcyBFeHRlcm4gZXh0ZW5kcyBJZCB7XG4gICBpZDogbnVtYmVyID0gX1xufVxuXG5mdW5jdGlvbiBleHRlcm4gKGlkOiBudW1iZXIpOiBFeHRlcm4ge1xuICAgcmV0dXJuIG1ha2UoRXh0ZXJuLCBpZClcbn1cblxuLy8gRnJlc2gga2V5cyByZXByZXNlbnQgaW5wdXRzIHRvIHRoZSBzeXN0ZW0sIGUuZy4gYWRkcmVzc2VzIG9mIHN5bnRheCBub2RlcyBwcm92aWRlZCBieSBhbiBleHRlcm5hbCBzdHJ1Y3R1cmUgZWRpdG9yLlxuZXhwb3J0IGNvbnN0IM69OiAoKSA9PiBFeHRlcm4gPVxuICAgKCgpID0+IHtcbiAgICAgIGxldCBjb3VudDogbnVtYmVyID0gMFxuICAgICAgcmV0dXJuICgpID0+IHtcbiAgICAgICAgIHJldHVybiBleHRlcm4oY291bnQrKylcbiAgICAgIH1cbiAgIH0pKClcbiIsImltcG9ydCB7IF9fbm9uTnVsbCB9IGZyb20gXCIuL0NvcmVcIlxuXG5leHBvcnQgZnVuY3Rpb24gZmxhdHRlbjxUPiAoeMyFzIU6IFRbXVtdKTogVFtdIHtcbiAgIHJldHVybiBbXS5jb25jYXQuYXBwbHkoW10sIHjMhcyFKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gY291bnRzPFQ+ICh4zIU6IFRbXSk6IE1hcDxULCBudW1iZXI+IHtcbiAgIGNvbnN0IGNvdW50czogTWFwPFQsIG51bWJlcj4gPSBuZXcgTWFwXG4gICB4zIUuZm9yRWFjaCh4ID0+IHtcbiAgICAgIGlmIChjb3VudHMuaGFzKHgpKSB7XG4gICAgICAgICBjb3VudHMuc2V0KHgsIF9fbm9uTnVsbChjb3VudHMuZ2V0KHgpKSArIDEpXG4gICAgICB9IGVsc2Uge1xuICAgICAgICAgY291bnRzLnNldCh4LCAxKVxuICAgICAgfVxuICAgfSlcbiAgIHJldHVybiBjb3VudHNcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIHppcDxULCBVPiAoeMyFOiBUW10sIHnMhTogVVtdKTogW1QsIFVdW10ge1xuICAgcmV0dXJuIHjMhS5tYXAoKHg6IFQsIG46IG51bWJlcik6IFtULCBVXSA9PiBbeCwgecyFW25dXSlcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGVxPFQ+ICh4zIU6IFRbXSwgecyFOiBUW10pOiBib29sZWFuIHtcbiAgIGxldCBuOiBudW1iZXIgPSB4zIUubGVuZ3RoXG4gICBpZiAobiAhPSB5zIUubGVuZ3RoKSB7XG4gICAgICByZXR1cm4gZmFsc2VcbiAgIH0gZWxzZSB7XG4gICAgICB3aGlsZSAobi0tKSB7XG4gICAgICAgICBpZiAoeMyFW25dICE9PSB5zIVbbl0pIHJldHVybiBmYWxzZTtcbiAgICAgIH1cbiAgICAgIHJldHVybiB0cnVlXG4gICB9XG59XG4iLCIvLyBBIG5vbWluYWwgdHlwaW5nIGlkaW9tOyBzZWUgaHR0cHM6Ly9iYXNhcmF0LmdpdGJvb2tzLmlvL3R5cGVzY3JpcHQvZG9jcy90aXBzL25vbWluYWxUeXBpbmcuaHRtbC5cbmV4cG9ydCBpbnRlcmZhY2UgVGFnPFQgZXh0ZW5kcyBzdHJpbmc+IHtcbiAgIHR5cGVuYW1lOiBUXG59XG5cbmV4cG9ydCB0eXBlIENsYXNzPFQ+ID0gbmV3ICguLi5hcmdzOiBhbnlbXSkgPT4gVFxuXG4vLyBQb3NzaWJseSBhYnN0cmFjdCBjbGFzczsgc2VlIGh0dHBzOi8vc3RhY2tvdmVyZmxvdy5jb20vcXVlc3Rpb25zLzM2ODg2MDgyLlxuZXhwb3J0IHR5cGUgQUNsYXNzPFQ+ID0gRnVuY3Rpb24gJiB7IHByb3RvdHlwZTogVCB9XG5cbmV4cG9ydCBmdW5jdGlvbiBjbGFzc09mPFQ+ICh4OiBUKTogQ2xhc3M8VD4ge1xuICAgcmV0dXJuIChfX25vbk51bGwoeCkgYXMgT2JqZWN0KS5jb25zdHJ1Y3RvciBhcyBDbGFzczxUPiAvLyB3ZWlyZGx5IGZhaWxpbmcgb24gQ2lyY2xlQ0kgd2l0aG91dCBjYXN0XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBjbGFzc05hbWUobzogT2JqZWN0KTogc3RyaW5nIHtcbiAgIHJldHVybiBjbGFzc09mKG8pLm5hbWVcbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGFzPFUsIFQgZXh0ZW5kcyBVPiAoeDogVSwgQzogQUNsYXNzPFQ+KTogVCB7XG4gICBpZiAoX19ub25OdWxsKHgpIGluc3RhbmNlb2YgQykge1xuICAgICAgcmV0dXJuIDxUPnhcbiAgIH0gZWxzZSB7XG4gICAgICByZXR1cm4gYXNzZXJ0KGZhbHNlLCBcIlthc10gRXhwZWN0ZWQgXCIgKyBDLm5hbWUgKyBcIiwgZ290IFwiICsgY2xhc3NOYW1lKHgpKVxuICAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gYXNPcHQ8VSwgVCBleHRlbmRzIFU+ICh4OiBVLCBjbHM6IEFDbGFzczxUPik6IFQge1xuICAgaWYgKHggPT09IG51bGwgfHwgeCA9PT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4geCBhcyBUXG4gICB9IGVsc2Uge1xuICAgICAgcmV0dXJuIGFzKHgsIGNscylcbiAgIH1cbn1cblxuZXhwb3J0IGZ1bmN0aW9uIGFzc2VydCAoYjogYm9vbGVhbiwgbXNnPzogc3RyaW5nLCAuLi54zIU6IGFueVtdKTogYW55IHtcbiAgIGlmICghYikge1xuICAgICAgaWYgKHjMhS5sZW5ndGggPiAwKSB7XG4gICAgICAgICBjb25zb2xlLndhcm4oXCJBc3NlcnRpb24gZGF0YTpcXG5cIilcbiAgICAgICAgIHjMhS5mb3JFYWNoKHggPT4gY29uc29sZS53YXJuKHgpKVxuICAgICAgfVxuICAgICAgdGhyb3cgbmV3IEVycm9yKG1zZyB8fCBcIkFzc2VydGlvbiBmYWlsdXJlXCIpXG4gICB9XG59XG5cbmV4cG9ydCBmdW5jdGlvbiBhYnN1cmQgKG1zZz86IHN0cmluZywgLi4ueMyFOiBhbnlbXSk6IGFueSB7XG4gICBhc3NlcnQoZmFsc2UsIG1zZywgLi4ueMyFKVxufVxuXG4vLyBVc2VyLWxldmVsIGVycm9yLlxuZXhwb3J0IGZ1bmN0aW9uIGVycm9yIChtc2c6IHN0cmluZywgLi4ueMyFOiBhbnlbXSk6IGFueSB7XG4gICBpZiAoeMyFLmxlbmd0aCA+IDApIHtcbiAgICAgIGNvbnNvbGUud2FybihcIkVycm9yIGRhdGE6XFxuXCIpXG4gICAgICB4zIUuZm9yRWFjaCh4ID0+IGNvbnNvbGUud2Fybih4KSlcbiAgIH1cbiAgIHRocm93IG5ldyBFcnJvcihcIlVzZXIgZXJyb3I6IFwiICsgbXNnKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gbm90WWV0SW1wbGVtZW50ZWQgKCk6IGFueSB7XG4gICB0aHJvdyBuZXcgRXJyb3IoXCJOb3QgeWV0IGltcGxlbWVudGVkXCIpXG59XG5cbi8vIFVzZWZ1bCB3aGVuIGEgbm90aW9uYWxseSBhYnN0cmFjdCBjbGFzcyBuZWVkcyB0byBiZSBjb25jcmV0ZS5cbmV4cG9ydCBmdW5jdGlvbiBhYnN0cmFjdE1ldGhvZEVycm9yPFQ+ICh0aGlzXzogT2JqZWN0KTogVCB7XG4gICByZXR1cm4gYXNzZXJ0KGZhbHNlLCBcIkFic3RyYWN0IG1ldGhvZCBpbiBcIiArIHRoaXNfKVxufVxuXG5leHBvcnQgZnVuY3Rpb24gX19ub25OdWxsPFQ+ICh4OiBUIHwgbnVsbCB8IHVuZGVmaW5lZCk6IFQge1xuICAgaWYgKHggIT09IG51bGwgJiYgeCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgICByZXR1cm4geFxuICAgfSBlbHNlIHtcbiAgICAgIHJldHVybiBhc3NlcnQoZmFsc2UsIFwiVW5leHBlY3RlZCBudWxsIHwgdW5kZWZpbmVkLlwiKVxuICAgfVxufVxuXG5leHBvcnQgZnVuY3Rpb24gZGVidWcgKG86IE9iamVjdCk6IHN0cmluZyB7XG4gICByZXR1cm4gY2xhc3NOYW1lKG8pICsgXCIjXCIgKyAoPGFueT5vKS5fX2lkXG59XG5cbmV4cG9ydCBmdW5jdGlvbiBsb2c8VD4gKFxuICAgeDogVCxcbiAgIG1zZz86IChpdDogVCkgPT4gc3RyaW5nLFxuICAgdHJhbnNmb3JtOiAoaXQ6IFQpID0+IFQgPSAoaXQ6IFQpID0+IGl0XG4pOiBUIHtcbiAgIGNvbnN0IHhfID0gdHJhbnNmb3JtKHgpXG4gICBpZiAobXNnKSB7XG4gICAgICBjb25zb2xlLmxvZyhtc2coeF8pKVxuICAgfVxuICAgY29uc29sZS5sb2coeF8pXG4gICByZXR1cm4geFxufVxuXG5leHBvcnQgZnVuY3Rpb24gX19jaGVjazxUPiAoeDogVCwgcHJlZGljYXRlOiAoaXQ6IFQpID0+IGJvb2xlYW4pOiBUIHtcbiAgIGFzc2VydChwcmVkaWNhdGUoeCkpXG4gICByZXR1cm4geFxufVxuIiwiaW1wb3J0IHsgX19ub25OdWxsIH0gZnJvbSBcIi4vQ29yZVwiXG5pbXBvcnQgeyBCb29sZWFuTGF0dGljZSB9IGZyb20gXCIuL09yZFwiXG5cbi8vIEFjdHVhbGx5IGEgYm9vbGVhbiBsYXR0aWNlLi4uXG5hYnN0cmFjdCBjbGFzcyBMYXR0aWNlSW1wbDxUPiBpbXBsZW1lbnRzIEJvb2xlYW5MYXR0aWNlPFQ+IHtcbiAgIGFic3RyYWN0IGJvdDogVFxuICAgYWJzdHJhY3QgdG9wOiBUXG5cbiAgIGpvaW4gKC4uLnTMhTogVFtdKTogVCB7XG4gICAgICByZXR1cm4gdMyFLnJlZHVjZSgodDEsIHQyKSA9PiB0aGlzLmpvaW4yKHQxLCB0MikpXG4gICB9XG5cbiAgIG1lZXQgKC4uLnTMhTogVFtdKTogVCB7XG4gICAgICByZXR1cm4gdMyFLnJlZHVjZSgodDEsIHQyKSA9PiB0aGlzLm1lZXQyKHQxLCB0MikpXG4gICB9XG5cbiAgIGFic3RyYWN0IGpvaW4yICh0MTogVCwgdDI6IFQpOiBUXG4gICBhYnN0cmFjdCBtZWV0MiAodDE6IFQsIHQyOiBUKTogVFxuICAgYWJzdHJhY3QgbmVnYXRlICh0OiBUKTogVFxufVxuXG5leHBvcnQgY2xhc3MgQm9vbExhdHRpY2UgZXh0ZW5kcyBMYXR0aWNlSW1wbDxib29sZWFuPiB7XG4gICBib3QgPSBmYWxzZVxuICAgdG9wID0gdHJ1ZVxuXG4gICAvLyBJbXBvcnRhbnQgdG8gYXNzZXJ0IHRoYXQgYXJndW1lbnRzIGFyZSBkZWZpbmVkIHNpbmNlIHVuZGVmaW5lZCBwcm9wYWdhdGVzIGluIGFuIHVuaGVscGZ1bCB3YXkuXG4gICBqb2luMiAoYjE6IGJvb2xlYW4sIGIyOiBib29sZWFuKTogYm9vbGVhbiB7XG4gICAgICByZXR1cm4gX19ub25OdWxsKGIxKSB8fCBfX25vbk51bGwoYjIpXG4gICB9XG5cbiAgIG1lZXQyIChiMTogYm9vbGVhbiwgYjI6IGJvb2xlYW4pOiBib29sZWFuIHtcbiAgICAgIHJldHVybiBfX25vbk51bGwoYjEpICYmIF9fbm9uTnVsbChiMilcbiAgIH1cblxuICAgbmVnYXRlIChiOiBib29sZWFuKTogYm9vbGVhbiB7XG4gICAgICByZXR1cm4gIWJcbiAgIH1cbn1cblxuZXhwb3J0IGNvbnN0IGFubjogQm9vbGVhbkxhdHRpY2U8QW5ub3RhdGlvbj4gPSBuZXcgQm9vbExhdHRpY2UoKVxuZXhwb3J0IHR5cGUgQW5ub3RhdGlvbiA9IGJvb2xlYW4gLy8gZm9yIG5vd1xuIiwiaW1wb3J0IHsgRXEgfSBmcm9tIFwiLi9FcVwiXG5cbmV4cG9ydCBpbnRlcmZhY2UgT3JkPEsgZXh0ZW5kcyBPcmQ8Sz4+IGV4dGVuZHMgRXE8Sz4ge1xuICAgLy8gVGhlIGFyZ3VtZW50IGlzIGFsd2F5cyBvZiB0aGUgdHlwZSBpbXBsZW1lbnRpbmcgT3JkLlxuICAgbGVxIChhOiBLKTogYm9vbGVhblxufVxuXG5leHBvcnQgZnVuY3Rpb24gZXE8SyBleHRlbmRzIE9yZDxLPj4oYTogSywgYjogSyk6IGJvb2xlYW4ge1xuICAgcmV0dXJuIGEubGVxKGIpICYmIGIubGVxKGEpXG59XG5cbmV4cG9ydCB0eXBlIENvbXBhcmF0b3I8VD4gPSAoeDogVCwgeTogVCkgPT4gbnVtYmVyXG5cbmV4cG9ydCBpbnRlcmZhY2UgSm9pblNlbWlsYXR0aWNlPFQ+IHtcbiAgIGpvaW4gKC4uLnRzOiBUW10pOiBUXG4gICBib3Q6IFRcbn1cblxuZXhwb3J0IGludGVyZmFjZSBNZWV0U2VtaWxhdHRpY2U8VD4ge1xuICAgbWVldCAoLi4udHM6IFRbXSk6IFRcbiAgIHRvcDogVFxufVxuXG5leHBvcnQgaW50ZXJmYWNlIExhdHRpY2U8VD4gZXh0ZW5kcyBKb2luU2VtaWxhdHRpY2U8VD4sIE1lZXRTZW1pbGF0dGljZTxUPiB7XG59XG5cbmV4cG9ydCBpbnRlcmZhY2UgQm9vbGVhbkxhdHRpY2U8VD4gZXh0ZW5kcyBMYXR0aWNlPFQ+IHtcbiAgIG5lZ2F0ZSAodDogVCk6IFRcbn1cbiJdLCJzb3VyY2VSb290IjoiIn0=
"use strict"

function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

exports.curry2 = curry2

/* global exports, it, describe */

// module Test.Spec.Mocha2

if (typeof describe !== 'function' || typeof it !== 'function') {
    throw new Error('Mocha globals seem to be unavailable!');
}

/*
    only :: Boolean
    name :: String
    run  :: (Effect Unit -> (Error -> Effect Unit) -> Effect Unit)
    done :: Error -> Effect Unit

*/
export function itAsync(only) {
    "use strict";
    return function (name) {
        return function (run) {
            return function () {
                var f = only ? it.only : it;
                f(name, function (done) {     // f :: String -> 
                    return run(function () {
                        done();
                        return function () {};
                    })(function (err) {
                        done(err);
                        return function () {};
                    })();
                });
            };
        };
    };
}

export function itPending(name) {
    "use strict";
    return function () {
        it(name);
    };
}

export function describe(only) {
    "use strict";
    return function (name) {
        return function (nested) {
            return function () {
                var f = only ? describe.only : describe;
                f(name, function () {
                    nested();
                });
            };
        };
    };
}

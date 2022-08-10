/* global exports, it, describe */

// module Test.Spec.Mocha

if (typeof describe !== 'function' || typeof it !== 'function') {
    throw new Error('Mocha globals seem to be unavailable!');
}

exports.itAsync = function (only) {
    "use strict";
    return function (name) {
        return function (run) {
            return function () {
                var f = only ? it.only : it;
                f(name, function (done) {
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
};

exports.itPending = function (name) {
    "use strict";
    return function () {
        it(name);
    };
};

exports.describe = function (only) {
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
};

var extend = require('raptor-util').extend;
var tryRequire = require('raptor-util').tryRequire;

function isPromise(promise) {
    return promise != null && typeof promise.then === 'function';
}

function makePromise(v) {
    if (isPromise(v)) {
        return v;
    }
    else {
        var promises = tryRequire('../../');
        if (promises) {
            return promises.resolved(v);
        }
        else {
            return v;
        }
    }
}

extend(exports, {
    isPromise: isPromise,

    valueOfPromise: function(p) {
        if (!p) {
            return p;
        }

        if (typeof p.then !== 'function') {
            return p; //It is not a promise, just return it
        }
        
        if (p && typeof p.inspect === 'function') {
            var inspected = p.inspect();
            if (inspected.state === 'fulfilled') {
                return inspected.value;
            }
            else {
                return undefined;
            }
        }
        else {
            return undefined;
        }
    },

    immediateThen: function(p, resolvedCallback, rejectedCallback) {
        var result;

        if (!isPromise(p)) {
            result = resolvedCallback(p);
            return makePromise(result);
        }
        else if (isPromise(p) && typeof p.inspect === 'function') {
            var inspected = p.inspect();
            if (inspected.state === 'fulfilled') {
                result = resolvedCallback(inspected.value);
                return makePromise(result); // Make sure it is a promise
            }
            else if (inspected.state === 'rejected') {
                result = rejectedCallback(inspected.reason);
                return makePromise(result); // Make sure it is a promise
            }
        }

        // Fall-through for the pending state or lack of "inspect"
        return p.then(resolvedCallback, rejectedCallback);
    }
});
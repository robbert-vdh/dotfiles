var extend = require('raptor-util').extend;
var _global = typeof global === 'undefined' ? window : global;
var provider;

function getImpl() {
    if (provider) {
        return provider;
    }

    var configuredProvider = exports.provider;
    if (configuredProvider) {
        if (typeof configuredProvider === 'string') {
            provider = require(configuredProvider);
        }
        else {
            provider = configuredProvider;
        }
    }
    else {
        throw new Error('No promise provider found');
    }

    return provider;
}

extend(exports, {
    provider: 'q',

    defer: function() {
        return getImpl().defer();
    },

    all: function(array) {
        return getImpl().all(array || []);
    },

    allSettled: function(array) {
        return getImpl().allSettled(array || []);
    },

    enableLongStacks: function() {
        _global.raptorPromisesLongStacks = true; // We make this a global so that it applies to all Q modules
        getImpl().longStackSupport = true;
    },

    resolved: function() {
        var provider = getImpl();
        return provider.apply(provider, arguments);
    },

    when: function(value) {
        return getImpl().when(value);
    },

    makePromise: function() {
        var provider = getImpl();
        return provider.apply(provider, arguments);
    },

    toString: function () {
        return '[raptor-promises ' + __filename + ']';
    }
});

if (_global.raptorPromisesLongStacks) {
    exports.enableLongStacks();
}
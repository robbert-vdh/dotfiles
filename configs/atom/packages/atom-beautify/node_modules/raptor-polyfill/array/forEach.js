// ES5 15.4.4.18
// http://es5.github.com/#x15.4.4.18
// https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/array/forEach

if (!Array.prototype.forEach) {
    var toObject = require('./_toObject');

    Array.prototype.forEach = function forEach(func, thisObj) {
        var self = toObject(this);
        var i = -1;
        var length = self.length >>> 0;

        // If no callback function or if callback is not a callable function
        if (typeof func !== 'function') {
            throw new TypeError();
        }

        while (++i < length) {
            if (i in self) {
                // Invoke the callback function with call, passing arguments:
                // context, property value, property key, thisArg object context
                func.call(thisObj, self[i], i, self);
            }
        }
    };
}
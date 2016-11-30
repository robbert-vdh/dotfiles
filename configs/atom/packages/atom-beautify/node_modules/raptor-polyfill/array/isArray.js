//
// Array
// =====
//

// ES5 15.4.3.2
// http://es5.github.com/#x15.4.3.2
// https://developer.mozilla.org/en/JavaScript/Reference/Global_Objects/Array/isArray
if (!Array.isArray) {
    var _toString = Object.prototype.toString;

    Array.isArray = function isArray(obj) {
        return _toString.call(obj) == "[object Array]";
    };
}
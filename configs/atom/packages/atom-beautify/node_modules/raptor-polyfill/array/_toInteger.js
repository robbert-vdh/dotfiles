// ES5 9.4
// http://es5.github.com/#x9.4
// http://jsperf.com/to-integer
module.exports = function(n) {
    n = +n;
    if (n !== n) { // isNaN
        n = 0;
    } else if (n !== 0 && n !== (1/0) && n !== -(1/0)) {
        n = (n > 0 || -1) * Math.floor(Math.abs(n));
    }
    return n;
};
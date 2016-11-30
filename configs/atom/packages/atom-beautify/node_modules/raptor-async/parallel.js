module.exports = function parallel(work, callback, thisObj) {

    var results;
    var len;
    var pending;
    var i;

    thisObj = thisObj || this;

    var isDone = false;

    function done(err, results) {
        if (isDone) {
            return;
        }
        isDone = true;
        callback.call(thisObj, err, results);
    }

    var createCallback = function(key) {
        var invoked = false;
        return function(err, data) {

            if (invoked === true) {
                throw new Error('callback for async operation with key "' + key + '" invoked after completion. ' + (err ? (err.stack || err) : '(no error)'));
            }

            invoked = true;

            results[key] = data;

            if (err) {
                done(err);
                return;
            }

            pending--;

            if (pending === 0) {
                done(null, results);
            }
        };
    };

    if (Array.isArray(work)) {

        len = pending = work.length;

        // results will be an array
        results = new Array(len);

        if (pending === 0) {
            return callback.call(thisObj, null, results);
        }

        for (i = 0; i < len; i++) {
            work[i].call(thisObj, createCallback(i));
        }
    } else {

        var keys = Object.keys(work);
        len = pending = keys.length;

        // results will be an object
        results = {};

        if (pending === 0) {
            return callback.call(thisObj, null, results);
        }

        for (i = 0; i < len; i++) {
            var key = keys[i];
            work[key].call(thisObj, createCallback(key));
        }
    }
};

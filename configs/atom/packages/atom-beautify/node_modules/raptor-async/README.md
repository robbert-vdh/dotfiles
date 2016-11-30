raptor-async
============

**raptor-async** is a simple library for orchestrating asynchronous operations. It was inspired by the popular <a href="https://github.com/caolan/async">caolan/async</a> library. The main difference being that this library only includes **parallel** and **series** functions. This library also supports an optional **thisObj** argument that determines the scope that will be used when invoking job and completion functions.

# Overview

The **raptor-async** library handles invoking functions (a.k.a. jobs) in **parallel** or **series**. Each function is invoked with a single argument &mdash; a callback function that should be invoked when the job completes. The callback function is a Node-style callback so it expects the first parameter to be an error object and the second parameter to be the data.

## Parallel

The **parallel** method is used to handle invoking worker jobs in parallel and notifying the provided callback when all of the jobs complete.

The **parallel** function supports the following calling conventions:
```javascript
async.parallel(function[], function, thisObj)
```
or
```javascript
async.parallel(object, function, thisObj)
```

If **parallel** is invoked with an array of functions as first argument, then results will provided to the completion callback as an array with the value at each index corresponding to the data returned by the job at that index. The order of the results will not be arbitrary &mdash; it will always correspond to the order in which the jobs were provided to the **parallel** function.

If **parallel** is invoked with an object as first argument, then each property is expected to be a function. The results will provided to the completion callback as an object with the value of each property corresponding to the data returned by the job with the corresponding key.

The **thisObj** argument is optional, but if it is provided then each job function will be invoked in the scope of the given value. Also, the final completion callback will be invoked in the scope of the given value.

Example of calling **parallel** with an array of functions as first argument:
```javascript
var async = require('raptor-async');

var work = [];

work[0] = function(callback) {
    setTimeout(function() {
        callback(null, 0);
    }, 1000);
};

work[1] = function(callback) {
    setTimeout(function() {
        callback(null, 1);
    }, 500);
};

work[2] = function(callback) {
    setTimeout(function() {
        callback(null, 2);
    }, 0);
};

async.parallel(work, function(err, results) {
    // results will be [0, 1, 2]
});
```

Example of calling **parallel** with object as first argument:
```javascript
var async = require('raptor-async');

var work = {};

work.a = function(callback) {
    setTimeout(function() {
        callback(null, 0);
    }, 1000);
};

work.b = function(callback) {
    setTimeout(function() {
        callback(null, 1);
    }, 500);
};

work.c = function(callback) {
    setTimeout(function() {
        callback(null, 2);
    }, 0);
};

async.parallel(work, function(err, results) {
    // results will be {a: 0, b: 1, c: 2}
});
```

## Series

The **series** method is used to handle invoking worker jobs in series and notifying the provided callback when all of the jobs complete. Execution of jobs will stop if a job provides an error to the callback. The results will always be an array whose order will correspond to the order in which the jobs were placed in array.

The **series** function supports the following calling convention:
```javascript
async.series(function[], function, thisObj)
```

The **thisObj** argument is optional, but if it is provided then each job function will be invoked in the scope of the given value. Also, the final completion callback will be invoked in the scope of the given value.

Example of calling **series**:
```javascript
var async = require('raptor-async');

var work = [];

work[0] = function(callback) {
    setTimeout(function() {
        callback(null, 0);
    }, 1000);
};

work[1] = function(callback) {
    setTimeout(function() {
        callback(null, 1);
    }, 500);
};

work[2] = function(callback) {
    setTimeout(function() {
        callback(null, 2);
    }, 0);
};

async.series(work, function(err, results) {
    // results will be [0, 1, 2]
});
```

## Error handling

For both **parallel** and **series** methods, if errors occur during execution of jobs then completion callback will be invoked with an error object as first argument.

The error object will have a **toMap** function that can be used to inspect which jobs returned errors. Each property in this map will have a key that corresponds to index (if input work was provided as array of functions) or key (if input work was provided as object) of original input job. The **toString** function will also provide a human-readable description of the error by invoking **toString** on each error and concatenating the results together in a meaningful way.

For example:
```javascript
var async = require('raptor-async');

async.series(work, function(err, results) {
    if (err) {
        // toString can be used
        console.error(err.toString());

        // you can also examine the errors yourself and output a message
        var mappedErrors = err.toMap();
        for (var key in mappedErrors) {
            console.error('Job "' + key + '" failed with error "' + mappedErrors[key] + '"';
        }
    }
});
```

Thrown exceptions will not be caught by **parallel** and **series** during invocations of jobs. It is responsibility of each job to provide their own try catch blocks if this is necessary.

## AsyncValue

Sometimes you need to keep track of an asynchronous operation to know if it is still pending, successfully completed or if it completed with an error. Promises allow for this, but Promises introduce a fair amount of overhead. The `AsyncValue` class offered by this module can be used as a lightweight alternative to promises with a much more limited feature set. `AsyncValue` instances do not support chaining, but they do support attaching Node.js-style callbacks. The usage of the `AsyncValue` class is best described using code as shown below:

```javascript
var AsyncValue = require('raptor-async/AsyncValue');

var configAsyncValue = new AsyncValue();

function loadConfig() {
    require('fs').readFile('config.json', 'utf8', function(err, json) {
        if (err) {
            // Something with wrong, I guess we won't be able to get a valid config...
            return configAsyncValue.reject(err);
        }

        var config = JSON.parse(json);

        // Success! We completed the asynchronous operation of loading the config
        // and now we can store the result in the async data holder instance.
        configAsyncValue.resolve(config);
    });
}

// Start loading the config immediately
loadConfig();

exports.onConfigLoaded = function(callback) {
    // Attach a listener to the data holder
    configAsyncValue.done(callback);
}
```

The constructor for the `AsyncValue` supports an optional `options` argument (described later).

The most important methods provided by `AsyncValue` instances are the following:

- `resolve(data)` - Move the data holder ot the "resolved" state and store the resulting data in the data holder
- `reject(err)` - Move the data holder ot the "rejected" state and store the resulting error in the data holder
- `done(callback)` - Attach a Node.js-style callback to the data holder (i.e. `function(err, data)`). If the data holder has already been resolved then the provided callback will be invoked with the stored data as the second argument. If the data holder has already been rejected then the provided callback will be invoked with the stored error as the first argument. If the data holder has not been resolved or rejected then a listener will be attached and the listener will later be invoked when the data holder is later resolved or rejected.

The complete set of `AsyncValue` properties are shown below:

- `data` - The resolved data or `undefined` if the data holder has not been resolved
- `error` - The rejected error or `undefined` if the data holder has not been rejected
- `isResolved() : Boolean` - Has resolved been called?
- `isRejected() : Boolean` - Has reject been called?
- `isLoading() : Boolean` - Is there an outstanding request to load data via loader?
- `isSettled() : Boolean` - Has reject or resolve been called?
- `load(callback, scope)`
- `done(callback, scope)`
- `reject(err)`
- `resolve(data)`
- `reset(data)`
- `unsettle(data)`

The signature for a `AsyncValue` is `function AsyncValue(options)` where options is an object with any of the following properties (all optional):

- `loader` - A function that can be used to load the asynchronous data. The provided loader function will be invoked with a callback argument when `load()` is called or lazily when a `done` listener is added for the first time.
- 'ttl' - A time-to-live in milliseconds. The data holder will go back into the initial unsettled state if the time-to-live is exceeded
- `scope` - The default value of `this` when invoking any of the provided callbacks or the loader function

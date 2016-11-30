// NOTE: Be careful if these numeric values are changed
//       because some of the logic is based on an assumed
//       sequencial order.
var STATE_INITIAL = 0;
var STATE_LOADING = 1;
var STATE_RESOLVED = 2;
var STATE_REJECTED = 3;

var now = Date.now || function() {
    return (new Date()).getTime();
};

function AsyncValue(options) {

    /**
     * The data that was provided via call to resolve(data).
     * This property is assumed to be public and available for inspection.
     */
    this.data = undefined;

    /**
     * The data that was provided via call to reject(err)
     * This property is assumed to be public and available for inspection.
     */
    this.error = undefined;

    /**
     * The queue of callbacks that are waiting for data
     */
    this._callbacks = undefined;

    /**
     * The state of the data holder (STATE_INITIAL, STATE_RESOLVED, or STATE_REJECTED)
     */
    this._state = STATE_INITIAL;

    /**
     * The point in time when this data provider was settled.
     */
    this._timestamp = undefined;

    if (options) {
        /**
         * An optional function that will be invoked to load the data
         * the first time data is requested.
         */
        this._loader = options.loader;

        /**
         * The "this" object that will be used when invoking callbacks and loaders.
         * NOTE: Some callbacks may have provided their own scope and that will be used
         *       instead of this scope.
         */
        this._scope = options.scope;

        /**
         * Time-to-live (in milliseconds).
         * A data holder can automatically invalidate it's held data or error after a preset period
         * of time. This should be used in combination of a loader. This is helpful in cases
         * where a data holder is used for caching purposes.
         */
        this._ttl = options.ttl || undefined;
    }
}

function notifyCallbacks(dataHolder, err, data) {
    var callbacks = dataHolder._callbacks;
    if (callbacks !== undefined) {
        // clear out the registered callbacks (we still have reference to the original value)
        dataHolder._callbacks = undefined;

        // invoke all of the callbacks and use their scope
        for (var i = 0; i < callbacks.length; i++) {
            // each callback is actually an object with "scope and "callback" properties
            var callbackInfo = callbacks[i];
            callbackInfo.callback.call(callbackInfo.scope, err, data);
        }
    }
}

function invokeLoader(dataProvider) {
    // transition to the loading state
    dataProvider._state = STATE_LOADING;

    // call the loader
    dataProvider._loader.call(dataProvider._scope || dataProvider, function (err, data) {
        if (err) {
            // reject with error
            dataProvider.reject(err);
        } else {
            // resolve with data
            dataProvider.resolve(data);
        }
    });
}

function addCallback(dataProvider, callback, scope) {
    if (dataProvider._callbacks === undefined) {
        dataProvider._callbacks = [];
    }

    dataProvider._callbacks.push({
        callback: callback,
        scope: scope || dataProvider._scope || dataProvider
    });
}

function isExpired(dataProvider) {
    var timeToLive = dataProvider._ttl;
    if ((timeToLive !== undefined) && ((now() - dataProvider._timestamp) > timeToLive)) {
        // unsettle the data holder if we find that it is expired
        dataProvider.unsettle();
        return true;
    } else {
        return false;
    }
}

AsyncValue.prototype = {

    /**
     * Has resolved function been called?
     */
    isResolved: function() {

        return (this._state === STATE_RESOLVED) && !isExpired(this);
    },

    /**
     * Has reject function been called?
     */
    isRejected: function() {
        return (this._state === STATE_REJECTED) && !isExpired(this);
    },

    /**
     * Is there an outstanding request to load data via loader?
     */
    isLoading: function() {
        return (this._state === STATE_LOADING);
    },

    /**
     * Has reject or resolve been called?
     *
     * This method will also do time-to-live checks if applicable.
     * If this data holder was settled prior to calling this method
     * but the time-to-live has been exceeded then the state will
     * returned to unsettled state and this method will return false.
     */
    isSettled: function() {
        // are we in STATE_RESOLVED or STATE_REJECTED?
        return (this._state > STATE_LOADING) && !isExpired(this);
    },

    /**
     * Trigger loading data if we have a loader and we are not already loading.
     * Even if a data holder is in a resolved or rejected state, load can be called
     * to get a new value.
     *
     * @return the resolved data (if loader synchronously calls resolve)
     */
    load: function(callback, scope) {
        if (!this._loader) {
            throw new Error('Cannot call load when loader is not configured');
        }

        if (this.isSettled()) {
            // clear out the old data and error
            this.unsettle();
        }

        // callback is optional for load call
        if (callback) {
            addCallback(this, callback, scope);
        }

        if (this._state !== STATE_LOADING) {
            // trigger the loading
            invokeLoader(this);
        }

        return this.data;
    },

    /**
     * Adds a callback to the queue. If there is not a pending request to load data
     * and we have a "loader" then we will use that loader to request the data.
     * The given callback will be invoked when there is an error or resolved data
     * available.
     */
    done: function (callback, scope) {
        if (!callback || (callback.constructor !== Function)) {
            throw new Error('Invalid callback: ' + callback);
        }

        // Do we already have data or error?
        if (this.isSettled()) {
            // invoke the callback immediately
            return callback.call(scope || this._scope || this, this.error, this.data);
        }

        if (process.domain) {
            callback = process.domain.bind(callback);
        }

        addCallback(this, callback, scope);

        // only invoke loader if we have loader and we are not currently loading value
        if (this._loader && (this._state !== STATE_LOADING)) {
            invokeLoader(this);
        }
    },

    /**
     * This method will trigger any callbacks to be notified of rejection (error).
     * If this data holder has a loader then the data holder will be returned to
     * its initial state so that any future requests to load data will trigger a
     * new load call.
     */
    reject: function(err) {
        // remember the error
        this.error = err;

        // clear out the data
        this.data = undefined;

        // record timestamp of when we were settled
        if (this._ttl !== undefined) {
            this._timestamp = now();
        }

        // Go to the rejected state if we don't have a loader.
        // If we do have a loader then return to the initial state
        // (we do this so that next call to done() will trigger load
        // again in case the error was transient).
        this._state = this._loader ? STATE_INITIAL : STATE_REJECTED;

        // always notify callbacks regardless of whether or not we return to the initial state
        notifyCallbacks(this, err, null);
    },

    /**
     * This method will trigger any callbacks to be notified of data.
     */
    resolve: function (data) {
        // clear out the error
        this.error = undefined;

        // remember the state
        this.data = data;

        // record timestamp of when we were settled
        if (this._ttl !== undefined) {
            this._timestamp = now();
        }

        // go to the resolved state
        this._state = STATE_RESOLVED;

        // notify callbacks
        notifyCallbacks(this, null, data);
    },

    /**
     * Clear out data or error and return this data holder to initial state.
     * If the are any pending callbacks then those will be removed and not invoked.
     */
    reset: function () {
        // return to the initial state and clear error and data
        this.unsettle();

        // remove any callbacks
        this.callbacks = undefined;
    },

    /**
     * Return to the initial state and clear stored error or data.
     * If there are any callbacks still waiting for data, then those
     * will be retained.
     */
    unsettle: function () {
        // return to initial state
        this._state = STATE_INITIAL;

        // reset error value
        this.error = undefined;

        // reset data value
        this.data = undefined;

        // clear the timestamp of when we were settled
        this._timestamp = undefined;
    }
};

AsyncValue.create = function(config) {
    return new AsyncValue(config);
};

module.exports = AsyncValue;

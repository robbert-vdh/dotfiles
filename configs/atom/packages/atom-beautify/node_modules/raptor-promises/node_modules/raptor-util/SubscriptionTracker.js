var INDEX_EVENT = 0;
var INDEX_LISTENER = 1;

function EventEmitterWrapper(target) {
    this._target = target;
    this._listeners = [];
}

EventEmitterWrapper.prototype = {
    _proxy: function(type, event, listener) {
        this._target[type](event, listener);
        this._listeners.push([event, listener]);
        return this;
    },

    _remove: function(test) {
        var target = this._target;
        var listeners = this._listeners;

        this._listeners = listeners.filter(function(listener) {
            var event = listener[INDEX_EVENT];
            var listenerFunc = listener[INDEX_LISTENER];

            if (test(event, listenerFunc)) {
                target.removeListener(event, listenerFunc);
                return false;

            } else {
                return true;
            }
        });
    },

    on: function(event, listener) {
        return this._proxy('on', event, listener);
    },

    once: function(event, listener) {
        return this._proxy('once', event, listener);
    },

    removeListener: function(event, listener) {
        if (listener) {
            this._remove(function(curEvent, curListener) {
                return listener === curListener;
            });
        } else {
            return this.removeAllListeners(event);
        }
    },

    removeAllListeners: function(event) {

        var listeners = this._listeners;
        var target = this._target;

        if (event) {
            this._remove(function(curEvent, curListener) {
                return event === curEvent;
            });
        } else {
            for (var i = listeners.length - 1; i >= 0; i--) {
                var cur = listeners[i];
                target.removeListener(cur[INDEX_EVENT], cur[INDEX_LISTENER]);
            }
            this._listeners.length = 0;
        }

        return this;
    }
};

EventEmitterWrapper.prototype.addListener = EventEmitterWrapper.prototype.on;

function SubscriptionTracker(source) {
    this._subscribeToList = [];
}

function attachDestroy(target, subscribeToList) {

    target.once('destroy', function() {
        for (var i = subscribeToList.length - 1; i >= 0; i--) {
            if (subscribeToList[i]._target === target) {
                subscribeToList.splice(i, 1);
                break;
            }
        }
    });
}

SubscriptionTracker.prototype = {

    subscribeTo: function(target) {
        var subscribeTo;
        var subscribeToList = this._subscribeToList;

        for (var i=0, len=subscribeToList.length; i<len; i++) {
            var cur = subscribeToList[i];
            if (cur._target === target) {
                subscribeTo = cur;
                break;
            }
        }


        if (!subscribeTo) {
            subscribeTo = new EventEmitterWrapper(target);
            attachDestroy(subscribeTo, subscribeToList);
            subscribeToList.push(subscribeTo);
        }

        return subscribeTo;
    },

    unsubscribeAll: function() {
        var subscribeToList = this._subscribeToList;
        for (var i = subscribeToList.length - 1; i >= 0; i--) {
            subscribeToList[i].removeAllListeners();
        }
        subscribeToList.length = 0;
    },

    unsubscribeFrom: function(target, event) {
        var subscribeTo;
        var subscribeToList = this._subscribeToList;

        for (var i=0, len=subscribeToList.length; i<len; i++) {
            var cur = subscribeToList[i];
            if (cur._target === target) {
                subscribeTo = cur;
                break;
            }
        }

        if (!subscribeTo) {
            return;
        }

        subscribeTo.removeAllListeners(event);
    }
};

module.exports = SubscriptionTracker;
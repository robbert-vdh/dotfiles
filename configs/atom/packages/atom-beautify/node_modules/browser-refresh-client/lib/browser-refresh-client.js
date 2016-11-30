var EventEmitter = require('events').EventEmitter;

function isBrowserRefreshEnabled() {
    return process.env.BROWSER_REFRESH_URL != null;
}

var browserRefreshVersion = process.env.BROWSER_REFRESH_VERSION;

if (browserRefreshVersion) {
    var browserRefreshVersionParts = browserRefreshVersion.split(/[.]/);

    browserRefreshVersion = {
        major: browserRefreshVersionParts[0],
        minor: browserRefreshVersionParts[1],
        patch: browserRefreshVersionParts[2],
    };
}

var nextId = 0;

exports.isBrowserRefreshEnabled = isBrowserRefreshEnabled;

exports.enableSpecialReload = function(patterns, options) {
    if (isBrowserRefreshEnabled() && process.send) {

        var specialReloadEvents = new EventEmitter();
        var modifiedEvent = 'browser-refresh-client.fileModified' + (nextId++);

        process.send({
            type: 'browser-refresh.specialReload',
            patterns: patterns,
            modifiedEvent: modifiedEvent,
            options: options
        });

        process.on('message', function(m) {
            if (typeof m === 'object' && m.type === modifiedEvent) {
                var path = m.path;
                specialReloadEvents.emit('fileModified', path);
            }
        });

        return {
            onFileModified: function(callback) {
                specialReloadEvents.on('fileModified', callback);
            },
            remove: function() {
                process.send({
                    type: 'browser-refresh.removeSpecialReload',
                    modifiedEvent: modifiedEvent
                });
            }
        };
    } else {
        return {
            onFileModified: function() { /* no-op */ },
            remove: function() { /* no-op */ }
        };
    }
};


var queuedRefreshInfo = {
    images: false,
    styles: false,
    page: false
};

var refreshQueued = false;

function triggerRefresh(type) {
    if (!isBrowserRefreshEnabled() || !process.send) {
        return;
    }

    queuedRefreshInfo[type] = true;

    if (refreshQueued) {
        return;
    }

    refreshQueued = true;

    process.nextTick(function() {
        refreshQueued = false;

        if (queuedRefreshInfo.page) {
            process.send({
                type: 'browser-refresh.refreshPage'
            });
        } else {
            if (queuedRefreshInfo.styles) {
                process.send({
                    type: 'browser-refresh.refreshStyles'
                });
            }

            if (queuedRefreshInfo.images) {
                process.send({
                    type: 'browser-refresh.refreshImages'
                });
            }
        }

        queuedRefreshInfo = {
            images: false,
            styles: false,
            page: false
        };
    });
}

var listenersAttached = false;
var events = new EventEmitter();

function attachListeners() {
    if (!isBrowserRefreshEnabled()) {
        return;
    }

    if (listenersAttached) {
        return;
    }

    listenersAttached = true;

    process.on('message', function(m) {
        if (typeof m === 'object' && m.type === 'browser-refresh.fileModified') {
            var path = m.path;
            events.emit('fileModified', path);
        }
    });

}
exports.onFileModified = function(callback) {
    if (!isBrowserRefreshEnabled()) {
        return;
    }

    attachListeners();

    events.on('fileModified', callback);
};

exports.refreshImages = function() {
    triggerRefresh('images');
};

exports.refreshStyles = function() {
    triggerRefresh('styles');
};

exports.refreshPage = function() {
    triggerRefresh('page');
};
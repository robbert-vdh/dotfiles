var fs = require('fs');
var AsyncValue = require('raptor-async/AsyncValue');
var nodePath = require('path');

var FS_READ_OPTIONS = { encoding: 'utf8' };

var cache = {};
var packageCache = {};

function Stats(stat) {
    if (stat) {
        this._exists = true;
        this._lastModified = stat.mtime ? stat.mtime.getTime() : -1;
        this._directory = stat.isDirectory();
    } else {
        this._exists = false;
        this._lastModified = undefined;
        this._directory = undefined;
    }
}

Stats.prototype = {
    isDirectory: function() {
        return (this._directory === true);
    },

    exists: function() {
        return (this._exists === true);
    },

    lastModified: function() {
        return this._lastModified;
    }
};

function stat(filePath, callback) {
    var dataHolder = cache[filePath];
    if (dataHolder === undefined) {
        cache[filePath] = dataHolder = new AsyncValue();
        fs.stat(filePath, function(err, stat) {
            dataHolder.resolve(new Stats(stat));
        });
    }

    dataHolder.done(callback);
}

function statSync(filePath, callback) {
    var dataHolder = cache[filePath];
    var stat;

    if ((dataHolder === undefined) || !dataHolder.isSettled()) {
        if (dataHolder === undefined) {
            cache[filePath] = dataHolder = new AsyncValue();
        }

        try {
            stat = new Stats(fs.statSync(filePath));
        } catch(err) {
            stat = new Stats(null);
        }

        dataHolder.resolve(stat);
    } else {
        stat = dataHolder.data;
    }

    return stat;
}

function readPackageSync(path) {
    var pkg = packageCache[path];

    if (pkg !== undefined) {
        return pkg;
    }

    var pkgJSON;

    try {
        pkgJSON = fs.readFileSync(path, FS_READ_OPTIONS);
    } catch(e) {
    }

    if (pkgJSON) {
        pkg = JSON.parse(pkgJSON);
        pkg.__filename = path;
        pkg.__dirname = nodePath.dirname(path);
    } else {
        pkg = null;
    }

    packageCache[path] = pkg;

    return pkg;
}

exports.stat = stat;
exports.statSync = statSync;

exports.lastModified = function(filePath, callback) {
    stat(filePath, function(err, stat) {
        callback(null, stat.lastModified());
    });
};

exports.exists = function(filePath, callback) {
    stat(filePath, function(err, stat) {
        callback(null, stat.exists());
    });
};

exports.existsSync = function(filePath) {
    return statSync(filePath).exists();
};

exports.isDirectorySync = function(filePath) {
    return statSync(filePath).isDirectory();
};

exports.clearCaches = function() {
    cache = {};
    packageCache = {};
};

exports.readPackageSync = readPackageSync;
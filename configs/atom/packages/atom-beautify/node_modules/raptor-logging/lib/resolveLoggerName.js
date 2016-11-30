var nodePath = require('path');
var fs = require('fs');

function removeExt(path) {
    var lastDot = path.lastIndexOf('.');
    if (lastDot !== -1) {
        return path.substring(0, lastDot);
    }
    else {
        return path;
    }
}

var existsCache = {};

function existsCached(path) {
    var exists = existsCached[path];
    if (exists !== undefined) {
        return exists;
    }

    exists = fs.existsSync(path);
    existsCache[path] = exists;
    return exists;
}

function readPackage(path) {
    if (existsCached(path)) {
        var pkg = require(path);
        pkg.__filename = path;
        pkg.__dirname = nodePath.dirname(path);
        return pkg;
    }
}

function findRootPackage(dirname) {
    if (dirname === '' || dirname === '/') {
        return null;
    }

    var packagePath = nodePath.join(dirname, 'package.json');
    var pkg = readPackage(packagePath);
    if (pkg && pkg.name) {
        return pkg;
    }

    var parentDirname = nodePath.dirname(dirname);
    if (parentDirname !== dirname) {
        return findRootPackage(parentDirname);
    }
    else {
        return null;
    }
}

function loggerName(path) {
    path = removeExt(path);
    var rootPkg = findRootPackage(nodePath.dirname(path));
    var moduleName = rootPkg.name || nodePath.basename(rootPkg.__dirname);
    return moduleName + path.substring(rootPkg.__dirname.length);
}

module.exports = loggerName;
var nodePath = require('path');
var lassoCachingFS = require('lasso-caching-fs');

var rootPackagesCache = {};

function getRootPackage(dirname) {
    var rootPkg = rootPackagesCache[dirname];
    if (rootPkg) {
        return rootPkg;
    }

    var currentDir = dirname;
    while (true) {
        var packagePath = nodePath.join(currentDir, 'package.json');
        var pkg = lassoCachingFS.readPackageSync(packagePath);
        if (pkg && pkg.name) {
            rootPkg = pkg;
            break;
        }

        var parentDir = nodePath.dirname(currentDir);
        if (!parentDir || parentDir === currentDir) {
            break;
        }
        currentDir = parentDir;
    }

    rootPackagesCache[dirname] = rootPkg || null;

    return rootPkg;
}


function getRootDir(path) {
    var rootPkg = getRootPackage(path);
    return rootPkg && rootPkg.__dirname;
}

exports.getRootPackage = getRootPackage;
exports.getRootDir = getRootDir;
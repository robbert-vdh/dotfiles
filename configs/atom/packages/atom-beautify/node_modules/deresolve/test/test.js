'use strict';
require('../'); // Load the module
var nodePath = require('path');
var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;

var deresolve = require('../');

var fixturesDir = nodePath.join(__dirname, 'fixtures');
require('app-module-path').addPath(nodePath.join(fixturesDir, 'test-project/src'));

describe('lasso-deresolve' , function() {

    it('should deresolve correctly for a main module on the search path', function() {
        var path = nodePath.join(fixturesDir, 'test-project/src/hello-world/index.js');
        var from = nodePath.join(fixturesDir, 'test-project/src');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./hello-world');
    });

    it('should deresolve correctly for a main module on the search path', function() {
        var path = nodePath.join(fixturesDir, 'test-project/node_modules/foo/lib/index.js');
        var from = nodePath.join(fixturesDir, 'test-project/src');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('foo');
    });

    it('should deresolve correctly for a non-main module on the search path', function() {
        var path = nodePath.join(fixturesDir, 'test-project/src/hello-world/hello.js');
        var from = nodePath.join(fixturesDir, 'test-project/src');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./hello-world/hello');
    });

    it('should deresolve correctly for sibling file in an installed module', function() {
        var path = nodePath.join(fixturesDir, 'test-project/node_modules/foo/lib/foo.js');
        var from = nodePath.join(fixturesDir, 'test-project/node_modules/foo/lib');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./foo');
    });

    it('should deresolve correctly for sibling file in a non-installed module', function() {
        var path = nodePath.join(fixturesDir, 'test-project/src/hello-world/hello.js');
        var from = nodePath.join(fixturesDir, 'test-project/src/hello-world');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./hello');
    });

    it('should deresolve correctly for sibling main file in a non-installed module', function() {
        var path = nodePath.join(fixturesDir, 'test-project/src/hello-world/index.js');
        var from = nodePath.join(fixturesDir, 'test-project/src/hello-world');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./');
    });

    it('should deresolve correctly for one installed module to another', function() {
        var path = nodePath.join(fixturesDir, 'test-project/node_modules/bar/lib/index.js');
        var from = nodePath.join(fixturesDir, 'test-project/node_modules/foo/lib');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('bar');
    });

    it('should resolve correctly when there is a nested node_modules within the search path', function() {
        var Module = require('module').Module;
        var projectPath = nodePath.join(fixturesDir, 'test-project');
        var old_nodeModulePaths = Module._nodeModulePaths;

        // HACK: Make sure projectPath is the first path entry
        Module._nodeModulePaths = function(from) {
            var paths = old_nodeModulePaths.call(this, from);
            paths.unshift(projectPath);
            return paths;
        };

        var path = nodePath.join(fixturesDir, 'test-project/node_modules/foo/lib/foo.js');
        var from = nodePath.join(fixturesDir, 'test-project/src/hello-world');

        expect(Module._nodeModulePaths(from)[0]).to.equal(projectPath);

        var deresolvedPath = deresolve(path, from);

        // UNHACK
        Module._nodeModulePaths = old_nodeModulePaths;

        expect(deresolvedPath).to.equal('foo/lib/foo');
    });

    it('should resolve correctly when there is a nested node_modules within the search path', function() {
        var Module = require('module').Module;
        var projectPath = nodePath.join(fixturesDir, 'test-project');
        var old_nodeModulePaths = Module._nodeModulePaths;

        // HACK: Make sure projectPath is the first path entry
        Module._nodeModulePaths = function(from) {
            var paths = old_nodeModulePaths.call(this, from);
            paths.unshift(projectPath);
            return paths;
        };

        var path = nodePath.join(fixturesDir, 'test-project/node_modules/bar/node_modules/baz/lib/index.js');
        var from = nodePath.join(fixturesDir, 'test-project/src/hello-world');

        expect(Module._nodeModulePaths(from)[0]).to.equal(projectPath);

        var deresolvedPath = deresolve(path, from);

        // UNHACK
        Module._nodeModulePaths = old_nodeModulePaths;

        expect(deresolvedPath).to.equal('bar/node_modules/baz');
    });

    it('should resolve correctly for a module in a directory with no main', function() {
        var path = nodePath.join(fixturesDir, 'test-project/src/no-main/foo.js');
        var from = nodePath.join(fixturesDir, 'test-project/src/no-main');

        var deresolvedPath = deresolve(path, from);

        expect(deresolvedPath).to.equal('./foo');
    });
});


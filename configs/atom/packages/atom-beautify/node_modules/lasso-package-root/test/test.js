'use strict';
var nodePath = require('path');
var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;

var lassoPackageRoot = require('../');
var fixturesDir = nodePath.join(__dirname, 'fixtures');

describe('lasso-package-root' , function() {
    it('should resolve root dir from root', function() {
        var rootDir = lassoPackageRoot.getRootDir(nodePath.join(fixturesDir, 'test-project'));
        expect(rootDir).to.equal(nodePath.join(fixturesDir, 'test-project'));
    });

    it('should resolve root dir from nested dir', function() {
        var rootDir = lassoPackageRoot.getRootDir(nodePath.join(fixturesDir, 'test-project/nested'));
        expect(rootDir).to.equal(nodePath.join(fixturesDir, 'test-project'));
    });

    it('should resolve root package from nested dir', function() {
        var rootPkg = lassoPackageRoot.getRootPackage(nodePath.join(fixturesDir, 'test-project'));
        expect(rootPkg.name).to.equal('test-project');
    });

    it('should resolve root package from nested dir', function() {
        var rootPkg = lassoPackageRoot.getRootPackage(nodePath.join(fixturesDir, 'test-project/nested'));
        expect(rootPkg.name).to.equal('test-project');
    });
});


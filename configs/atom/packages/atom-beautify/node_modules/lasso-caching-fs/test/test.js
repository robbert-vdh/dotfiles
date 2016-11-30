'use strict';
var nodePath = require('path');
var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;
var fs = require('fs');
var lassoCachingFS = require('../');

try {
    fs.mkdirSync(nodePath.join(__dirname, 'temp/'));
} catch(e) {}

describe('lasso-caching-fs' , function() {
    it('should statSync() correctly for a directory', function() {
        var stats = lassoCachingFS.statSync(nodePath.join(__dirname, 'fixtures/'));
        expect(stats.isDirectory()).to.equal(true);
    });

    it('should cache a statSync() call for a missing file', function() {
        var targetFile = nodePath.join(__dirname, 'temp/missing.js');
        try {
            fs.unlinkSync(targetFile);
        } catch(e) {}

        var stats = lassoCachingFS.statSync(targetFile);
        expect(stats.exists()).to.equal(false);

        fs.writeFileSync(targetFile, '', { encoding: 'utf8' });

        stats = lassoCachingFS.statSync(targetFile);
        expect(stats.exists()).to.equal(false);

        lassoCachingFS.clearCaches();

        stats = lassoCachingFS.statSync(targetFile);
        expect(stats.exists()).to.equal(true);
    });
});


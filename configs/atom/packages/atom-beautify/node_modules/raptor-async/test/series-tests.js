'use strict';

require('chai').should();
require('chai').config.includeStack = true;
var expect = require('chai').expect;

var async = require('..');

describe('raptor-async series' , function() {

    it('should handle empty work', function(done) {

        var work = [];

        async.series(work, function(err, results) {
            expect(results).to.deep.equal([]);
            done();
        });

    });

    it('should return array results in correct order', function(done) {

        this.timeout(2000);

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
            expect(results).to.deep.equal([0, 1, 2]);
            done();
        });

    });

    it('should throw error if callback for job is invoked more than once', function(done) {

        var uncaughtException;

        var mochaExceptionHandler = process.listeners('uncaughtException').pop();
        process.removeListener('uncaughtException', mochaExceptionHandler);

        process.once('uncaughtException', function(err) {
            // errors for invoking callback more than once will be thrown as errors
            // since they fall into the unexpected exception category
            uncaughtException = err;
            process.on('uncaughtException', mochaExceptionHandler);
        });

        this.timeout(2000);

        var work = [];

        work[0] = function(callback) {
            setTimeout(function() {
                callback(null, 0);
            }, 500);
        };

        work[1] = function(callback) {
            setTimeout(function() {
                callback(null, 1);
                callback(null, 'oops!');
            }, 250);
        };

        work[2] = function(callback) {
            setTimeout(function() {
                callback(null, 2);
            }, 0);
        };

        async.series(work, function(err, results) {
            expect(results).to.deep.equal([0, 1, 2]);
            expect(uncaughtException.toString()).to.equal('Error: callback for async operation at index \"1\" invoked after completion. (no error)');
            done();
        });

    });

    it('should include original error if callback is invoked after completion', function(done) {

        var uncaughtException;

        var mochaExceptionHandler = process.listeners('uncaughtException').pop();
        process.removeListener('uncaughtException', mochaExceptionHandler);

        process.once('uncaughtException', function(err) {
            // errors for invoking callback more than once will be thrown as errors
            // since they fall into the unexpected exception category
            uncaughtException = err;
            process.on('uncaughtException', mochaExceptionHandler);
        });

        this.timeout(2000);

        var work = [];

        work[0] = function(callback) {
            setTimeout(function() {
                callback(null, 0);
            }, 500);
        };

        work[1] = function(callback) {
            setTimeout(function() {
                callback(null, 1);
                callback(new Error('Something bad happened'));
            }, 250);
        };

        work[2] = function(callback) {
            setTimeout(function() {
                callback(null, 2);
            }, 0);
        };

        async.series(work, function(err, results) {
            expect(results).to.deep.equal([0, 1, 2]);
            expect(uncaughtException.toString().indexOf('Something bad happened')).to.not.equal(-1);
            done();
        });

    });

    it('should handle errors for arrays as input', function(done) {

        this.timeout(2000);

        var work = [];

        work[0] = function(callback) {
            setTimeout(function() {
                callback(null, 0);
            }, 400);
        };

        work[1] = function(callback) {
            setTimeout(function() {
                callback(null, 1);
            }, 200);
        };

        work[2] = function(callback) {
            setTimeout(function() {
                callback('This throws an error');
            }, 0);
        };

        async.series(work, function(err, results) {
            expect(results == null).to.equal(true);
            expect(err != null).to.equal(true);
            done();
        });

    });

});


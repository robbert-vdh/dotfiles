'use strict';

require('chai').should();
require('chai').config.includeStack = true;
var expect = require('chai').expect;
var Domain = require('domain');

var AsyncValue = require('../AsyncValue');

function createGroupDone(limit, next) {

    return function done(err) {
        if (err) {
            return next(err);
        }
        if (--limit <= 0) {
            next();
        }
    };
}

describe('raptor-async/AsyncValue domain' , function() {

    afterEach(function () {
        var current;
        while((current = process.domain)) {
            current.exit();
        }
    });

    it('should preserve empty domain', function(done) {

        var asyncValue = new AsyncValue();
        asyncValue.done(function shouldBeEmpty() {
            expect(!!process.domain).equal(false);
            done();
        });

        asyncValue.resolve('ok');
    });

    it('should preserve corresponding state of domain', function(done) {

        done = createGroupDone(2, done);

        var asyncValue = new AsyncValue();
        asyncValue.done(function shouldBeEmpty() {
            expect(!!process.domain).equal(false);
            done();
        });

        var domain = Domain.create();
        domain.run(function () {
            asyncValue.done(function shouldNoBeEmpty() {
                expect(!!process.domain).equal(true);
                done();
            });
        });

        asyncValue.resolve('ok');
    });

    it('should preserve corresponding state of domain, complex', function(done) {

        done = createGroupDone(3, done);

        var asyncValue = new AsyncValue();
        asyncValue.done(function shouldBeEmpty() {
            expect(!!process.domain).equal(false);
            done();
        });

        var domain1 = Domain.create();
        domain1.run(function () {
            asyncValue.done(function shouldNoBeEmpty() {
                expect(process.domain).to.equal(domain1);
                done();
            });
        });

        var domain2 = Domain.create();
        domain2.run(function () {
            asyncValue.done(function shouldNoBeEmpty() {
                expect(process.domain).to.equal(domain2);
                done();
            });
        });

        asyncValue.resolve('ok');
    });


});

'use strict';

require('chai').should();
require('chai').config.includeStack = true;
var expect = require('chai').expect;


describe('raptor-async/api' , function() {

    it('should export raptor-async/AsyncValue', function(done) {
        var AsyncValue = require('../AsyncValue');

        var asyncValue = new AsyncValue();
        asyncValue.done(function (err, value) {
            expect(value).to.equal('ok');
            done();
        });

        asyncValue.resolve('ok');
    });

    it('should export raptor-async/DataHolder for backwards compatibility', function(done) {
        var DataHolder = require('../DataHolder');

        var asyncValue = new DataHolder();
        asyncValue.done(function (err, value) {
            expect(value).to.equal('ok');
            done();
        });

        asyncValue.resolve('ok');
    });
});

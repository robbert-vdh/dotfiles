'use strict';
var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;


describe('raptor-json/stringify' , function() {

    beforeEach(function(done) {
        for (var k in require.cache) {
            if (require.cache.hasOwnProperty(k)) {
                delete require.cache[k];
            }
        }
        done();
    });

    it('should support a non-native parser', function() {
        var parse = require('../parse');
        var o = parse('{a: 100}');
        expect(o.a).to.equal(100);
        
        
     });

    it('should support escaping of backslash', function() {
        var stringify = require('../stringify');
        expect(stringify("\\")).to.equal('"\\\\"');
        expect(stringify("\\", {useSingleQuote: true})).to.equal("'\\\\'");
        //expect(stringify("TEST\\")).to.equal('"\\n{1}\\nTEST\\\\"');
        
        
     });
});


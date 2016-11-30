var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;

var raptorUtil = require('../'); // Load this module just to make sure it works

describe('raptor-util/index' , function() {

    beforeEach(function(done) {
        done();
    });

    it("should allow for toArray to be called with an arguments object", function() {

        var args = [];
        function foo() {
            args = raptorUtil.toArray(arguments);
        }

        foo('a', 'b');

        expect(args).to.be.an('array');
        expect(args.length).to.equal(2);
        expect(args.push).to.be.a('function');
    });

    it("should allow for toArray to be called with a String object", function() {
        var chars = raptorUtil.toArray('foo');
        expect(chars).to.be.an('array');
        expect(chars.length).to.equal(3);
        expect(chars).to.deep.equal(['f', 'o', 'o']);
        expect(chars.push).to.be.a('function');
    });

    it("should allow for toArray to be called with an Array object", function() {
        var inArray = ['foo'];
        var outArray = raptorUtil.toArray(inArray);
        expect(inArray).to.equal(outArray);
    });

    it("should allow for toArray to be called with a null object", function() {
        var outArray = raptorUtil.toArray(null);
        expect(outArray).to.equal(null);
    });

    it("should allow for toArray to be called with a number object", function() {
        var outArray = raptorUtil.toArray(1);
        expect(outArray).to.deep.equal([1]);
    });
});


var chai = require('chai');
chai.Assertion.includeStack = true;
var expect = require('chai').expect;

describe('property-handlers' , function() {

    it('should work if property handlers exist', function() {
        var propertyHandlers = require('../');
        var object = {
            foo: 'foo',
            bar: 'bar'
        };

        var foo;
        var bar;

        propertyHandlers(object, {
            foo: function(value) {
                foo = value;
            },

            bar: function(value) {
                bar = value;
            }
        }, 'config');

        expect(foo).to.equal('foo');
        expect(bar).to.equal('bar');
    });

    it('should report error for invalid property', function() {
        var propertyHandlers = require('../');
        var object = {
            foo: 'foo',
            bar: 'bar',
            baz: 'baz'
        };

        var foo;
        var bar;
        var err = null;

        try {
            propertyHandlers(object, {
                foo: function(value) {
                    foo = value;
                },

                bar: function(value) {
                    bar = value;
                }
            }, 'config');
        } catch(e) {
            err = e;
        }

        expect(err).to.not.equal(null);
    });

});


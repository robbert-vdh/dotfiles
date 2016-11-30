var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;

var raptorUtil = require('../'); // Load this module just to make sure it works

describe('raptor-util/classes' , function() {

    beforeEach(function(done) {
        done();
    });

    it("should allow for class inheritance", function() {

        var Bird = raptorUtil.makeClass({
            $init: function(species) {
                this.species = species;
            },
            
            getSpecies: function() {
                return this.species;
            },
            
            isFlighted: function() {
                return true;
            },
            
            toString: function() {
                return '[Bird: ' + this.getSpecies() + ']';
            }
        });

        function Ostrich() {
            Ostrich.$super.call(this, 'ostrich');
        }
        
        Ostrich.prototype = {
            isFlighted: function() {
                return false;
            },
            
            isOstrich: function() {
                return true;
            },
            
            toString: function() {
                return Ostrich.$super.prototype.toString.call(this);
            }
        };

        raptorUtil.inherit(Ostrich, Bird);

        var ostrich = new Ostrich();
        expect(ostrich.getSpecies()).to.equal('ostrich');
        expect(ostrich.isFlighted()).to.equal(false);
        expect(ostrich.isOstrich()).to.equal(true);
        expect(Ostrich.prototype.constructor).to.equal(Ostrich);
        expect(Ostrich.$super.prototype.constructor).to.equal(Bird);
        expect(Ostrich.$super).to.equal(Bird);
        expect(ostrich.toString()).to.equal('[Bird: ostrich]');
    });

    it("should allow for $super with makeClass", function() {

        var Bird = raptorUtil.makeClass({
            $init: function(species) {
                this.species = species;
            },
            
            getSpecies: function() {
                return this.species;
            },
            
            isFlighted: function() {
                return true;
            },
            
            toString: function() {
                return '[Bird: ' + this.getSpecies() + ']';
            }
        });

        var Ostrich = raptorUtil.makeClass({
            $init: function(species) {
                Ostrich.$super.call(this, 'ostrich');
            },

            $super: Bird,
            
            isFlighted: function() {
                return false;
            },
            
            isOstrich: function() {
                return true;
            },
            
            toString: function() {
                return Ostrich.$super.prototype.toString.call(this);
            }
        });

        var ostrich = new Ostrich();
        expect(ostrich.getSpecies()).to.equal('ostrich');
        expect(ostrich.isFlighted()).to.equal(false);
        expect(ostrich.isOstrich()).to.equal(true);
        expect(Ostrich.prototype.constructor).to.equal(Ostrich);
        expect(Ostrich.$super.prototype.constructor).to.equal(Bird);
        expect(Ostrich.$super).to.equal(Bird);
        expect(ostrich.toString()).to.equal('[Bird: ostrich]');
    });
});


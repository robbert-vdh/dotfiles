var chai = require('chai');
chai.Assertion.includeStack = true;
require('chai').should();
var expect = require('chai').expect;

var raptorUtil = require('../'); // Load this module just to make sure it works

var SimpleDay = raptorUtil.makeEnum(
    [
        "SUN",
        "MON",
        "TUE",
        "WED",
        "THU",
        "FRI",
        "SAT"
    ]);

var ComplexDay = raptorUtil.makeEnum(
    {
        SUN: [false, "Sunday"],
        MON: [true, "Monday"],
        TUE: [true, "Tuesday"],
        WED: [true, "Wednesday"],
        THU: [true, "Thursday"],
        FRI: [true, "Friday"],
        SAT: [false, "Saturday"]
    },
    {
        $init: function(isWeekday, longName) {
            this._isWeekday = isWeekday;
            this._longName = longName;
        },
         
        getLongName: function() {
            return this._longName;
        },
         
        isWeekday: function() {
            return this._isWeekday;
        }
    });
 
var ComplexDay2 = raptorUtil.makeEnum(
        {
            SUN: [false, "Sunday"],
            MON: [true, "Monday"],
            TUE: [true, "Tuesday"],
            WED: [true, "Wednesday"],
            THU: [true, "Thursday"],
            FRI: [true, "Friday"],
            SAT: [false, "Saturday"]
        },
        (function() {
            function ComplexDay2(isWeekday, longName) {
                this._isWeekday = isWeekday;
                this._longName = longName;
            }
            
            ComplexDay2.TEST_STATIC = true;
            
            ComplexDay2.prototype = {
                getLongName: function() {
                    return this._longName;
                },
                 
                isWeekday: function() {
                    return this._isWeekday;
                },
                
                toString: function() {
                    return this._longName;
                }
            };
            
            return ComplexDay2;
        }()));

describe('raptor-util/makeEnum' , function() {

    beforeEach(function(done) {
        done();
    });

    it("should allow for simple enums", function() {
        expect(SimpleDay.SUN).to.not.equal(null);
    });

    it("should allow for complex enums", function() {
        expect(ComplexDay.SUN.getLongName()).to.equal('Sunday');
        expect(ComplexDay2.SUN.getLongName()).to.equal('Sunday');
    });

    it('should allow instanceof to be used with enum vales', function() {
        expect(SimpleDay.SUN instanceof SimpleDay).to.equal(true);
        expect(ComplexDay.SUN instanceof ComplexDay).to.equal(true);
        expect(ComplexDay.SUN instanceof SimpleDay).to.equal(false);
        expect(ComplexDay2.SUN instanceof ComplexDay2).to.equal(true);
    });
    
    it('should allow enums to have static properties', function() {
        expect(ComplexDay2.TEST_STATIC).to.not.equal(null);
    });
    
    it('should support "valueOf" method for enum classes', function() {
        expect(SimpleDay.valueOf("SUN")).to.equal(SimpleDay.SUN);
    });

    it('should support "ordinal" method for enum values', function() {
        expect(SimpleDay.SUN.ordinal).to.equal(0);
        expect(SimpleDay.MON.ordinal).to.equal(1);
        expect(SimpleDay.TUE.ordinal).to.equal(2);
        
    });
    
    it('should support "name" method for enum values', function() {
        expect(SimpleDay.SUN.name).to.equal("SUN");
    });

    it('should support "toString" method for enum values', function() {
        expect(SimpleDay.SUN.toString()).to.equal("SUN");
        expect(ComplexDay2.SUN.toString()).to.equal("Sunday");
    });

    it('should support "compareTo" method for enum values', function() {
        expect(SimpleDay.SUN.compareTo(SimpleDay.MON) < 0).to.equal(true);
        expect(SimpleDay.MON.compareTo(SimpleDay.SUN) > 0).to.equal(true);
        expect(SimpleDay.MON.compareTo(SimpleDay.MON) === 0).to.equal(true);
        expect(SimpleDay.MON.compareTo(SimpleDay.THU) < 0).to.equal(true);
        expect(SimpleDay.THU.compareTo(SimpleDay.MON) > 0).to.equal(true);
    });

    
});


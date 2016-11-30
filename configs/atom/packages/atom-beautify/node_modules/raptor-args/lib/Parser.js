var optionRegExp = /^--?(.+)$/;
var argNameRegExp = /^(--?)(no-)?([^\s]+)$/;
var arrayTypeRegExp = /(.*)\[\]$/;

function DEFAULT_ON_ERROR(err) {
    throw err;
}

function leftPad(str, len) {
    while (str.length < len) {
        str = ' ' + str;
    }
    return str;
}

function removeDashes(str) {
    return str.replace(/-([a-z])/g, function(match, lower) {
        return lower.toUpperCase();
    });
}

function getTargetPropertyForOption(option, arg) {
    var targetProperty = option.targetProperty;

    if (arg && targetProperty === '*') {
        targetProperty = arg;

        if (option.removeDashes !== false) {
            targetProperty = removeDashes(targetProperty);
        }
    }

    return targetProperty;
}

function Options(config) {
    this._lookup = {};
    this._options = [];

    if (config) {
        this.add(config);
    }
}

Options.prototype = {
    add: function(config) {
        for (var optionsString in config) {
            if (config.hasOwnProperty(optionsString)) {
                var optionConfig = config[optionsString];

                if (!optionConfig) {
                    optionConfig = {};
                } else if (typeof optionConfig === 'string') {
                    optionConfig = {
                        type: optionConfig
                    };
                }

                optionConfig.optionsString = optionsString;
                this._options.push(optionConfig);

                var aliases = optionsString.split(/\s+/);

                if (!optionConfig.targetProperty) {
                    var optionNameMatches = optionRegExp.exec(aliases[0]);

                    if (optionNameMatches) {
                        optionConfig.targetProperty = optionNameMatches[1];
                    } else {
                        optionConfig.targetProperty = aliases[0];
                    }

                    if (optionConfig.removeDashes !== false) {
                        optionConfig.targetProperty = removeDashes(optionConfig.targetProperty);
                    }
                }

                if (optionConfig.type) {
                    var arrayTypeMatches = arrayTypeRegExp.exec(optionConfig.type);
                    if (arrayTypeMatches) {
                        optionConfig.type = arrayTypeMatches[1] || null;
                        optionConfig.isArray = true;
                    }
                }

                if (optionConfig.options) {
                    optionConfig.options = new Options(optionConfig.options);
                }

                for (var i = 0, len = aliases.length; i < len; i++) {
                    this._lookup[aliases[i]] = optionConfig;
                }
            }
        }
    },

    getAllowedOptions: function() {
        return Object.keys(this._lookup);
    },

    getProperties: function() {
        return this._options.map(function(optionConfig) {
            return optionConfig.targetProperty;
        });
    },

    get: function(optionName) {
        return this._lookup[optionName];
    }
};


function Parser() {
    this._options = new Options();
    this._usage = null;
    this._examples = [];
    this._validators = [];
    this._onError = null;
}

Parser.prototype = {
    options: function(config) {
        this._options.add(config);
        return this;
    },

    forEachOption: function(fn) {
        this._options._options.forEach(fn);
    },

    getProperties: function() {
        return this._options.getProperties();
    },

    usage: function(usage) {
        this._usage = usage;
        return this;
    },

    example: function(label, command) {
        if (arguments.length === 1) {
            command = label;
            label = null;
        }

        this._examples.push({
            label: label,
            command: command
        });
        return this;
    },

    validate: function(validateFunc) {
        this._validators.push(validateFunc);
        return this;
    },

    onError: function(handlerFunc) {
        this._onError = handlerFunc;
        return this;
    },

    getUsageString: function() {
        var lines = [];
        var i;
        var $0 = require('path').basename(process.argv[1]);
        var usage = (this._usage || 'Usage: $0 [OPTIONS]').replace(/\$0/g, $0);
        lines.push(usage);
        lines.push('');

        if (this._examples.length) {
            lines.push('Examples:');
            lines.push('');
            for (i = 0; i < this._examples.length; i++) {
                var example = this._examples[i];
                var label = example.label;
                var command = example.command.replace(/\$0/g, $0);
                if (label) {
                    lines.push('  ' + label + ':');
                    lines.push('     ' + command);
                } else {
                    lines.push('   ' + command);
                }
                lines.push('');
            }
        }

        var optionsString;

        var optionConfigs = this._options._options;
        if (optionConfigs.length) {
            // Figure out the width for the left column
            var maxOptionLength = -1;
            for (i = 0; i < optionConfigs.length; i++) {
                optionsString = optionConfigs[i].optionsString;
                if (maxOptionLength === -1 || optionsString.length > maxOptionLength) {
                    maxOptionLength = optionsString.length;
                }
            }

            lines.push('Options:');
            lines.push('');
            for (i = 0; i < optionConfigs.length; i++) {
                var option = optionConfigs[i];
                optionsString = option.optionsString;

                var leftColumn = leftPad(optionsString, maxOptionLength);
                var indent = leftPad('', maxOptionLength + 1);
                var description = option.description || '';
                if (option.type) {
                    description += (description ? ' ' : '') + '[' + option.type + ']';
                }

                var rightColumn = description.replace(/\n/g, '\n' + indent);

                lines.push(leftColumn + ' ' + rightColumn);
                lines.push('');
            }
        }

        return lines.join('\n');
    },

    printUsage: function() {
        console.log(this.getUsageString());
    },

    parse: function(args) {
        if (args === undefined) {
            args = process.argv.slice(2);
        }

        var state = {
            option: null,
            targetProperty: null,
            result: {},
            options: this._options
        };

        var stack = [state];

        function finishLastOption() {
            if (state.option) {

                var targetProperty = state.targetProperty;

                // Finish off the previous option...
                if (state.result[targetProperty] == null) {
                    state.result[targetProperty] = true;
                }
            }

            state.option = null;
        }


        var onError = this._onError ? this._onError.bind(this) : DEFAULT_ON_ERROR;

        function addValue(arg) {
            var targetProperty = state.targetProperty;

            var option = state.option;
            if (!option) {
                option = state.option = state.options.get('*');
            }

            if (!option) {
                return onError('Illegal argument: "' + arg + '"');
            }

            if (!targetProperty) {
                targetProperty = state.targetProperty = getTargetPropertyForOption(option);
            }


            if (option.isArray || targetProperty === '*') {
                if (state.result[targetProperty] == null) {
                    state.result[targetProperty] = [];
                }
            }

            var existingValue = state.result[targetProperty];

            if (arg === 'true') {
                arg = true;
            } else if (arg === 'false') {
                arg = false;
            } else if (option.type === 'number') {
                arg = parseFloat(arg);
            } else if (option.type === 'int' || option.type === 'integer') {
                arg = parseInt(arg, 10);
            } else if (option.type === 'boolean') {
                arg = arg.toLowerCase();
                arg = arg === '1' || arg === 'true' || arg === 'yes' || arg === 'y';
            }

            if (existingValue == null) {
                state.result[targetProperty] = arg;
            } else {
                if (Array.isArray(existingValue)) {
                    existingValue.push(arg);
                } else {
                    state.result[targetProperty] = [existingValue, arg];
                }
            }
        }

        for (var i = 0, len = args.length; i < len; i++) {
            var arg = args[i];
            var argNameMatches;
            var option;

            if (arg === '[') {

                var nestedOptions = state.option.options;
                if (!nestedOptions) {
                    nestedOptions = new Options({
                        '*': null
                    });
                }

                state = {
                    option: null,
                    targetProperty: null,
                    result: {},
                    options: nestedOptions
                };

                stack.push(state);
            } else if (arg === ']') {
                finishLastOption();

                var complexResult = stack[stack.length - 1].result;

                // Restore parser state
                stack.pop();
                state = stack[stack.length - 1];

                addValue(complexResult);
            } else if( (argNameMatches = argNameRegExp.exec(arg)) ) {
                // We hit either a '--' or '-' arg
                finishLastOption();

                var prefix = argNameMatches[1];
                var no = argNameMatches[2];
                var argName = argNameMatches[3];

                if (no) {
                    if (state.options.get(arg)) {
                        no = false;
                    } else {
                        no = true;
                        arg = prefix + argName;
                    }
                }

                var splitArgs = null;


                if (prefix === '-') {
                    /* jshint loopfunc:true */
                    splitArgs = argName.split('').map(function(shortArg) {
                        return {
                            argName: shortArg,
                            arg: '-' + shortArg
                        };
                    });
                } else {
                    splitArgs = [{
                            argName: argName,
                            arg: arg
                        }];
                }

                for (var j = 0, len2 = splitArgs.length; j < len2; j++) {
                    arg = splitArgs[j].arg;
                    argName = splitArgs[j].argName;
                    option = state.options.get(arg) || state.options.get('--*') || state.options.get('-*');

                    if (!option) {
                        return onError('Illegal argument: "' + arg + '" (allowed: ' + state.options.getAllowedOptions().join(', ') + ')');
                    }

                    state.option = option;

                    state.targetProperty = getTargetPropertyForOption(option, argName);

                    if (splitArgs.length > 1) {
                        state.result[state.targetProperty] = true;
                    } else if (no) {
                        // Prefixed with 'no-'
                        state.result[state.targetProperty] = false;
                    }
                }
            } else {
                // This is an argument that is not prefixed with '--' or '-'
                addValue(arg);
            }
        }

        finishLastOption();

        // Run the validators
        try {
            for (i = 0, len = this._validators.length; i < len; i++) {
                this._validators[i].call(this, state.result, this);
            }
        } catch (e) {
            onError(e);
        }

        return state.result;



    }
};

module.exports = Parser;
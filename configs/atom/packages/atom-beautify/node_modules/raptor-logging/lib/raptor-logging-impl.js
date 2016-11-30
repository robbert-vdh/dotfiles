/*
 * Copyright 2011 eBay Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

require('raptor-polyfill/string/startsWith');

var Logger = require('./Logger');
var logLevels = ['TRACE', 'DEBUG', 'INFO', 'WARN', 'ERROR', 'FATAL', 'OFF'];
var LogLevel = {};
var _loggerConfigs;
var _rootLoggerConfig;
var _appenders = [];
var loggerConfigsByName = {};
var ConsoleAppender = require('./ConsoleAppender');
var env = typeof process !== 'undefined' ? process.env : null;
var resolveLoggerName = require('./resolveLoggerName');

function toLogLevel(logLevelName) {
    if (logLevelName && logLevelName.toUpperCase) {
        return LogLevel[logLevelName.toUpperCase()] || LogLevel.TRACE;
    } else {
        return LogLevel.TRACE;
    }
}

function createLogLevel(logLevel, index) {
    return {
        name: logLevel,
        level: index,
        methodName: logLevel.toLowerCase(),
        toString: function() {
            return logLevel;
        }
    };
}

function _configureLogger(loggerName, config) {

    var levelName;

    if (typeof config === 'string') {
        levelName = config;
    }
    else {
        levelName = config.levelName || config.level;
    }

    var loggerConfig = {
        levelName: levelName,
        loggerName: loggerName.replace(/\./g, '/'),
        logLevel: toLogLevel(levelName)
    };

    loggerConfigsByName[loggerName] = loggerConfig;

    if (loggerName === 'ROOT') {
        _rootLoggerConfig = loggerConfig;
    }
}

function loggerConfigComparator(a, b) {
    return b.loggerName.length - a.loggerName.length;
}

function _sortConfigs() {
    _loggerConfigs = [];

    for (var k in loggerConfigsByName) {
        if (loggerConfigsByName.hasOwnProperty(k)) {
            _loggerConfigs.push(loggerConfigsByName[k]);
        }
    }

    _loggerConfigs.sort(loggerConfigComparator);
}

for (var i = 0; i < logLevels.length; i++) {
    var logLevel = logLevels[i];
    LogLevel[logLevel] = createLogLevel(logLevel, i);
}

LogLevel.toLogLevel = function(level) {
    if (level.constructor === String) {
        // convert log level name to LogLevel
        return this[level];
    } else {
        // convert ordinal to LogLevel
        return this[logLevels[level]];
    }
};

Logger.setLogLevel(LogLevel);

var logging = {
    _modifiedFlag: 0,

    /**
     * enum type of all of the log levels
     */
    LogLevel: LogLevel,

    /**
     * @function
     * @private
     * @returns {Object} the configuration for the given logger
     */
    getLoggerConfig: function(loggerName) {
        var i = 0,
            len = _loggerConfigs.length,
            curLoggerConfig,
            curLoggerName;
        for (; i < len; i++) {
            curLoggerConfig = _loggerConfigs[i];
            curLoggerName = curLoggerConfig.loggerName;

            if (loggerName.startsWith(curLoggerName)) {
                return curLoggerConfig;
            }
        }

        return _rootLoggerConfig;
    },

    /**
     * @private
     * @returns {Array}
     */
    getLoggerConfigs: function() {
        return _loggerConfigs;
    },

    /**
     * @private
     * @returns {Array}
     */
    getAppenders: function() {
        return _appenders;
    },

    /**
     * Add an appender that will be used by all loggers
     */
    addAppender: function(appender) {
        _appenders.push(appender);
    },

    /**
     * Configures the appenders that will be used by the logging subsystem. The given appenders
     * will replace any existing appenders.
     */
    configureAppenders: function(appenders) {
        _appenders = appenders;
        logging._modifiedFlag++;
    },

    /**
     * @param loggerName name of the logger (e.g. "raptor/packaging")

     * @returns a new Logger for the given logger name
     */
    logger: function(loggerName) {

        if (typeof loggerName === 'object' && loggerName.filename) {
            loggerName = resolveLoggerName(loggerName.filename);
        }

        return new Logger(
            this.getLoggerConfig(loggerName),
            loggerName,
            _appenders);
    },

    configureLogger: function(loggerName, config) {
        _configureLogger(loggerName, config);
        _sortConfigs();
        logging._modifiedFlag++;
    },

    /**
     * Configures the logging subsystem with the given loggers (which will replace any existing loggers).
     *
     * Example logger configuration:
     * {
     *     'raptor': {
     *         level: 'INFO'
     *     },
     *     'raptor/packaging': {
     *         level: 'DEBUG'
     *     }
     * }
     *
     * @param loggers an object whose keys are the names of loggers and whose values are the configuration for that logger
     */
    configureLoggers: function(loggerConfigs) {
        var oldConfig = loggerConfigsByName;

        loggerConfigsByName = {};
        var rootFound = false;
        for (var k in loggerConfigs) {
            if (loggerConfigs.hasOwnProperty(k)) {
                if (k === 'ROOT') {
                    rootFound = true;
                }

                _configureLogger(k, loggerConfigs[k]);
            }
        }

        if (!rootFound) {
            _configureLogger('ROOT', 'WARN');
        }

        if (env) {
            var configureEnvLoggers = function (level) {
                var loggers = env[level] || env[level.toLowerCase()];

                if (loggers) {
                    loggers.split(/\s*[,]\s*/).forEach(function(logger) {
                        _configureLogger(logger, level);
                    });
                }
            };

            configureEnvLoggers('TRACE');
            configureEnvLoggers('DEBUG');
            configureEnvLoggers('INFO');
            configureEnvLoggers('WARN');
            configureEnvLoggers('ERROR');
            configureEnvLoggers('FATAL');
        }

        _sortConfigs();
        logging._modifiedFlag++;

        return oldConfig;
    },

    /**
     * This method will configure the logging subsystem. It typically should only be called once in application code.
     *
     * If the given config parameter contains a "loggers" property then the value of this property will be used
     * to replace the configuration for any existing loggers.
     *
     * If the given config parameter contains an "appenders" property then the value of this property will be used
     * to replace the configuration for any existing appenders.
     *
     * @param config the configuration object
     */
    configure: function(config) {
        if (!config) {
            return;
        }

        if (config.appenders) {
            // configure appenders
            this.configureAppenders(config.appenders);
        }

        if (config.loggers) {
            // configure loggers
            this.configureLoggers(config.loggers);
        }
    },

    ConsoleAppender: ConsoleAppender,

    toString: function () {
        return '[raptor-logging]';
    }
};

Logger.setLogging(logging);

logging.configure({
    loggers: {
        'ROOT': {
            level: 'WARN'
        }
    }
});

if (typeof console !== 'undefined') {
    logging.addAppender(new ConsoleAppender());
}

var loggingStubs = require('./raptor-logging');

for (var k in logging) {
    if (logging.hasOwnProperty(k)) {
        loggingStubs[k] = logging[k];
    }
}
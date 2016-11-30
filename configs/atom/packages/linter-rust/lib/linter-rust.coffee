fs = require 'fs'
path = require 'path'

{CompositeDisposable} = require 'atom'
atom_linter = require 'atom-linter'
semver = require 'semver'
XRegExp = require 'xregexp'

errorModes = require './mode'

class LinterRust
  patternRustcVersion: XRegExp('rustc (?<version>1.\\d+.\\d+)(?:(?:-(?:(?<nightly>nightly)|(?<beta>beta.*?))|(?:[^\s]+))? \
                                \\((?:[^\\s]+) (?<date>\\d{4}-\\d{2}-\\d{2})\\))?')
  cargoDependencyDir: "target/debug/deps"

  constructor: ->
    @subscriptions = new CompositeDisposable

    @subscriptions.add atom.config.observe 'linter-rust.rustcPath',
    (rustcPath) =>
      rustcPath = do rustcPath.trim if rustcPath
      @rustcPath = rustcPath

    @subscriptions.add atom.config.observe 'linter-rust.cargoPath',
    (cargoPath) =>
      @cargoPath = cargoPath

    @subscriptions.add atom.config.observe 'linter-rust.useCargo',
    (useCargo) =>
      @useCargo = useCargo

    @subscriptions.add atom.config.observe 'linter-rust.cargoCommand',
    (cargoCommand) =>
      @cargoCommand = cargoCommand

    @subscriptions.add atom.config.observe 'linter-rust.rustcBuildTest',
    (rustcBuildTest) =>
      @rustcBuildTest = rustcBuildTest

    @subscriptions.add atom.config.observe 'linter-rust.cargoManifestFilename',
    (cargoManifestFilename) =>
      @cargoManifestFilename = cargoManifestFilename

    @subscriptions.add atom.config.observe 'linter-rust.jobsNumber',
    (jobsNumber) =>
      @jobsNumber = jobsNumber

    @subscriptions.add atom.config.observe 'linter-rust.disabledWarnings',
    (disabledWarnings) =>
      @disabledWarnings = disabledWarnings

    @subscriptions.add atom.config.observe 'linter-rust.specifiedFeatures',
    (specifiedFeatures) =>
      @specifiedFeatures = specifiedFeatures

    @subscriptions.add atom.config.observe 'linter-rust.allowedToCacheVersions',
    (allowedToCacheVersions) =>
      @allowedToCacheVersions = allowedToCacheVersions

  destroy: ->
    do @subscriptions.dispose

  lint: (textEditor) =>
    @initCmd(textEditor.getPath()).then (result) =>
      [cmd_res, errorMode] = result
      [file, cmd] = cmd_res
      env = JSON.parse JSON.stringify process.env
      curDir = path.dirname file
      cwd = curDir
      command = cmd[0]
      args = cmd.slice 1
      env.PATH = path.dirname(cmd[0]) + path.delimiter + env.PATH

      # we set flags only for intermediate json support
      if errorMode == errorModes.FLAGS_JSON_CARGO
        if !env.RUSTFLAGS? or !(env.RUSTFLAGS.indexOf('--error-format=json') >= 0)
          additional = if env.RUSTFLAGS? then ' ' + env.RUSTFLAGS else ''
          env.RUSTFLAGS = '--error-format=json' + additional

      atom_linter.exec(command, args, {env: env, cwd: cwd, stream: 'both'})
        .then (result) =>
          {stdout, stderr, exitCode} = result
          # first, check if an output says specified features are invalid
          if stderr.indexOf('does not have these features') >= 0
            atom.notifications.addError "Invalid specified features",
              detail: "#{stderr}"
              dismissable: true
            []
          # then, if exit code looks okay, process an output
          else if exitCode is 101 or exitCode is 0
            # in dev mode show message boxes with output
            showDevModeWarning = (stream, message) ->
              atom.notifications.addWarning "Output from #{stream} while linting",
                detail: "#{message}"
                description: "This is shown because Atom is running in dev-mode and probably not an actual error"
                dismissable: true
            if do atom.inDevMode
              showDevModeWarning('stderr', stderr) if stderr
              showDevModeWarning('stdout', stdout) if stdout

            # call a needed parser
            output = errorMode.neededOutput(stdout, stderr)
            messages = errorMode.parse output, {@disabledWarnings, textEditor}

            # correct file paths
            messages.forEach (message) ->
              if !(path.isAbsolute message.filePath)
                message.filePath = path.join curDir, message.filePath
            messages
          else
            # whoops, we're in trouble -- let's output as much as we can
            atom.notifications.addError "Failed to run #{command} with exit code #{exitCode}",
              detail: "with args:\n #{args.join(' ')}\nSee console for more information"
              dismissable: true
            console.log "stdout:"
            console.log stdout
            console.log "stderr:"
            console.log stderr
            []
        .catch (error) ->
          console.log error
          atom.notifications.addError "Failed to run #{command}",
            detail: "#{error.message}"
            dismissable: true
          []

  initCmd: (editingFile) =>
    curDir = path.dirname editingFile
    cargoManifestPath = @locateCargo curDir
    if not @useCargo or not cargoManifestPath
      @decideErrorMode(curDir, 'rustc').then (mode) =>
        mode.buildArguments(this, [editingFile, cargoManifestPath]).then (cmd) =>
          [cmd, mode]
    else
      @decideErrorMode(curDir, 'cargo').then (mode) =>
        mode.buildArguments(this, cargoManifestPath).then (cmd) =>
          [cmd, mode]

  compilationFeatures: (cargo) =>
    if @specifiedFeatures.length > 0
      if cargo
        ['--features', @specifiedFeatures.join(' ')]
      else
        result = []
        cfgs = for f in @specifiedFeatures
          result.push ['--cfg', "feature=\"#{f}\""]
        result

  decideErrorMode: (curDir, commandMode) =>
    # error mode is cached to avoid delays
    if @cachedErrorMode? and @allowedToCacheVersions
      Promise.resolve().then () =>
        @cachedErrorMode
    else
      # current dir is set to handle overrides
      atom_linter.exec(@rustcPath, ['--version'], {cwd: curDir}).then (stdout) =>
        try
          match = XRegExp.exec(stdout, @patternRustcVersion)
          if match
            nightlyWithJSON = match.nightly and match.date > '2016-08-08'
            stableWithJSON = not match.nightly and semver.gte(match.version, '1.12.0')
            canUseIntermediateJSON = nightlyWithJSON or stableWithJSON
            switch commandMode
              when 'cargo'
                canUseProperCargoJSON = (match.nightly and match.date >= '2016-10-10') or
                  (match.beta or not match.nightly and semver.gte(match.version, '1.13.0'))
                if canUseProperCargoJSON
                  errorModes.JSON_CARGO
                # this mode is used only through August till October, 2016
                else if canUseIntermediateJSON
                  errorModes.FLAGS_JSON_CARGO
                else
                  errorModes.OLD_CARGO
              when 'rustc'
                if canUseIntermediateJSON
                  errorModes.JSON_RUSTC
                else
                  errorModes.OLD_RUSTC
          else
            throw 'rustc returned unexpected result: ' + stdout
      .then (result) =>
        @cachedErrorMode = result
        result


  locateCargo: (curDir) =>
    root_dir = if /^win/.test process.platform then /^.:\\$/ else /^\/$/
    directory = path.resolve curDir
    loop
      return path.join directory, @cargoManifestFilename if fs.existsSync path.join directory, @cargoManifestFilename
      break if root_dir.test directory
      directory = path.resolve path.join(directory, '..')
    return false

module.exports = LinterRust

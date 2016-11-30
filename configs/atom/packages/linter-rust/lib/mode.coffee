path = require 'path'

atom_linter = require 'atom-linter'
XRegExp = require 'xregexp'

pattern = XRegExp('(?<file>[^\n\r]+):(?<from_line>\\d+):(?<from_col>\\d+):\\s*\
  (?<to_line>\\d+):(?<to_col>\\d+)\\s+\
  ((?<error>error|fatal error)|(?<warning>warning)|(?<info>note|help)):\\s+\
  (?<message>.+?)[\n\r]+($|(?=[^\n\r]+:\\d+))', 's')

parseOldMessages = (output, {disabledWarnings, textEditor}) ->
  elements = []
  XRegExp.forEach output, pattern, (match) ->
    range = if match.from_col == match.to_col and match.from_line == match.to_line
      atom_linter.rangeFromLineNumber(textEditor, Number.parseInt(match.from_line, 10) - 1, Number.parseInt(match.from_col, 10) - 1)
    else
      [
        [match.from_line - 1, match.from_col - 1],
        [match.to_line - 1, match.to_col - 1]
      ]
    level = if match.error then 'error'
    else if match.warning then 'warning'
    else if match.info then 'info'
    else if match.trace then 'trace'
    else if match.note then 'note'
    element =
      type: level
      message: match.message
      file: match.file
      range: range
    elements.push element
  buildMessages elements, disabledWarnings

parseJsonMessages = (messages, {disabledWarnings}) ->
  elements = []
  for input in messages
    continue unless input and input.spans
    primary_span = input.spans.find (span) -> span.is_primary
    continue unless primary_span
    while primary_span.expansion and primary_span.expansion.span
      primary_span = primary_span.expansion.span
    range = [
      [primary_span.line_start - 1, primary_span.column_start - 1],
      [primary_span.line_end - 1, primary_span.column_end - 1]
    ]
    input.level = 'error' if input.level == 'fatal error'
    element =
      type: input.level
      message: input.message
      file: primary_span.file_name
      range: range
      children: input.children
    for span in input.spans
      unless span.is_primary
        element.children.push
          message: span.label
          range: [
            [span.line_start - 1, span.column_start - 1],
            [span.line_end - 1, span.column_end - 1]
          ]
    elements.push element
  buildMessages elements, disabledWarnings

parseJsonOutput = (output, {disabledWarnings, additionalFilter} ) ->
  results = output.split('\n').map (message) ->
    message = message.trim()
    if message.startsWith '{'
      json = JSON.parse message
      if additionalFilter?
        additionalFilter(json)
      else
        json
  .filter (m) -> m?
  parseJsonMessages results, {disabledWarnings}

buildMessages = (elements, disabledWarnings) ->
  messages = []
  lastMessage = null
  for element in elements
    switch element.type
      when 'info', 'trace', 'note'
        # Add only if there is a last message
        if lastMessage
          lastMessage.trace or= []
          lastMessage.trace.push
            type: "Trace"
            text: element.message
            filePath: element.file
            range: element.range
      when 'warning'
        # If the message is warning and user enabled disabling warnings
        # Check if this warning is disabled
        if disabledWarnings and disabledWarnings.length > 0
          messageIsDisabledLint = false
          for disabledWarning in disabledWarnings
            # Find a disabled lint in warning message
            if element.message.indexOf(disabledWarning) >= 0
              messageIsDisabledLint = true
              lastMessage = null
              break
          if not messageIsDisabledLint
            lastMessage = constructMessage "Warning", element
            messages.push lastMessage
        else
          lastMessage = constructMessage "Warning" , element
          messages.push lastMessage
      when 'error', 'fatal error'
        lastMessage = constructMessage "Error", element
        messages.push lastMessage
  return messages

constructMessage = (type, element) ->
  message =
    type: type
    text: element.message
    filePath: element.file
    range: element.range
  # children exists only in JSON messages
  if element.children
    message.trace = []
    for children in element.children
      message.trace.push
        type: "Trace"
        text: children.message
        filePath: element.file
        range: children.range or element.range
  message

buildRustcArguments = (linter, paths) ->
  [editingFile, cargoManifestPath] = paths
  Promise.resolve().then () =>
    rustcArgs = switch linter.rustcBuildTest
      when true then ['--cfg', 'test', '-Z', 'no-trans', '--color', 'never']
      else ['-Z', 'no-trans', '--color', 'never']
    cmd = [linter.rustcPath]
      .concat rustcArgs
    if cargoManifestPath
      cmd.push '-L'
      cmd.push path.join path.dirname(cargoManifestPath), linter.cargoDependencyDir
    compilationFeatures = linter.compilationFeatures(false)
    cmd = cmd.concat compilationFeatures if compilationFeatures
    cmd = cmd.concat [editingFile]
    [editingFile, cmd]

cachedUsingMultitoolForClippy = null

buildCargoArguments = (linter, cargoManifestPath) ->
  buildCargoPath = (cargoPath, cargoCommand) ->
    # the result is cached to avoid delays
    if cachedUsingMultitoolForClippy? and linter.allowedToCacheVersions
      Promise.resolve().then () =>
        cachedUsingMultitoolForClippy
    else
      # Decide if should use older multirust or newer rustup
      usingMultitoolForClippy =
        atom_linter.exec 'rustup', ['--version'], {ignoreExitCode: true}
          .then ->
            result: true, tool: 'rustup'
          .catch ->
            # Try to use older multirust at least
            atom_linter.exec 'multirust', ['--version'], {ignoreExitCode: true}
              .then ->
                result: true, tool: 'multirust'
              .catch ->
                result: false
      usingMultitoolForClippy.then (canUseMultirust) ->
        if cargoCommand == 'clippy' and canUseMultirust.result
          [canUseMultirust.tool, 'run', 'nightly', 'cargo']
        else
          [cargoPath]
      .then (cached) =>
        cachedUsingMultitoolForClippy = cached
        cached

  cargoArgs = switch linter.cargoCommand
    when 'check' then ['check']
    when 'test' then ['test', '--no-run']
    when 'rustc' then ['rustc', '-Zno-trans', '--color', 'never']
    when 'clippy' then ['clippy']
    else ['build']

  compilationFeatures = linter.compilationFeatures(true)
  buildCargoPath(linter.cargoPath, linter.cargoCommand).then (cmd) ->
    cmd = cmd
      .concat cargoArgs
      .concat ['-j', linter.jobsNumber]
    cmd = cmd.concat compilationFeatures if compilationFeatures
    cmd = cmd.concat ['--manifest-path', cargoManifestPath]
    [cargoManifestPath, cmd]

# These define the behabiour of each error mode linter-rust has
errorModes =
  JSON_RUSTC:
    neededOutput: (stdout, stderr) ->
      stderr

    parse: (output, options) =>
      parseJsonOutput output, options

    buildArguments: (linter, file) ->
      buildRustcArguments(linter, file).then (cmd_res) ->
        [file, cmd] = cmd_res
        cmd = cmd.concat ['--error-format=json']
        [file, cmd]

  JSON_CARGO:
    neededOutput: (stdout, stderr) ->
      stdout

    parse: (output, options) ->
      options.additionalFilter = (json) ->
        if json? and json.reason == "compiler-message"
          json.message
      parseJsonOutput output, options

    buildArguments: (linter, file) ->
      buildCargoArguments(linter, file).then (cmd_res) ->
        [file, cmd] = cmd_res
        cmd = cmd.concat ['--message-format', 'json']
        [file, cmd]

  FLAGS_JSON_CARGO:
    neededOutput: (stdout, stderr) ->
      stderr

    parse: parseJsonOutput

    buildArguments: buildCargoArguments

  OLD_RUSTC:
    neededOutput: (stdout, stderr) ->
      stderr

    parse: parseOldMessages

    buildArguments: buildRustcArguments

  OLD_CARGO:
    neededOutput: (stdout, stderr) ->
      stderr

    parse: parseOldMessages

    buildArguments: buildCargoArguments

module.exports = errorModes

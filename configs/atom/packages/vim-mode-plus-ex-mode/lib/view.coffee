_ = require 'underscore-plus'
{SelectListView, $, $$} = require 'atom-space-pen-views'
fuzzaldrin = require 'fuzzaldrin'

MAX_ITEMS = 5
module.exports =
class View extends SelectListView
  initialInput: null

  # Disable throttling populateList for initialInput
  schedulePopulateList: ->
    if @initialInput
      @populateList() if @isOnDom()
      @initialInput = false
    else
      super

  initialize: ->
    @setMaxItems(MAX_ITEMS)
    @commands = require './commands'
    @addClass('vim-mode-plus-ex-mode')
    super

  getFilterKey: ->
    'displayName'

  cancelled: ->
    @hide()

  toggle: (@vimState, @commandKind) ->
    if @panel?.isVisible()
      @cancel()
    else
      {@editorElement, @editor} = @vimState
      @show()

  show: ->
    @initialInput = true
    @count = null
    @storeFocusedElement()
    @panel ?= atom.workspace.addModalPanel({item: this})
    @panel.show()
    @setItems(@getItemsFor(@commandKind))
    @focusFilterEditor()

  getItemsFor: (kind) ->
    commands = _.keys(@commands[kind])
    humanize = (name) -> _.humanizeEventName(_.dasherize(name))
    switch kind
      when 'normalCommands'
        commands.map (name) -> {name, displayName: name}
      when 'toggleCommands', 'numberCommands'
        commands.map (name) -> {name, displayName: humanize(name)}

  executeCommand: (kind, name) ->
    action = @commands[kind][name]
    action(@vimState, @count)

  hide: ->
    @panel?.hide()

  getCommandKindFromQuery: (query) ->
    if query.match(/^!/)
      'toggleCommands'
    else if query.match(/(\d+)(%)?$/)
      'numberCommands'
    else
      null

  # Use as command missing hook.
  getEmptyMessage: (itemCount, filteredItemCount) ->
    query = @getFilterQuery()
    return unless @commandKind = @getCommandKindFromQuery(query)

    items = @getItemsFor(@commandKind)
    switch @commandKind
      when 'toggleCommands'
        filterQuery = query[1...] # to trim first '!'
        items = fuzzaldrin.filter(items, filterQuery, key: @getFilterKey())
      when 'numberCommands'
        [number, percent] = query.match(/(\d+)(%)?$/)[1..2]
        @count = Number(number)
        items = items.filter ({name}) ->
          if percent?
            name is 'moveToLineByPercent'
          else
            name is 'moveToLine'

    @setError(null)
    @setFallbackItems(items)
    @selectItemView(@list.find('li:first'))

  setFallbackItems: (items) ->
    for item in items
      itemView = $(@viewForItem(item))
      itemView.data('select-list-item', item)
      @list.append(itemView)

  viewForItem: ({displayName}) ->
    # console.log displayName
    # Style matched characters in search results
    filterQuery = @getFilterQuery()
    filterQuery = filterQuery[1..] if filterQuery.startsWith('!')

    matches = fuzzaldrin.match(displayName, filterQuery)
    # console.log matches
    $$ ->
      highlighter = (command, matches, offsetIndex) =>
        lastIndex = 0
        matchedChars = [] # Build up a set of matched chars to be more semantic

        for matchIndex in matches
          matchIndex -= offsetIndex
          continue if matchIndex < 0 # If marking up the basename, omit command matches
          unmatched = command.substring(lastIndex, matchIndex)
          if unmatched
            @span matchedChars.join(''), class: 'character-match' if matchedChars.length
            matchedChars = []
            @text unmatched
          matchedChars.push(command[matchIndex])
          lastIndex = matchIndex + 1

        @span matchedChars.join(''), class: 'character-match' if matchedChars.length
        # Remaining characters are plain text
        @text command.substring(lastIndex)

      @li class: 'event', 'data-event-name': name, =>
        @span title: displayName, -> highlighter(displayName, matches, 0)

  confirmed: ({name}) ->
    @cancel()
    @executeCommand(@commandKind, name)

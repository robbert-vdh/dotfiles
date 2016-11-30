{Emitter, CompositeDisposable} = require 'atom'
getEditorState = null

module.exports =
  activate: ->
    @emitter = new Emitter
    @subscriptions = new CompositeDisposable
    @subscriptions.add atom.commands.add 'atom-text-editor:not([mini])',
      'vim-mode-plus-ex-mode:open': => @toggle('normalCommands')
      'vim-mode-plus-ex-mode:toggle-setting': => @toggle('toggleCommands')

  toggle: (commandKind) ->
    editor = atom.workspace.getActiveTextEditor()
    @getEditorState(editor).then (vimState) =>
      @getView().toggle(vimState, commandKind)

  getEditorState: (editor) ->
    if getEditorState?
      Promise.resolve(getEditorState(editor))
    else
      new Promise (resolve) =>
        @onDidConsumeVim ->
          resolve(getEditorState(editor))

  deactivate: ->
    @subscriptions.dispose()
    @view?.destroy?()
    {@subscriptions, @view} = {}

  getView: ->
    @view ?= new (require('./view'))

  onDidConsumeVim: (fn) ->
    @emitter.on('did-consume-vim', fn)

  consumeVim: (service) ->
    {getEditorState} = service
    @emitter.emit('did-consume-vim')

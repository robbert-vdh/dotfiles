'use babel';

describe 'The OpenCL build provider for Atom Linter',  ->
  linter = require('../lib/linter-opencl').provideLinter().lint

  beforeEach ->
    waitsForPromise ->
      atom.config.set('linter-opencl.pythonPath', 'python')
      atom.config.set('linter-opencl.vendor', 'AMD')
      atom.config.set('linter-opencl.openCL.platformIndex', 0)
      atom.config.set('linter-opencl.hybridGraphics.enable', false)
      atom.config.set('linter-opencl.hybridGraphics.offloadingPath', '/usr/bin/optirun')
      atom.config.set('linter-opencl.debug', true)
      atom.packages.activatePackage('language-opencl')
      atom.packages.activatePackage('linter-opencl')
      console.log(atom.packages.resolvePackagePath('linter-opencl'))
      return atom.packages.activatePackage("linter-opencl")

  it 'should be in the packages list', ->
    expect(atom.packages.isPackageLoaded('linter-opencl')).toBe(true)

  it 'should be an active package', ->
    expect(atom.packages.isPackageActive('linter-opencl')).toBe(true)

  it 'find an error in error.cl', ->
    waitsForPromise ->
      filePath = '/home/travis/build/BenSolus/linter-opencl/spec/files/error.cl'
      expect(filePath).toExistOnDisk()
      return atom.workspace.open(filePath).then (editor) ->
        return linter(editor).then (messages) ->
          expect(messages.length).toEqual(2)
          expect(messages[0].type).toEqual('error')
          expect(messages[0].text).toEqual('expected a ";"')
          expect(messages[1].type).toEqual('warning')
          expect(messages[1].text).toEqual('variable "a" was declared but never')

  it 'find an error in error.cl', ->
    waitsForPromise ->
      filePath = '/home/travis/build/BenSolus/linter-opencl/spec/files/correct.cl'
      expect(filePath).toExistOnDisk()
      return atom.workspace.open(filePath).then (editor) ->
        return linter(editor).then (messages) ->
          expect(messages.length).toEqual(0)

# Exec

Node's Process spawning APIs beautified

## Installation

```sh
npm install --save sb-exec
```

## API

```js
type $OptionsAccepted = {
  timeout?: number | Infinity,
  stream?: 'stdout' | 'stderr'  | 'both',
  env: Object,
  stdin?: ?string,
  local?: {
    directory: string,
    prepend?: boolean
  },
  throwOnStdErr?: boolean = true,
  allowEmptyStderr?: boolean = false,
  ignoreExitCode?: boolean
} // Also supports all options of child_process::spawn

type PromisedProcess = {
  then(callback: Function): Promise
  catch(callback: Function): Promise
  kill(signal: number)
}

export function exec(filePath: string, parameters: array, options: $OptionsAccepted): PromisedProcess
export function execNode(filePath: string, parameters: array, options: $OptionsAccepted): PromisedProcess
```

## Explanation

### `options.local`

`options.local` adds node executables in `node_modules` relative to
`options.local.directory` to `PATH` like in npm scripts.

`options.local.prepend` prioritizes local executables over ones already in `PATH`.

## License

This project is licensed under the terms of MIT License, see the LICENSE file
for more info

# 0.7.1
* Added beta and stable versions to proper JSON support mode (@White-Oak)

# 0.7.0
* Added a proper support for cargo --message-format json (@White-Oak)
* Fix span for macro invocations (@utkarshkukreti)

# 0.6.0
* Use asynchronous execs instead of synchronous (@White-Oak)

# 0.5.7
* Assume non-nightly versions >= 1.12.0, fix #78 (@jviide)

# 0.5.6
* Fixed the way json errors are requested from cargo & rustc, fix #69 (@White-Oak)

# 0.5.5
* Update atom-package-deps to version 4.3.0
* Fix #70 (@Maplicant)

# 0.5.4
* Fix indexOf check (@benstreb)

# 0.5.2
* Removed default values for features specified, fix #65 (@White-Oak)

# 0.5.1
* Ability to specify features to lint (@White-Oak)

# 0.5.0
* Added support for JSON error output of rustc (@White-Oak)

# 0.4.6
* Fixed undesirable modification of global objects (@lhecker)
* Trim whitespace from rustc and cargo paths (@chriskrycho)

# 0.4.5
* Added fallback to cargo for clippy if multirust isn't available (@White-Oak)
* Added option to disable certain lintings in editor (@White-Oak)

# 0.4.1
* Update xregexp to version 3.1.0
* Allow using multirust for clippy (@White-Oak)

# 0.4.0
* Add choice to use clippy for linting. Tanks to @White-Oak

# 0.3.0
* Minor refactor + option for cargo-check (@colin-kiegel)

# 0.2.11
* Add option to use "cargo rustc" instead of "cargo build"

# 0.2.10
* Add name for the linter (@LegNeato)

# 0.2.9
* Fix #36 (@andrewrynhard)

# 0.2.8
* Include rustc's help messages in lints (@johnsoft)

# 0.2.7
* Use "Trace" instead of "Warning" for rustc's informational messages (@johnsoft)

# 0.2.6
* Add config option for building test code (@dgriffen)

# 0.2.5
* Changed 'rustc' and 'cargo' commands so that test code gets linted in addition to normal code (@psFried)

# 0.2.4
* Support multiline lint messages
* Use explicit path to rusts executables
* Move parsing into dedicated method for testability


# 0.2.3
* Include columns in lint range (thanks to @b52)

# 0.2.2
* Remove 'linter-package' from the package.json. Close #23

# 0.2.1
* Fix and close #20.

# 0.2.0
* Migrate to linter-plus
* Add Rust home path configuration option

# 0.1.0
* Remove linting "on the fly"
* Support Cargo. See #5. Thanks @liigo for ideas

# 0.0.13
Added linting on the fly (experimental)

# 0.0.12
Fix #11

# 0.0.11
Fix deprecated activationEvents. Closes #10

# 0.0.10
Extern crate problem fix. See PR #9

# 0.0.9
Fix spaces problem in the executable path. Closes #3

# 0.0.8
Show file name where an error occurred

# 0.0.7
Fix uncaught error 'ENOENT'. See #3 and AtomLinter/Linter#330

# 0.0.6
Fix rustc command-line options

# 0.0.5
Add info support. See #7

# 0.0.4
Correct regex string. See #6

# 0.0.3
Check original file, not a tmp copy. See #1

# 0.0.2
Used JSON Schema for linter config

# 0.0.1
* Implemented first version of 'linter-rust'

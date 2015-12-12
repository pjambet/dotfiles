# test.vim

A Vim wrapper for running tests on different granularities.

![usage overview](/screenshots/granularity.gif)

Currently the following testing frameworks are supported:

| Language       | Frameworks                            | Identifiers                     |
| :------------: | ------------------------------------- | ------------------------------- |
| **Ruby**       | RSpec, [Minitest][minitest], Cucumber | `rspec`, `minitest`, `cucumber` |
| **JavaScript** | Mocha, Jasmine                        | `mocha`, `jasmine`              |
| **Python**     | Nose, PyTest, Django                  | `nose`, `pytest`, `djangotest`  |
| **Elixir**     | ExUnit, ESpec                         | `exunit`, `espec`               |
| **Go**         | Go                                    | `gotest`                        |
| **Clojure**    | Fireplace.vim                         | `fireplacetest`                 |
| **Shell**      | Bats                                  | `bats`                          |
| **VimScript**  | VSpec, Vader.vim                      | `vspec`, `vader`                |
| **Lua**        | Busted                                | `busted`                        |
| **PHP**        | PHPUnit, Behat, PHPSpec               | `phpunit`, `behat`, `phpspec`   |
| **Java**       | Maven                                 | `maventest`                     |

## Idea

Since Gary Bernhardt invented testing from Vim, there have been multiple
plugins implementing this functionality (rspec.vim, vroom.vim etc). However,
all of these solutions have bad designs, unclear ideas and aren't extendable.
So I decided to create test.vim, featuring:

* zero dependencies
* zero configuration required (it Does the Right Thing™, see [**Philosophy**](https://github.com/janko-m/vim-test/wiki))
* interface for adding new testing frameworks
* automatic detection of correct test runner
* **polyfill** for nearest tests (by [constructing regexes](#commands))
* built-in integration with Dispatch/Vimux/Tslime
* fully customized CLI options configuration

Internally test.vim consists of a thoughtfully designed core, and testing
frameworks are simply plugged in, so that they all work in the same unified
way.

Ruby users, you get all of the features of rspec.vim + vroom.vim, but
without any of the tedious configuration (test.vim knows how you want to
run your test command).

## Setup

```vim
Plug[in] 'janko-m/vim-test'
```

Add your preferred mappings to your `.vimrc` file:

```vim
nmap <silent> <leader>t :TestNearest<CR>
nmap <silent> <leader>T :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>l :TestLast<CR>
nmap <silent> <leader>g :TestVisit<CR>
```

| Command        | Description                                                                                                                                                                                                                                                                            |
| :-------       | :-----------                                                                                                                                                                                                                                                                           |
| `:TestNearest` | In a test file runs the test nearest to the cursor, otherwise runs the last nearest test. In test frameworks that don't support line numbers it will **polyfill** this functionality with [regexes](#commands).                                                                        |
| `:TestFile`    | In a test file runs all tests in the current file, otherwise runs the last file tests.                                                                                                                                                                                                 |
| `:TestSuite`   | Runs the whole test suite (if the current file is a test file, runs that framework's test suite, otherwise determines the test framework from the last run test).                                                                                                                      |
| `:TestLast`    | Runs the last test.                                                                                                                                                                                                                                                                    |
| `:TestVisit`   | Visits the test file from which you last run your tests (useful when you're trying to make a test pass, and you dive deep into application code and close your test buffer to make more space, and once you've made it pass you want to go back to the test file to write more tests). |

## Strategies

You can instruct test.vim to run your tests with different strategies (with
synchronous or asynchronous execution). To use a specific strategy, assign
it like this:

```vim
" make test commands execute using dispatch.vim
let test#strategy = "dispatch"
```

| Strategy                                                                        | Identifier | Description                                                                      |
| :-----:                                                                         | :-----:    | :----------                                                                      |
| **Basic**&nbsp;(default)                                                        | `basic`    | Runs test commands with `:!`, which switches your Vim to the terminal.           |
| **Neovim**                                                                      | `neovim`   | Runs test commands with `:terminal`, which spawns a terminal inside your Neovim. |
| [**Neoterm**](https://github.com/kassio/neoterm)                                | `neoterm`  | Runs test commands with `:T`, see neoterm docs for display customization.        |
| [**Dispatch.vim**](https://github.com/tpope/vim-dispatch)                       | `dispatch` | Runs test commands with `:Dispatch`.                                             |
| [**Vimux**](https://github.com/benmills/vimux)                                  | `vimux`    | Runs test commands in a small tmux pane at the bottom of your terminal.          |
| [**Tslime.vim**](https://github.com/kikijump/tslime.vim)                        | `tslime`   | Runs test commands in a tmux pane you specify.                                   |
| [**Vim&nbsp;Tmux&nbsp;Runner**](https://github.com/christoomey/vim-tmux-runner) | `vtr`      | Runs test commands in a small tmux pane.                                         |
| **Terminal.app**                                                                | `terminal` | Sends test commands to Terminal.app (useful in MacVim GUI).                      |
| **iTerm2.app**                                                                  | `iterm`    | Sends test commands to iTerm2.app (useful in MacVim GUI).                        |
| **Vagrant**                                                                     | `vagrant`  | Runs test commands with `vagrant ssh`.                                           |

### Custom Strategies

Strategy is a function which takes one argument – the shell command for the
test being run – and it is expected to run that command in some way. Test.vim
comes with many predefined strategies (see above), but if none of them suit
your needs, you can define your own custom strategy like this:

```vim
function! MyStrategy(cmd)
  echo 'It works! Command for running tests: ' . a:cmd
endfunction

let g:test#custom_strategies = {'my_strategy': function('MyStrategy')}
let g:test#strategy = 'my_strategy'
```

## Commands

![nearest polyfill](/screenshots/nearest.gif)

You can execute Test.vim commands directly, and pass them CLI options:

```
:TestNearest --verbose
:TestFile --format documentation
:TestSuite --fail-fast
```

If you want some options to stick around, see [Configuring](#configuring).

### Runner commands

Aside from the main commands, you get a corresponding Vim command for each
test runner (which also accept options):

```
:RSpec --tag ~slow
:Mocha --grep 'API'
:ExUnit --trace
:Nose --failed
```

I found these commands to be really useful when using multiple testing
frameworks in the same project.

## Configuring

### CLI options

If you want some CLI options to stick around, you can configure them in your
`.vimrc`:

```vim
let test#ruby#minitest#options = '--verbose'
```

You can also choose a more granular approach:

```vim
let test#ruby#rspec#options = {
  \ 'nearest': '--backtrace',
  \ 'file':    '--format documentation',
  \ 'suite':   '--tag ~slow',
\}
```
### Executable

You can instruct test.vim to use a custom executable for a test runner.

```vim
let test#ruby#rspec#executable = 'foreman run rspec'
```

### File pattern

Test.vim has file pattern it uses to determine whether a file belongs to
certain testing framework. You can override that pattern by overriding the
`file_pattern` variable:

```vim
let test#ruby#minitest#file_pattern = '_spec\.rb' " the default is '_test\.rb'
```

### Language-specific

#### Python

Since there are multiple Python test runners for the same type of tests,
test.vim has no way of detecting which one did you intend to use. By default
the first available will be chosen, but you can force a specific one:

``` vim
let test#python#runner = 'pytest'
" or
let test#python#runner = 'nose'
" or
let test#python#runner = 'djangotest'
```

## Extending

If you wish to extend this plugin with your own test runners, first of all,
if the runner is well-known, I would encourage to help me merge it into
test.vim.

That being said, if you want to do this for yourself, you need to do 2 things.
First, add your runner to the list in your `.vimrc`:

```vim
" First letter of runner's name must be uppercase
let test#runners = {'MyLanguage': ['MyRunner']}
```

Second, create `~/.vim/autoload/test/mylanguage/myrunner.vim`, and define the following
methods:

```vim
" Returns true if the given file belongs to your test runner
function! test#mylanguage#myrunner#test_file(file)

" Returns test runner's arguments which will run the current file and/or line
function! test#mylanguage#myrunner#build_position(type, position)

" Returns processed args (if you need to do any processing)
function! test#mylanguage#myrunner#build_args(args)

" Returns the executable of your test runner
function! test#mylanguage#myrunner#executable()
```

See [`autoload/test`](/autoload/test) for examples.

## Running tests

Tests are run using a Ruby test runner, so you'll have to have Ruby installed.
Then run

```sh
$ gem install vim-flavor
```

Now you can run tests with

```sh
$ vim-flavor test spec/
```

Or if you're inside of Vim, you can simply run `:VSpec` provided by test.vim.

## Credits

This plugin was strongly influenced by Gary Bernhardt's Destroy All Software.
I also want to thank [rspec.vim](https://github.com/thoughtbot/vim-rspec), from
which I borrowed GUI support for OS X, and Windows support. And also thanks to
[vroom.vim](https://github.com/skalnik/vim-vroom).

## License

Copyright © Janko Marohnić. Distributed under the same terms as Vim itself. See
`:help license`.

[minitest]: https://github.com/janko-m/vim-test/wiki/Minitest

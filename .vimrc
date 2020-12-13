" sensible.vim - Defaults everyone can agree on
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.2

if exists('g:loaded_sensible') || &compatible
  finish
else
  let g:loaded_sensible = 'yes'
endif

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

" Use :help 'option' to see the documentation for the given option.

set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab

set nrformats-=octal

if !has('nvim') && &ttimeoutlen == -1
  set ttimeout
  set ttimeoutlen=100
endif

set incsearch
" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

set laststatus=2
set ruler
set wildmenu

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
  set shell=/usr/bin/env\ bash
endif

set autoread

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif
if !empty(&viminfo)
  set viminfo^=!
endif
set sessionoptions-=options
set viewoptions-=options

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
  set t_Co=16
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

if empty(mapcheck('<C-U>', 'i'))
  inoremap <C-U> <C-G>u<C-U>
endif
if empty(mapcheck('<C-W>', 'i'))
  inoremap <C-W> <C-G>u<C-W>
endif

" vim:set ft=vim et sw=2:
" ---

" ---
" https://github.com/christoomey/vim-tmux-navigator/blob/master/plugin/tmux_navigator.vim
" Maps <C-h/j/k/l> to switch vim splits in the given direction. If there are
" no more windows in that direction, forwards the operation to tmux.
" Additionally, <C-\> toggles between last active vim splits/tmux panes.

if exists("g:loaded_tmux_navigator") || &cp || v:version < 700
  finish
endif
let g:loaded_tmux_navigator = 1

function! s:VimNavigate(direction)
  try
    execute 'wincmd ' . a:direction
  catch
    echohl ErrorMsg | echo 'E11: Invalid in command-line window; <CR> executes, CTRL-C quits: wincmd k' | echohl None
  endtry
endfunction

if !get(g:, 'tmux_navigator_no_mappings', 0)
  nnoremap <silent> <c-h> :TmuxNavigateLeft<cr>
  nnoremap <silent> <c-j> :TmuxNavigateDown<cr>
  nnoremap <silent> <c-k> :TmuxNavigateUp<cr>
  nnoremap <silent> <c-l> :TmuxNavigateRight<cr>
  nnoremap <silent> <c-\> :TmuxNavigatePrevious<cr>
endif

if empty($TMUX)
  command! TmuxNavigateLeft call s:VimNavigate('h')
  command! TmuxNavigateDown call s:VimNavigate('j')
  command! TmuxNavigateUp call s:VimNavigate('k')
  command! TmuxNavigateRight call s:VimNavigate('l')
  command! TmuxNavigatePrevious call s:VimNavigate('p')
  finish
endif

command! TmuxNavigateLeft call s:TmuxAwareNavigate('h')
command! TmuxNavigateDown call s:TmuxAwareNavigate('j')
command! TmuxNavigateUp call s:TmuxAwareNavigate('k')
command! TmuxNavigateRight call s:TmuxAwareNavigate('l')
command! TmuxNavigatePrevious call s:TmuxAwareNavigate('p')

if !exists("g:tmux_navigator_save_on_switch")
  let g:tmux_navigator_save_on_switch = 0
endif

if !exists("g:tmux_navigator_disable_when_zoomed")
  let g:tmux_navigator_disable_when_zoomed = 0
endif

function! s:TmuxOrTmateExecutable()
  return (match($TMUX, 'tmate') != -1 ? 'tmate' : 'tmux')
endfunction

function! s:TmuxVimPaneIsZoomed()
  return s:TmuxCommand("display-message -p '#{window_zoomed_flag}'") == 1
endfunction

function! s:TmuxSocket()
  " The socket path is the first value in the comma-separated list of $TMUX.
  return split($TMUX, ',')[0]
endfunction

function! s:TmuxCommand(args)
  let cmd = s:TmuxOrTmateExecutable() . ' -S ' . s:TmuxSocket() . ' ' . a:args
  let l:x=&shellcmdflag
  let &shellcmdflag='-c'
  let retval=system(cmd)
  let &shellcmdflag=l:x
  return retval
endfunction

function! s:TmuxNavigatorProcessList()
  echo s:TmuxCommand("run-shell 'ps -o state= -o comm= -t ''''#{pane_tty}'''''")
endfunction
command! TmuxNavigatorProcessList call s:TmuxNavigatorProcessList()

let s:tmux_is_last_pane = 0
augroup tmux_navigator
  au!
  autocmd WinEnter * let s:tmux_is_last_pane = 0
augroup END

function! s:NeedsVitalityRedraw()
  return exists('g:loaded_vitality') && v:version < 704 && !has("patch481")
endfunction

function! s:ShouldForwardNavigationBackToTmux(tmux_last_pane, at_tab_page_edge)
  if g:tmux_navigator_disable_when_zoomed && s:TmuxVimPaneIsZoomed()
    return 0
  endif
  return a:tmux_last_pane || a:at_tab_page_edge
endfunction

function! s:TmuxAwareNavigate(direction)
  let nr = winnr()
  let tmux_last_pane = (a:direction == 'p' && s:tmux_is_last_pane)
  if !tmux_last_pane
    call s:VimNavigate(a:direction)
  endif
  let at_tab_page_edge = (nr == winnr())
  " Forward the switch panes command to tmux if:
  " a) we're toggling between the last tmux pane;
  " b) we tried switching windows in vim but it didn't have effect.
  if s:ShouldForwardNavigationBackToTmux(tmux_last_pane, at_tab_page_edge)
    if g:tmux_navigator_save_on_switch == 1
      try
        update " save the active buffer. See :help update
      catch /^Vim\%((\a\+)\)\=:E32/ " catches the no file name error
      endtry
    elseif g:tmux_navigator_save_on_switch == 2
      try
        wall " save all the buffers. See :help wall
      catch /^Vim\%((\a\+)\)\=:E141/ " catches the no file name error
      endtry
    endif
    let args = 'select-pane -t ' . shellescape($TMUX_PANE) . ' -' . tr(a:direction, 'phjkl', 'lLDUR')
    silent call s:TmuxCommand(args)
    if s:NeedsVitalityRedraw()
      redraw!
    endif
    let s:tmux_is_last_pane = 1
  else
    let s:tmux_is_last_pane = 0
  endif
endfunction
" ---

" Beginning of my own shit:
" https://vim.fandom.com/wiki/Mac_OS_X_clipboard_sharing
set clipboard=unnamed

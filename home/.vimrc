" Most of this file is copied from Gary Berhardt's vimrc:
" https://github.com/garybernhardt/dotfiles/blob/master/.vimrc
" and some of it is mine. Enjoy!


set nocompatible

"NeoBundle Scripts-----------------------------
if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/neoyank.vim'
NeoBundle 'christoomey/vim-tmux-navigator'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'bkad/CamelCaseMotion'
NeoBundle 'Townk/vim-autoclose'
NeoBundle 'ctrlpvim/ctrlp.vim'
NeoBundle 'mattn/gist-vim'
NeoBundle 'mattn/webapi-vim'
NeoBundle 'bronson/vim-visual-star-search'
NeoBundle 'janko-m/vim-test'
NeoBundle 'editorconfig/editorconfig-vim'
NeoBundle 'elixir-lang/vim-elixir'
NeoBundle 'rking/ag.vim'
NeoBundle 'godlygeek/tabular'
NeoBundle 'jgdavey/tslime.vim'
NeoBundle 'jimenezrick/vimerl'
NeoBundle 'mbbill/undotree'


" You can specify revision/branch/tag.
" NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------


" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " BASIC EDITING CONFIGURATION
" """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" " allow unsaved background buffers and remember marks/undo for them
set hidden
" " remember more commands and search history
set clipboard=unnamed
set history=10000
set expandtab
set tabstop=2
set shiftwidth=2
set shiftround
set softtabstop=2
set autoindent
set laststatus=2
set showmatch
set incsearch
set hlsearch
set number
" " make searches case-sensitive only if they contain upper-case characters
set ignorecase smartcase
" " highlight current line
set cursorline
set cmdheight=2
set switchbuf=useopen
set numberwidth=5
set showtabline=0
set winwidth=79
set shell=zsh
" " Prevent Vim from clobbering the scrollback buffer. See
" " http://www.shallowsky.com/linux/noaltscreen.html
set t_ti= t_te=
" " keep more context when scrolling off the end of a buffer
set scrolloff=3
" " Store temporary files in a central spot
set nobackup
set noswapfile
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
" " allow backspacing over everything in insert mode
set backspace=indent,eol,start
" " display incomplete commands
set showcmd
" " Enable highlighting for syntax
syntax on
" " use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
" " make tab completion for files/buffers act like bash
set wildmenu
let mapleader=","

" try to make redraw a little bit faster in big files
set re=1
set ttyfast
set lazyredraw

set relativenumber

map ; :

function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()

colorscheme solarized
set bg=dark

" leader leader goes to the previous file
nnoremap <leader><leader> <c-^>

" fixes C-h for neovim
if has('nvim')
  nmap <BS> <C-W>h
endif

" split shortcuts
map <leader>sv :vsplit<CR>
map <leader>sh :split<CR>


set statusline=%<%f\ [%{strlen(&fenc)?&fenc:'none'}]%h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Auto Commands
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!

  autocmd BufLeave,FocusLost * silent! wall
  autocmd FocusLost * :set number
  autocmd FocusGained * :set relativenumber

augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" unite.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:unite_source_history_yank_enable = 1
" call unite#filters#matcher_default#use(['matcher_fuzzy'])
" nnoremap <leader>t :<C-u>Unite -no-split -buffer-name=files   -start-insert file_rec/async:!<cr>
" " nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files   -start-insert file<cr>
" nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru     -start-insert file_mru<cr>
" " nnoremap <leader>o :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
" nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank    history/yank<cr>
" nnoremap <leader>e :<C-u>Unite -no-split -buffer-name=buffer  buffer<cr>

" " Custom mappings for the unite buffer
" autocmd FileType unite call s:unite_settings()
" function! s:unite_settings()
"   " Play nice with supertab
"   let b:SuperTabDisabled=1
"   " Enable navigation with control-j and control-k in insert mode
"   imap <buffer> <C-j>   <Plug>(unite_select_next_line)
"   imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
" endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrlp.vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>gv :CtrlP app/views<cr>
map <leader>gc :CtrlP app/controllers<cr>
map <leader>gm :CtrlP app/models<cr>
map <leader>gh :CtrlP app/helpers<cr>
map <leader>gl :CtrlP lib<cr>
map <leader>gp :CtrlP public<cr>
map <leader>gg :topleft 100 :split Gemfile<cr>
map <leader>f :CtrlP<cr>
map <leader>F :CtrlP %%<cr>
map <leader>h :CtrlPBuffer<cr>
map <leader>H :CtrlPMRUFiles<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CamelCaseMotion
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap e


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MULTIPURPOSE TAB KEY
" Indent if we're at the beginning of a line. Else, do completion.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction
inoremap <tab> <c-r>=InsertTabWrapper()<cr>
inoremap <s-tab> <c-n>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" gist-vim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gist_post_private = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" undotree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
nnoremap <F5> :UndotreeToggle<cr>
if has("persistent_undo")
    set undodir=~/.undodir/
    set undofile
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OPEN FILES IN DIRECTORY OF CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-tmux-navigator
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:tmux_navigator_save_on_switch = 1

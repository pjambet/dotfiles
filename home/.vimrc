" " This is Gary Bernhardt's .vimrc file
" " vim:set ts=2 sts=2 sw=2 expandtab:

set nocompatible

" call pathogen#infect()
call pathogen#infect()


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
" " Enable file type detection.
" " Use the default filetype settings, so that mail gets 'tw' set to 72,
" " 'cindent' is on in C files, etc.
" " Also load indent files, to automatically do language-dependent indenting.
filetype plugin indent on
" " use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
" " make tab completion for files/buffers act like bash
set wildmenu
let mapleader=","

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" CUSTOM AUTOCMDS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup vimrcEx
  " Clear all autocmds in the group
  autocmd!
  autocmd FileType text setlocal textwidth=78
  " Jump to last cursor position unless it's invalid or in an event handler
  autocmd BufReadPost *
    \ if line("'\"") > 0 && line("'\"") <= line("$") |
    \   exe "normal g`\"" |
    \ endif

  "for ruby, autoindent with two spaces, always expand tabs
  autocmd FileType ruby,haml,eruby,yaml,html,javascript,sass,scss,cucumber set ai sw=2 sts=2 ts=2 et
  autocmd FileType python,php,c,go,rust,less,scss,javascript set sw=4 sts=4 ts=4 et
  autocmd FileType python setlocal formatoptions+=q

  autocmd! BufRead,BufNewFile *.sass setfiletype sass
  autocmd! BufRead,BufNewFile *.scss setfiletype scss

  autocmd FileType javascript nnoremap <silent><F1> :JSHint<CR>|inoremap <silent><F1> <C-O>:JSHint<CR>|vnoremap <silent><F1> :JSHint<CR>|cnoremap <F1> JSHint

  autocmd BufRead *.mkd  set ai formatoptions=tcroqn2 comments=n:&gt;
  autocmd BufRead *.markdown  set ai formatoptions=tcroqn2 comments=n:&gt;
  fun! IsModified()
    let bufmodified = getbufvar(bufnr('%'), "&mod")
    echo "foo"
    if bufmodified == 0
      highlight StatusLine ctermfg=black
    else
      highlight StatusLine ctermfg=red
    endif
  endfun
  " autocmd InsertEnter * call IsModified()
  " autocmd InsertChange * call IsModified()
  " autocmd InsertLeave * call IsModified()
  " autocmd FileChangedShell * call IsModified()



  " Indent p tags
  " autocmd FileType html,eruby if g:html_indent_tags !~ '\\|p\>' | let g:html_indent_tags .= '\|p\|li\|dt\|dd' | endif

  " Don't syntax highlight markdown because it's often wrong
  autocmd! FileType mkd setlocal syn=off

  " Leave the return key alone when in command line windows, since it's used
  " to run commands there.
  autocmd! CmdwinEnter * :unmap <cr>
  autocmd! CmdwinLeave * :call MapCR()

  " My own (pierre jambet) settings
  autocmd BufWritePre * :%s/\s\+$//e
  autocmd User fugitive if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' | nnoremap <buffer> .. :edit %:h<CR> | endif
  autocmd BufReadPost fugitive://* set bufhidden=delete

  " Source the vimrc file after saving it
  " autocmd bufwritepost .vimrc source $MYVIMRC
  autocmd! BufWritePost .vimrc source %

  autocmd InsertEnter,InsertLeave * set cul!
  au BufLeave,FocusLost * silent! wa
  au BufRead,BufNewFile *.hamlc set ft=haml

  au FocusLost * :set number
  au FocusGained * :set relativenumber

  autocmd BufWritePre *.go Fmt
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" COLOR
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" :set t_Co=256 " 256 colors
" :set background=dark
" :color twilight
syntax enable
set background=dark
colorscheme solarized

":color grb256

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" STATUS LINE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set statusline=%<%f\ (%{&ft})\ %{fugitive#statusline()}%-4(%m%)%=%-19(%3l,%02c%03V%)
set statusline=%<%f\ [%{strlen(&fenc)?&fenc:'none'}]%h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
" let g:airline_left_sep = '▶'
" let g:airline_right_sep = '◀'
" let g:airline_powerline_fonts=1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MISC KEY MAPS
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>y "*y
" Move around splits with <c-hjkl>
" nnoremap <c-j> <c-w>j
" nnoremap <c-k> <c-w>k
" nnoremap <c-h> <c-w>h
" nnoremap <c-l> <c-w>l
" Insert a hash rocket with <c-l>
imap <c-l> <space>=><space>
" Can't be bothered to understand ESC vs <c-c> in insert mode
imap <c-c> <esc>
" Clear the search buffer when hitting return
function! MapCR()
  nnoremap <cr> :nohlsearch<cr>
endfunction
call MapCR()
nnoremap <leader><leader> <c-^>

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
" inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ARROW KEYS ARE UNACCEPTABLE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <Left> <Nop>
map <Right> <Nop>
map <Up> <Nop>
map <Down> <Nop>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OPEN FILES IN DIRECTORY OF CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>e :edit %%

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" RENAME CURRENT FILE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>n :call RenameFile()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" PROMOTE VARIABLE TO RSPEC LET
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! PromoteToLet()
  :normal! dd
  " :exec '?^\s*it\>'
  :normal! P
  :.s/\(\w\+\) = \(.*\)$/let(:\1) { \2 }/
  :normal ==
endfunction
:command! PromoteToLet :call PromoteToLet()
:map <leader>p :PromoteToLet<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" EXTRACT VARIABLE (SKETCHY)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! ExtractVariable()
    let name = input("Variable name: ")
    if name == ''
        return
    endif
    " Enter visual mode (not sure why this is needed since we're already in
    " visual mode anyway)
    normal! gv

    " Replace selected text with the variable name
    exec "normal c" . name
    " Define the variable on the line above
    exec "normal! O" . name . " = "
    " Paste the original selected text to be the variable value
    normal! $p
endfunction
vnoremap <leader>rv :call ExtractVariable()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" INLINE VARIABLE (SKETCHY)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! InlineVariable()
    " Copy the variable under the cursor into the 'a' register
    :let l:tmp_a = @a
    :normal "ayiw
    " Delete variable and equals sign
    :normal 2daW
    " Delete the expression into the 'b' register
    :let l:tmp_b = @b
    :normal "bd$
    " Delete the remnants of the line
    :normal dd
    " Go to the end of the previous line so we can start our search for the
    " usage of the variable to replace. Doing '0' instead of 'k$' doesn't
    " work; I'm not sure why.
    normal k$
    " Find the next occurence of the variable
    exec '/\<' . @a . '\>'
    " Replace that occurence with the text we yanked
    exec ':.s/\<' . @a . '\>/' . @b
    :let @a = l:tmp_a
    :let @b = l:tmp_b
endfunction
nnoremap <leader>ri :call InlineVariable()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MAPS TO JUMP TO SPECIFIC COMMAND-T TARGETS AND FILES
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <leader>gr :topleft :split config/routes.rb<cr>
function! ShowRoutes()
  " Requires 'scratch' plugin
  :topleft 100 :split __Routes__
  " Make sure Vim doesn't write __Routes__ as a file
  :set buftype=nofile
  " Delete everything
  :normal 1GdG
  " Put routes output in buffer
  :0r! rake -s routes
  " Size window to number of lines (1 plus rake output length)
  :exec ":normal " . line("$") . "_ "
  " Move cursor to bottom
  :normal 1GG
  " Delete empty trailing line
  :normal dd
endfunction
map <leader>gR :call ShowRoutes()<cr>
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
" SWITCH BETWEEN TEST AND PRODUCTION CODE
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenTestAlternate()
  let new_file = AlternateForCurrentFile()
  exec ':e ' . new_file
endfunction
function! AlternateForCurrentFile()
  let current_file = expand("%")
  let new_file = current_file
  let in_spec = match(current_file, '^spec/') != -1
  let going_to_spec = !in_spec
  let in_app = match(current_file, '\<controllers\>') != -1 || match(current_file, '\<models\>') != -1 || match(current_file, '\<views\>') != -1
  if going_to_spec
    if in_app
      let new_file = substitute(new_file, '^app/', '', '')
    end
    let new_file = substitute(new_file, '\.rb$', '_spec.rb', '')
    let new_file = 'spec/' . new_file
  else
    let new_file = substitute(new_file, '_spec\.rb$', '.rb', '')
    let new_file = substitute(new_file, '^spec/', '', '')
    if in_app
      let new_file = 'app/' . new_file
    end
  endif
  return new_file
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Md5 COMMAND
" Show the MD5 of the current buffer
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! -range Md5 :echo system('echo '.shellescape(join(getline(<line1>, <line2>), '\n')) . '| md5')

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" OpenChangedFiles COMMAND
" Open a split for each dirty file in git
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! OpenChangedFiles()
  only " Close all windows, unless they're modified
  let status = system('git status -s | grep "^ \?\(M\|A\)" | cut -d " " -f 3')
  let filenames = split(status, "\n")
  exec "edit " . filenames[0]
  for filename in filenames[1:]
    exec "sp " . filename
  endfor
endfunction
command! OpenChangedFiles :call OpenChangedFiles()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" InsertTime COMMAND
" Insert the current time
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
command! InsertTime :normal a<c-r>=strftime('%F %H:%M:%S.0 %z')<cr>

" Hide mvim toolbar if it exists
if has("gui_running")
    set guioptions=egmrt
endif


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Pierre jambet custom bindings :
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" call pathogen#helptags()

function! ToggleChars()

  exec "set list!"
endfunction

" Shortcut to rapidly toggle `set list`
nmap <leader>l :call ToggleChars()<CR>

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬,trail:~,extends:>,precedes:<,nbsp:•

"Invisible character colors
highlight NonText guifg=#4a4a59
highlight SpecialKey guifg=white guibg=#cc0000


" Bind a shortup to :edit $MYVIMRC
nmap <leader>r :edit $MYVIMRC<CR>

" Open coffe compile split vertically by default
let coffee_compile_vert = 1

function! MarkWindowSwap()
    let g:markedWinNum = winnr()
endfunction

function! DoWindowSwap()
    "Mark destination
    let curNum = winnr()
    let curBuf = bufnr( "%" )
    exe g:markedWinNum . "wincmd w"
    "Switch to source and shuffle dest->source
    let markedBuf = bufnr( "%" )
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' curBuf
    "Switch to dest and shuffle source->dest
    exe curNum . "wincmd w"
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' markedBuf
endfunction

nmap <silent> <leader>mw :call MarkWind wSwap()<CR>
nmap <silent> <leader>pw :call DoWindowSwap()<CR>


set wildignore+=*.o,*.obj,.git,node_modules,venv,*.pyc,tmp,vendor

" Highlight text over 80
highlight clear OverLength
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/
" An other option is to set the colorcolumn
set colorcolumn=80

" Save on focus lost


" autocmd BufWritePost *
"       \ if filereadable('tags') |
"       \   call system('ctags -a '.expand('%')) |
"       \ endif

" Switch hash syntax to 1.9
command! HashSyntax :%s/:\([^ ]*\)\(\s*\)=>/\1:/g

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Test-running stuff
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" map <Leader>t :call RunCurrentSpecFile()<CR>
" map <Leader>s :call RunNearestSpec()<CR>
" map <Leader>a :call RunAllSpecs()<CR>
" let g:rspec_command = 'call Send_to_Tmux("rspec {spec}\n")'

nmap <silent> <leader>sp :TestNearest<CR>
nmap <silent> <leader>t :TestFile<CR>
nmap <silent> <leader>a :TestSuite<CR>
nmap <silent> <leader>la :TestLast<CR>
nmap <silent> <leader>v :TestVisit<CR>

map <leader>csc :%s/;/<cr>
map <leader>g :GundoToggle<CR>
map <leader>d <Plug>TaskList
map <leader>k :RopeGotoDefinition<CR>
map <leader>j :RopeRename<CR>
map <leader>sv :vsplit<CR>
map <leader>sh :split<CR>


map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap e

" CtrlP (awesome!) settings
let g:ctrlp_max_height = 20

let g:gitgutter_enabled = 0

map $ g_
map ; :

" vim-gist config
let g:gist_post_private = 1
let g:gist_clip_command = 'pbcopy'

" Python mode settings
let g:pymode_lint = 1
let g:pymode_indent = 0
let g:pymode_lint_checker = "pep8"
let g:pymode_lint_ignore = "E501,E128,E122"

let g:pymode_folding = 0
let g:pymode_run = 1
" Key for run python code
let g:pymode_run_key = 'R'

let g:pymode_syntax_slow_sync = 0

set autoread

" Center screen after each bounce to new search result
nmap n nzz
nmap N Nzz

call togglebg#map("<F5>")

let current_day = strftime('%w')
let current_time = strftime('%H')
if current_day == 0 || current_day == 6 || current_time > "20" || current_time < "09"
  set bg=dark
else
  set bg=dark
endif

" map <C-A> gg V G
set guifont=Inconsolata-dz\ for\ Powerline:h11
set relativenumber

function! NumberToggle()
  if(&relativenumber == 1)
    set number
  else
    set relativenumber
  endif
endfunc

command! NumberToggle call NumberToggle()
" nnoremap <C-n> :call NumberToggle()<cr>

" Disable Ex mode
map Q <Nop>

" Get rid of the delay when hitting esc!
set noesckeys
" set timeoutlen=100
" set ttimeoutlen=100
" map <leader>dt :set makeprg=REUSEDB=1\ python\ manage.py\ test\ core\|:call MakeGreen()<CR>
imap <c-b> import pdb; pdb.set_trace()

let g:netrw_list_hide='^\.DS'

let g:localvimrc_whitelist = '/\(Users\|home\)/pierre/dev/\(harrys\|harrys-blog\).*'
let g:localvimrc_sandbox = 0
set re=1
set ttyfast
set lazyredraw

if has('nvim')
  nmap <BS> <C-W>h
endif

" vim-test
let test#strategy = "tslime"

vmap <C-c><C-c> <Plug>SendSelectionToTmux
nmap <C-c><C-c> <Plug>NormalModeSendToTmux
nmap <C-c>r <Plug>SetTmuxVars

" Enter accepts the current autocompletion instead of a new line
" from: http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
" :inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

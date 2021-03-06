" All system-wide defaults are set in $VIMRUNTIME/debian.vim and sourced by
" the call to :runtime you can find below.  If you wish to change any of those
" settings, you should do it in this file (/etc/vim/vimrc), since debian.vim
" will be overwritten everytime an upgrade of the vim packages is performed.
" It is recommended to make changes after sourcing debian.vim since it alters
" the value of the 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim
autocmd! BufWritePost ~/.vimrc source %

" Uncomment the next line to make Vim more Vi-compatible
" NOTE: debian.vim sets 'nocompatible'.  Setting 'compatible' changes numerous
" options, so any other options should be set AFTER setting 'compatible'.
set nocompatible

let mapleader=","

if has("syntax")
  syntax on
endif

" set nobackup
set writebackup
set autoread
set backupdir=~/.vimbackup
set backupcopy=auto
set noswapfile
set magic
set t_Co=256
" set cursorline   "hight current line
" set cursorcolumn
set novisualbell
set viminfo+=/100  "set the limit viminfo
set ttyfast
set ttyscroll=3
set lazyredraw
set pumheight=10
set gcr=a:block-blinkon0 "" 禁止光标闪烁

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set number          " show line number
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching
set incsearch       " Incremental search
set hlsearch
set autowrite       " Automatically save before commands like :next  and :make
set hidden          " Hide buffers when they are abandoned
set mouse+=a        " Enable mouse usage (all modes)
set bs=2            " make backspace behave like normal again

highlight Pmenu guibg=brown gui=bold

set listchars=tab:▸·,trail:·,precedes:<,extends:>
set list
set noendofline     " why?

nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
nnoremap <C-p> :CommandT<CR>

" emacs key
inoremap <leader>e <END>
inoremap <leader>a <HOME>
inoremap <C-n> <C-\><C-O>j
inoremap <C-p> <C-\><C-O>k
inoremap <C-B> <C-\><C-O>h
inoremap <C-F> <C-\><C-O>l

" autocmd! FileType javascript nnoremap <C-b> :!node %<CR>

" easier moving of code blocks
" Try to go into visual mode (v), thenselect several lines of code here and
" then press ``>`` several times.
vnoremap < <gv  " better indentation
vnoremap > >gv  " better indentation
:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>
:vnoremap <Tab> >gv

set fo+=tw   "auto wrap require formatoptions+=t"
set wrap linebreak
set showbreak=↩\ 
set textwidth=79  " width of document (used by gd)
set colorcolumn=80
highlight ColorColumn ctermbg=220 guifg=yellow
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
" highlight Pmenu term=reverse ctermbg=cyan ctermfg=black
" highlight PmenuSel term=reverse ctermbg=lightred ctermfg=black
" YCM 补全菜单配色
" 菜单
highlight Pmenu ctermfg=2 ctermbg=3 guifg=#005f87 guibg=#EEE8D5
" 选中项
highlight PmenuSel ctermfg=2 ctermbg=3 guifg=#AFD700 guibg=#106900


highlight ExtraWhitespace ctermbg=darkgreen ctermfg=white
match ExtraWhitespace /\s\+$/
match ExtraWhitespace /[^\t]\zs\t\+/

" Real programmers don't use TABs but spaces
set cindent
set smartindent
set sw=4
set sts=4
set ts=4
set expandtab
set pastetoggle=<F2>
autocmd FileType html,json,yaml setlocal shiftwidth=2 tabstop=2 sts=2 expandtab

if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" backup plugin 
" Plug 'easymotion/vim-easymotion'
" Plug 'The-NERD-Commenter'
" Plug 'marijnh/tern_for_vim'
" Plug 'lambdatoast/elm.vim'
" Plug 'reedes/vim-pencil'
" Plug 'fatih/vim-go'
" Plug 'majutsushi/tagbar'
" Plug 'easymotion/vim-easymotion'
" Plug 'Shougo/vimproc.vim'
" Plug 'Shougo/vimshell.vim'
" Plug 'sjl/gundo.vim'
" Plug 'fholgado/minibufexpl.vim'
" Plug 'sheerun/vim-polyglot'

call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'bling/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'Auto-Pairs'
Plug 'Valloric/YouCompleteMe'
Plug 'scrooloose/nerdtree'      "文件浏览
Plug 'mattn/emmet-vim'
Plug 'kien/ctrlp.vim'
Plug 'editorconfig/editorconfig-vim'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'moll/vim-node'
Plug 'yegappan/grep'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'chrisgillis/vim-bootstrap3-snippets'
Plug 'maksimr/vim-jsbeautify'
Plug 'Yggdroot/indentLine'
Plug 'suan/vim-instant-markdown'
Plug 'mileszs/ack.vim'
Plug 'heavenshell/vim-jsdoc'
Plug 'Shougo/vimshell.vim'
Plug 'Shougo/vimproc.vim'
Plug 'briancollins/vim-jst'
Plug 'scrooloose/syntastic'
Plug 'elzr/vim-json'
Plug 'nvie/vim-flake8'
call plug#end()            " required


" tagbar
nmap <F3> :NERDTreeToggle<CR>
" let g:NERDTreeChDirMode=2
" let g:NERDTreeDirArrows = 1
" let g:NERDTreeQuitOnOpen = 1


" commenter
let g:NERDSpaceDelims=1       " 让注释符与语句之间留一个空格
let g:NERDCompactSexyComs=1   " 多行注释时样子更好看

" jedi and YCM config
autocmd FileType python,javascript setlocal completeopt=longest,menuone
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
inoremap <Tab> <C-x><C-o>
let g:ycm_confirm_extra_conf=0
set completeopt-=preview " show the preview window
let g:ycm_add_preview_to_completeopt=0
let g:ycm_seed_identifiers_with_syntax=1
" let g:ycm_complete_in_comments = 1
" let g:ycm_complete_in_strings = 1
"注释和字符串中的文字也会被收入补全
" let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_cache_omnifunc=0
let g:ycm_min_num_of_chars_for_completion=1
" let g:ycm_collect_identifiers_from_tags_files=1

" powerline
set laststatus=2

""""""""""""""""""""""""""""""
" airline
""""""""""""""""""""""""""""""
let g:airline_enable = 1
let g:airline_detect_paste=1
let g:airline_mode_map = {
  \ '__' : '-',
  \ 'n'  : 'N',
  \ 'i'  : 'I',
  \ 'R'  : 'R',
  \ 'c'  : 'C',
  \ 'v'  : 'V',
  \ 'V'  : 'V',
  \ '' : 'V',
  \ 's'  : 'S',
  \ 'S'  : 'S',
  \ '' : 'S',
  \ }

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = '▶'
let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#bufferline#left_sep = '▶'
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#virtualenv#enabled = 1
let g:airline_left_sep='▶'
let g:airline_left_alt_sep='▶'
let g:airline_right_sep='◀'
let g:airline_right_alt_sep='◀'

if !exists('g:airline_symbols')
      let g:airline_symbols = {}
endif
let g:airline_symbols.space = "\ua0"

let g:airline_theme             = 'wombat'

let g:syntastic_always_populate_loc_list = 0
let g:syntastic_check_on_w = 1
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_python_checkers = ['flake8']

" tern.js setting
" let g:tern#is_show_argument_hints_enabled=1
" let g:tern_show_argument_hints='on_hold'
" let g:tern_map_keys=1

"javascript
let g:javascript_enable_domhtmlcss = 1

" markdown setting
let g:vim_markdown_folding_disabled=1
let g:vim_markdown_frontmatter=1

" MiniBufExpl Colors
let g:miniBufExplorerAutoStart = 1
let g:miniBufExplBuffersNeeded = 1
let g:miniBufExplStatusLineText = "-已打开文件-"
" let g:miniBufExplHideWhenDiff = 1
" let g:miniBufExplMinSize = 25
" let g:miniBufExplMaxSize = 30
" let g:miniBufExplVSplit = 1
" let g:miniBufExplBRSplit = 1

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<C-e>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" ctrl_p
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)|(node_modules)$',
  \ }

" auto-save
" let g:auto_save = 1
" let g:auto_save_in_insert_mode = 0  "do not save while in insert mode

" indentLine
let g:indentLine_color_term = 239
let g:indentLine_char = '|'

" gitgutter
let g:gitgutter_sign_added = 'A'
let g:gitgutter_sign_modified = 'M'
let g:gitgutter_sign_removed = 'R'
let g:gitgutter_sign_removed_first_line = '^^'
let g:gitgutter_sign_modified_removed = 'MR'

"html
let g:user_emmet_install_global = 0
autocmd FileType html,css,jst,jsx,js,javascript EmmetInstall
let g:syntastic_html_tidy_exec = 'tidy'
let g:syntastic_always_populate_loc_list = 1
let g:use_emmet_complete_tag = 1
let g:user_emmet_leader_key='<C-l>'
let g:user_emmet_mode='a'
let g:emmet_indent_size = 2
let g:emmet_html5=1

" web group
augroup web
    autocmd!
    autocmd FileType javascript noremap <buffer><leader><c-f> :call JsBeautify()<cr>
    autocmd FileType html noremap <buffer><leader><c-f> :call HtmlBeautify()<cr>
    autocmd FileType css noremap <buffer><leader><c-f> :call CSSBeautify()<cr>
    autocmd FileType javascript vnoremap <buffer><leader>  <c-f> :call RangeJsBeautify()<cr>
    autocmd FileType html vnoremap <buffer><leader><c-f> :call RangeHtmlBeautify()<cr>
    autocmd FileType css vnoremap <buffer><leader><c-f> :call RangeCSSBeautify()<cr>
augroup END

" golang group
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

if has("gui_running")
    colorscheme solarized
    " colorscheme monokai-soda
    set bg=dark
    highlight ColorColumn ctermbg=220 guibg=tan
    set guifont=Menlo\ Bold
    set guioptions-=T
    set t_Co=256
    set lines=50 columns=180
    set guioptions-=r
    set guioptions-=L
    highlight Pmenu ctermfg=2 ctermbg=3 guifg=#005f87 guibg=#EEE8D5
    highlight PmenuSel ctermfg=2 ctermbg=3 guifg=#AFD700 guibg=#106900
    set pumheight=10
else
    colorscheme slate
    set bg=light
endif


"ultisnips setting
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

"for search
let g:ackprg = 'ag --nogroup --nocolor --column'
let g:tern_request_timeout = 100


"set scheme
let g:solarized_termcolors=256
set enc=utf-8
setglobal fileencoding=utf-8
set wildignore+=/bower_components/*,/node_modules/*

" set no delay 
set timeoutlen=300 ttimeoutlen=0

"jsdoc setting
let g:jsdoc_additional_descriptions = 1
let g:jsdoc_input_description = 1
let g:jsdoc_allow_input_prompt = 1

highlight Cursor guifg=white guibg=green

let g:vim_json_syntax_conceal = 0

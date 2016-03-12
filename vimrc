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

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
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
set lazyredraw
colorscheme wombat256mod
set bg=dark

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
let mapleader=","
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
imap <C-e> <END>
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l

autocmd! FileType javascript nnoremap <C-b> :!node %<CR>

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
set textwidth=79  " width of document (used by gd)
set colorcolumn=80
highlight ColorColumn ctermbg=220
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
highlight Pmenu term=reverse ctermbg=cyan ctermfg=black
highlight PmenuSel term=reverse ctermbg=lightred ctermfg=black

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
" 'easymotion/vim-easymotion'
" 'The-NERD-Commenter'
call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-fugitive'
Plug 'gregsexton/gitv'
Plug 'airblade/vim-gitgutter'
Plug 'Auto-Pairs'
Plug 'Valloric/YouCompleteMe'
Plug 'scrooloose/nerdtree'      "文件浏览
Plug 'bling/vim-airline'
Plug 'mattn/emmet-vim'
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/syntastic'
Plug 'editorconfig/editorconfig-vim'
Plug 'marijnh/tern_for_vim'
Plug 'scrooloose/nerdcommenter'
Plug 'tpope/vim-surround'
Plug 'fholgado/minibufexpl.vim'
Plug 'moll/vim-node'
Plug 'yegappan/grep'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'chrisgillis/vim-bootstrap3-snippets'
Plug 'sheerun/vim-polyglot'
Plug 'fatih/vim-go'
Plug 'maksimr/vim-jsbeautify'
Plug 'lambdatoast/elm.vim'
Plug 'Yggdroot/indentLine'
Plug 'majutsushi/tagbar'
Plug 'reedes/vim-pencil'
Plug 'easymotion/vim-easymotion'
call plug#end()            " required

" tagbar
nmap <F3> :NERDTreeToggle<CR>
let g:NERDTreeChDirMode=2
let g:NERDTreeDirArrows = 1
" let g:NERDTreeQuitOnOpen = 1

" commenter
let g:NERDSpaceDelims=1       " 让注释符与语句之间留一个空格
let g:NERDCompactSexyComs=1   " 多行注释时样子更好看

" jedi and YCM config
autocmd FileType python,javascript setlocal completeopt=longest
set pumheight=10
" let g:jedi#use_tabs_not_buffers = 0
" let g:jedi#popup_on_dot = 1
" let g:jedi#popup_select_first = 1
" let g:jedi#documentation_command = "K"
" let g:jedi#rename_command = "<leader>r"
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
inoremap <Tab> <C-x><C-o>
let g:ycm_confirm_extra_conf=0
set completeopt-=preview " show the preview window
let g:ycm_add_preview_to_completeopt=0
let g:ycm_seed_identifiers_with_syntax=1
let g:ycm_complete_in_comments = 1
let g:ycm_complete_in_strings = 1
"注释和字符串中的文字也会被收入补全
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_cache_omnifunc=1
let g:ycm_min_num_of_chars_for_completion=2
let g:ycm_collect_identifiers_from_tags_files=1

" powerline
set laststatus=2

""""""""""""""""""""""""""""""
" airline
""""""""""""""""""""""""""""""
let g:airline_enable = 1
let g:airline_left_sep = '▶'
let g:airline_left_alt_sep = '▶'
let g:airline_right_sep = '◀'
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
let g:airline#extensions#bufferline#enabled = 1
let g:airline#extensions#tagbar#enabled = 1
let g:airline#extensions#virtualenv#enabled = 1
if !exists('g:airline_symbols')
      let g:airline_symbols = {}
endif
" let g:airline_theme             = 'wombat'
" let g:airline_theme             = 'papercolor'
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_check_on_w = 1
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_python_checkers = ['pyflakes']


"html css
let g:user_emmet_install_global = 0

" let g:tern#is_show_argument_hints_enabled=1
let g:tern_show_argument_hints='on_hold'
let g:tern_map_keys=1

"javascript
let g:javascript_enable_domhtmlcss = 1

" markdown setting
let g:vim_markdown_folding_disabled=1
let g:vim_markdown_frontmatter=1

" MiniBufExpl Colors
" let g:miniBufExplHideWhenDiff = 1
" let g:miniBufExplMinSize = 25
" let g:miniBufExplMaxSize = 30
" let g:miniBufExplVSplit = 1
" let g:miniBufExplBRSplit = 1
" let g:miniBufExplStatusLineText = "-已打开文件-"

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<C-e>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" ctrl_p
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
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
autocmd FileType html,css EmmetInstall
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
    autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
    autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
    autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>
    autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
    autocmd FileType html vnoremap <buffer> <c-f> :call RangeHtmlBeautify()<cr>
    autocmd FileType css vnoremap <buffer> <c-f> :call RangeCSSBeautify()<cr>
augroup END

" golang group
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1


" vim pencil
let g:pencil#wrapModeDefault = 'soft'
let g:pencil#autoformat = 1
let g:pencil#textwidth = 80

augroup pencil
    autocmd!
    autocmd FileType markdown,mkd call pencil#init()
    autocmd FileType text         call pencil#init({'wrap': 'hard'})
augroup END

if has("gui_running")
    set guifont=Monaco:h14
    set guioptions-=T
    set t_Co=256
    set lines=42 columns=180
endif

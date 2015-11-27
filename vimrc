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
set backupdir=~/.vimbackup
set writebackup
set backupcopy=auto

set noswapfile
set magic
set t_Co=256
set cursorline   "hight current line
set cursorcolumn 
set bg=dark
color wombat256mod
" color synic
set lazyredraw
set novisualbell

if has('gui_running')
  set guifont=Inconsolata\ Bold\ 12
  color molokai
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
let mapleader=","
set nocompatible    " be iMproved, required
set number          " show line number
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching
set incsearch       " Incremental search
set hlsearch
set autowrite       " Automatically save before commands like :next         and :make
set hidden          " Hide buffers when they are abandoned
set mouse=a         " Enable mouse usage (all modes)
set bs=2            " make backspace behave like normal again

highlight Pmenu guibg=brown gui=bold

set listchars=tab:»·,trail:·,precedes:<,extends:>
set list


nnoremap <leader>n :bn<cr>
nnoremap <leader>p :bp<cr>
imap <C-e> <END>
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l

" easier moving of code blocks
" Try to go into visual mode (v), thenselect several lines of code here and
" then press ``>`` several times.
vnoremap < <gv  " better indentation
vnoremap > >gv  " better indentation

:nnoremap <Tab> :bnext<CR>
:nnoremap <S-Tab> :bprevious<CR>

:vnoremap <Tab> >gv



set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=210
highlight Pmenu term=reverse ctermbg=cyan ctermfg=black
highlight PmenuSel term=reverse ctermbg=lightred ctermfg=black

" Real programmers don't use TABs but spaces
set cindent
set smartindent
set expandtab
set sw=4
set sts=4
set ts=4
set pastetoggle=<F2>
set nowrap
autocmd FileType html,json,yaml setlocal shiftwidth=2 tabstop=2 sts=2

filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'Auto-Pairs'
Plugin 'Valloric/YouCompleteMe'
Plugin 'davidhalter/jedi'
Plugin 'scrooloose/nerdtree'      "文件浏览
Plugin 'bling/vim-airline'
Plugin 'mattn/emmet-vim'
Plugin 'The-NERD-Commenter'
Plugin 'pangloss/vim-javascript'

"tools 
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/syntastic'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'majutsushi/tagbar'
Plugin 'marijnh/tern_for_vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-surround'
Plugin 'fholgado/minibufexpl.vim'
Plugin 'vim-scripts/vim-auto-save'
Plugin 'Yggdroot/indentLine'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'mklabs/grunt.vim'
" Plugin 'geekjuice/vim-mocha'

" Track the engine.
Plugin 'SirVer/ultisnips'
" " Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'
Plugin 'chrisgillis/vim-bootstrap3-snippets'
" "

"filetype
Plugin 'elzr/vim-json'
Plugin 'jade.vim'
Plugin 'plasticboy/vim-markdown'
Plugin 'fatih/vim-go'

" js beautify
Plugin 'maksimr/vim-jsbeautify'
Plugin 'einars/js-beautify'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" tagbar
nmap <F8> :TagbarToggle<CR>
nmap <F3> :NERDTreeToggle<CR>
nmap <silent><leader>t :NERDTreeToggle<CR>
let g:NERDTreeChDirMode=2
" commenter
let g:NERDSpaceDelims=1       " 让注释符与语句之间留一个空格
let g:NERDCompactSexyComs=1   " 多行注释时样子更好看


" jedi and YCM config
autocmd FileType python,javascript setlocal completeopt=longest
set pumheight=10
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
let g:jedi#documentation_command = "K"
let g:jedi#rename_command = "<leader>r"
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
inoremap <Tab> <C-x><C-o>
let g:ycm_confirm_extra_conf=0
set completeopt-=preview
let g:ycm_add_preview_to_completeopt=0
let g:ycm_seed_identifiers_with_syntax=1    
let g:ycm_complete_in_comments = 1
let g:ycm_complete_in_strings = 1
"注释和字符串中的文字也会被收入补全
let g:ycm_collect_identifiers_from_comments_and_strings = 0
let g:ycm_cache_omnifunc=0
let g:ycm_min_num_of_chars_for_completion=2
let g:ycm_collect_identifiers_from_tags_files=1

" powerline
set laststatus=2
" set fillchars+=stl:\ ,stlnc:\
" let g:Powerline_symbols='unicode'

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
let g:airline_theme             = 'wombat'

" syntax check plugin
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_check_on_w = 1
let g:syntastic_javascript_checkers = ['jshint']
let g:syntastic_python_checkers = ['pyflakes']


"html css
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
let g:use_emmet_complete_tag = 1
let g:user_emmet_leader_key='<C-l>'
let g:user_emmet_mode='a'
let g:emmet_indent_size = 2
let g:emmet_html5=1

" let g:tern#is_show_argument_hints_enabled=1
let g:tern_show_argument_hints='on_hold'
let g:tern_map_keys=1

"javascript
let g:javascript_enable_domhtmlcss = 1

" markdown setting
let g:vim_markdown_folding_disabled=1
let g:vim_markdown_frontmatter=1

"js beautify
map <c-f> :call JsBeautify()<cr>
" or
autocmd FileType javascript noremap <buffer>  <c-f> :call JsBeautify()<cr>
autocmd FileType html noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType css noremap <buffer> <c-f> :call CSSBeautify()<cr>

autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
autocmd FileType html vnoremap <buffer> <c-f> :call RangeHtmlBeautify()<cr>
autocmd FileType css vnoremap <buffer> <c-f> :call RangeCSSBeautify()<cr>

" setting for go
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

" MiniBufExpl Colors
hi MBENormal               guifg=#808080 guibg=fg
hi MBEChanged              guifg=#CD5907 guibg=fg
hi MBEVisibleNormal        guifg=#5DC2D6 guibg=fg
hi MBEVisibleChanged       guifg=#F1266F guibg=fg
hi MBEVisibleActiveNormal  guifg=#A6DB29 guibg=fg
hi MBEVisibleActiveChanged guifg=#F1266F guibg=fg

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<C-e>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" ctrl_p
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\v[\/]\.(git|hg|svn)$',
  \ 'file': '\v\.(exe|so|dll)$',
  \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
  \ }

" auto-save
let g:auto_save = 0
let g:auto_save_in_insert_mode = 0  "do not save while in insert mode

" indentLine
let g:indentLine_color_term = 239
let g:indentLine_char = '|'


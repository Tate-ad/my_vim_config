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

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
"set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
"if has("autocmd")
"  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
"endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
"if has("autocmd")
"  filetype plugin indent on
"endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
" set showcmd		" Show (partial) command in status line.
" set showmatch		" Show matching brackets.
" set ignorecase		" Do case insensitive matching
" set smartcase		" Do smart case matching
" set incsearch		" Incremental search
" set autowrite		" Automatically save before commands like :next and :make
" set hidden		" Hide buffers when they are abandoned
" set mouse=a		" Enable mouse usage (all modes)

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif
set nobackup
set nowritebackup
set noswapfile

set bg=dark
set t_Co=256
set cursorline
set cursorcolumn 
color wombat256mod

let mapleader=","
set nocompatible    " be iMproved, required
set number          " show line number
set showcmd         " Show (partial) command in status line.
set showmatch       " Show matching brackets.
set ignorecase      " Do case insensitive matching
set smartcase       " Do smart case matching
set incsearch       " Incremental search
set autowrite       " Automatically save before commands like :next         and :make
set hidden          " Hide buffers when they are abandoned
set mouse+=a         " Enable mouse usage (all modes)
set bs=2            " make backspace behave like normal again

" for search 
set hlsearch

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



set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=100

" Real programmers don't use TABs but spaces
set cindent
set smartindent
set expandtab
set sw=4
set sts=4
set ts=4
set pastetoggle=<F2>
set nowrap
autocmd FileType html,json,yaml setlocal shiftwidth=2 tabstop=2


filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'Auto-Pairs'
Plugin 'AutoComplPop'
Plugin 'Valloric/YouCompleteMe'
Plugin 'davidhalter/jedi'
Plugin 'scrooloose/nerdtree'      "文件浏览
Plugin 'Lokaltog/vim-powerline'   "状态栏美化
Plugin 'mattn/emmet-vim'
Plugin 'ctrlp.vim' 
Plugin 'jsbeautify'
Plugin 'The-NERD-Commenter'
Plugin 'surround.vim'
Plugin 'marijnh/tern_for_vim'
Plugin 'majutsushi/tagbar'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'scrooloose/syntastic'
Plugin 'JSON.vim'
Plugin 'yaml.vim'
Plugin 'Markdown'
Plugin 'html5.vim'
Plugin 'Yggdroot/indentLine'
Plugin 'Markdown-syntax'
    " All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" tagbar
nmap <F8> :TagbarToggle<CR>
nmap <silent><leader>t :NERDTreeToggle<CR>
nmap <silent><leader>bn :bn<CR>
nmap <silent><leader>bp :bp<CR>


" jedi and YCM config
autocmd FileType python setlocal completeopt=longest
set pumheight=10
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 1
let g:jedi#popup_select_first = 1
let g:jedi#documentation_command = "K"
let g:jedi#rename_command = "<leader>r"
nnoremap <leader>g :YcmCompleter GoToDefinitionElseDeclaration<CR>
inoremap <Tab> <C-x><C-o>
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
set fillchars+=stl:\ ,stlnc:\
let g:Powerline_symbols='unicode'

" syntax check plugin
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['jshint']

" indentLine
let g:indentLine_char=">"
let g:indentLine_fileType = ['c', 'cpp', 'javascript', 'python', 'html']
let g:indentLine_color_term = 120

"html css
let g:user_emmet_leader_key='<C-l>'
let g:user_emmet_mode='a'
let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

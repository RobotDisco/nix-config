" When started as "evim", evim.vim will already have these settings
if v:progname =~? "evim"
  finish
endif

set backup " We like backups

" CTRL-U in insert mode deletes a lot. Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
" inoremap <C-U> <C-G>u<C-U>

set nocompatible "Use Vim defaults instead of 100% vi compatibility
set backspace=indent,eol,start  " more powerful backspacing.

" Now we set some defaults for the editor
set history=50  " keep 50 lines of command line history
set ruler " show the curser position all of the time.

" modeliens have historically been a source of security/resource
" vulnerabilities -- disable by default, even when 'nocompatible' is set
set nomodeline

" Suffixes that get lower priority when doing tab completion for filenames.
" These are files that we are not likely to want to edit or read.
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brg,.cb,.ind,.idx,.ilg,.inx,.out,.toc

" We know xterm-debian is a color terminal
if &term =~ "xterm-debian" || &term =~ "xterm-xfree86"
  set t_Co=16
  set t_Sf=[3%dm
  set t_Sb=[4%dm
endif

" Uncomment the next line to make Vim more Vi-compatible
" NOTE: debian.vim sets 'nocompatible'.  Setting 'compatible' changes numerous
" options, so any other options should be set AFTER setting 'compatible'.
"set compatible

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" Also, prettify my user interface. Tee hee, LOTR or something.
colorscheme desert

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on

  " Put these in an autocmd group, so we can delete them easily.
  augroup vimrcEx
  au!

  " Uncomment the following to have Vim jump to the last position when
  " reopening a file
  autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
  
  " For all text files set 'textwidth to 78 characters.
  autocmd FileType text setlocal textwidth=78

  augroup END
else
  set autoindent
endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd			" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase	" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set hlsearch		" Highlight seach terms
"set autowrite		" Automatically save before commands like :next and :make
"set hidden     " Hide buffers when they are abandoned
if has('mouse')
  set mouse=a		" Enable mouse usage (all modes) in terminals that support it.
	set mousehide " Hide the mouse when typing text
	
  " Make shift-insert work like in Xterm
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
endif

" Source a global configuration file if available
" XXX Deprecated, please move your changes here in /etc/vim/vimrc
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

" I prefer mod-2 indents.
set tabstop=2
set shiftwidth=2
set softtabstop=2
set noexpandtab
set copyindent " For the love of God, preserve the previous line's indent rules

" Perforce
let g:p4Presets = 'p4proxy.waterloo.bluecoat.com:1666 gaelan_4_2 gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_4_3 gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_5_2 gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_5_3 gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_5_4 gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_legacy gaelan.dcosta,
			\p4proxy.waterloo.bluecoat.com:1666 gaelan_scorpius gaelan.dcosta'





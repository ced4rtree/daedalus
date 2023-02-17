let &packpath = &runtimepath
source ~/.vimrc
filetype detect

" Gives a cool color set
runtime colors/herald.vim

call plug#begin()
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

" The following stuff is coc configuration
let g:my_coc_file_types = ['c', 'cpp', 'h', 'hpp', 'vim', 'lua', 'py', 'java', 'sh']

function! s:disable_coc_for_type()
	if index(g:my_coc_file_types, &filetype) == -1
		let b:coc_enabled = 0
	endif
endfunction

augroup CocGroup
	autocmd!
	autocmd BufNew,BufEnter * call s:disable_coc_for_type()
augroup end

set updatetime=300
set signcolumn=yes

inoremap <silent><expr> <TAB>
	\ coc#pum#visible() ? coc#pum#next(1) :
	\ CheckBackspace() ? "\<Tab>" :
	\ coc#refresh()
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
							  \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
function! CheckBackspace() abort
	let col = col('.') - 1
	return !col || getline('.')[col - 1] =~# '\s'
endfunction

inoremap <silent><expr> <c-space> coc#refresh()

" The following stuff is treesitter configuration
lua << EOF
	require'nvim-treesitter.configs'.setup {
		ensure_installed = { "cpp", "c", "java" },

		sync_install = false,

		auto_install = true,

		ignore_install = { "*" },

		highlight = {
			enable = true,

			disable = function(lang, buf)
				local max_filesize = 100 * 1024 -- 100kb
				local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
				if ok and stats and stats.size > max_filesize then
					return true
				end
			end,

			additional_vim_regex_highlighting = false,
		},
	}
EOF

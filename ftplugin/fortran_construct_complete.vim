
" Exit quickly when:
" - this plugin was already loaded
if !exists("g:fortran_construct_complete_loaded")
    python3 << EOF
import sys, vim
sys.path.append(vim.eval("expand('<sfile>:p:h')")  + '/python/')
EOF
    python3 from fortran_construct_complete import do_complete_fortran_statement

    function! s:do_complete_fortran_statement()
        python3 do_complete_fortran_statement()
        return ''
    endfunction

    function! s:cr_complete_fortran_statement()
        if py3eval('do_complete_fortran_statement()')
            return ''
        else
            return "\<CR>"
        endif
    endfunction
    let g:fortran_construct_complete_loaded = 1
endif
inoremap <buffer> <CR> <C-g>u<C-r>=<SID>cr_complete_fortran_statement()<CR>

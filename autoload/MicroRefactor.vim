function! MicroRefactor#LocalVariable() " {{{
	" what's our filetype
  let l:pseudoFT = <SID>GetFiletype()
endfunction " }}}

function! <SID>GetFiletype()
	let l:pseudoFT = &l:ft
	if l:pseudoFT == 'php'
		" it might be javascript!
		if synIDattr(synID(line('.'), col('.'), 0), 'name') =~? '^j\%(s\|avascript\)'
			let l:pseudoFT = 'javascript'
		endif
  elseif l:pseudoFT == 'jsx'
    " use 'javascript' features for JSX files
    let l:pseudoFT = 'javascript'
	endif
  return l:pseudoFT
endfunction

" vim: sw=2 et ts=2 sts=2 foldmethod=marker

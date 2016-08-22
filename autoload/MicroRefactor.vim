function! MicroRefactor#LocalVariable() " {{{
	" what's our filetype
  let l:pseudoFT = <SID>GetFiletype()
  call <SID>SpecialOpUseVariableStart(l:pseudoFT)
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

function! <SID>SpecialOpUseVariableStart(pseudoFT) " {{{
	" if the visual selection spanned multiple lines, we need to abort
	let l:visStart = getpos("'<")
	let l:visEnd = getpos("'>")
	if l:visStart[1] != l:visEnd[1]
		echoerr 'Visual selection cannot span multiple lines'
		return
	endif

	" if the end of the visual area is past the end of the line (that's possible
	" in visual mode), move it back to the end of the line.
	let l:line = getline('.')
	let l:linelen = strlen(l:line)
	if l:visEnd[2] > l:linelen
		" Note that setpos() is unable to move the '> mark, so we need to do this
		" the hard way: go to visual start position, select to end of line, and
		" yank
		execute 'normal! v$hy'
		" get the new end position
		let l:visEnd = getpos("'>")
		let l:selectToEnd = 1
	elseif l:visEnd[2] == l:linelen
		" if the visual selection was to the end of the line, remember this as it
		" affects our cursor position later when we cut the visual visual
		let l:selectToEnd = 1
	else
		let l:selectToEnd = 0
	endif

	" default is to strip whitespace from value ...
	let l:valueTrim = ''
  let l:varDecl = ''
  " a regex to determine if the current line is part of a group of lines
  let l:currLineNotWhole = ''
  " a regex to determine if the previous line was not a complete statement
  let l:prevLineNotFinished = ''
	if a:pseudoFT == 'vim'
		let l:varMatch = '[glsbwta]:\h\w*'
		let l:lineIsCommentMatch = '^\s*"'
    let l:currLineNotWhole = '^\s*\\'
		let l:varLineAssignsMatch = '^\s*let\s\+%s\s*=\s*%s\s*$'
	elseif a:pseudoFT == 'sql'
		let l:varMatch = '@\h\w*'
		let l:lineIsCommentMatch = '^\s*#'
		let l:varLineAssignsMatch = '^\s*set\s\+%s\s*=\s*%s\s*;\=\s*$'
	elseif a:pseudoFT == 'javascript'
		let l:varMatch = '[$A-Za-z_][$A-Za-z0-9_]*'
		let l:lineIsCommentMatch = '^\s*//'
		let l:varLineAssignsMatch = '^\s*\%(\%(var\|let\|const\)\s\+\)\=%s\s*=\s*%s\s*;\=\s*$'
    let l:varDecl = 'var '
    "let l:prevLineNotFinished  = '[[({,.+-*/|&<>=?:]\s*$'
    "let l:currLineNotWhole = '^\s*[])},.+-*/|&<>=?:]'
	elseif a:pseudoFT == 'python'
		let l:varMatch = '\h\w*'
		let l:lineIsCommentMatch = '^\s*#'
		let l:varLineAssignsMatch = '^\s*%s\s*=\s*%s\s*\%(#.*\)\=$'
    "let l:prevLineNotFinished  = '[[({,.+-*/|&<>=]\s*$'
    "let l:currLineNotWhole = '^\s*[])},.+-*/|&<>=]'
	else
		" PHP
		let l:varMatch = '\$\h\w*'
		let l:lineIsCommentMatch = '^\%(/\*\*/\)\=\s*\%(//.*\)\=$'
		let l:varLineAssignsMatch = '^\%(/\*\*/\)\=\s*%s\s*=\s*%s\s*;\=$'
		" strip leading/trailing whitespace and semicolon from the value
		let l:valueTrim = '^\s\+\|\s*\%(;\s*\)\=$'
	endif

	" if the selection was a variable name, swap in the value of that
	" variable instead
	if @" =~? ('^' . l:varMatch . '$') " {{{
		let l:prevLine = line('.') - 1

		" do we need to confirm the line number?
		let l:confirmLine = 0

		while l:prevLine > 0 " {{{
			let l:prevLineData = getline(l:prevLine)

			" if the previous line is blank, or just has a comment, skip over it
			" a regular line of code, check the line before it instead
      if <SID>LineIsBlank(a:pseudoFT, l:prevLineData)
						\ || l:prevLineData =~ '^\s*\w\+\s*('
				let l:prevLine -= 1
				continue
			endif

			" does the line set the variable name?
			let l:find = printf(l:varLineAssignsMatch, escape(@", '\$'), '\zs.\{-}\ze')
			let l:expr = matchstr(l:prevLineData, l:find)
				
			" if that's the case, then we can insert that value
			if strlen(l:expr) " {{{
				" but do we need to confirm that we got the right expression?
				if l:confirmLine
							\ && 1 != confirm("Is this the correct expression?\n" . l:prevLineData, "&Yes\n&No", 0, 'Question')
					break
				endif

				execute 'normal! mtgvs' . l:expr

				" delete the line where the variable was defined
				execute l:prevLine . 'delete'

				" if the lines above and below where the variable was defined were
				" BOTH empty, then we need to remove one of them
				if getline(l:prevLine) =~ '^\s*$' && getline(l:prevLine - 1) =~ '^\s*$'
					execute l:prevLine . 'delete'
				endif

				" jump back to where we were
				normal! `t
				return
			endif " }}}

			" if the line of code is assigning a different variable, skip over it
			if l:prevLineData =~ '^\s*\$\h'
				let l:prevLine -= 1
				continue
			endif

			" We've come to a non-blank line that doesn't match, so we'll need to
			" confirm the replacement if ever we do find it
			let l:confirmLine = 0
			let l:prevLine -= 1
		endwhile " }}}

		" We couldn't find the variable definition, could we?
		echoerr printf("Couldn't find where %s is defined", @")
		return
	endif " }}}

	" if the select begins with an assignment to a variable, then just
	" use that variable name (and move it to the line above)
	let l:match = matchlist(@", '^\('.l:varMatch.'\)\s*=\s*\(.*\)$')
	if len(l:match) " {{{
		" replace with just the variable name
		execute 'normal! gvs' . l:match[1]

		" put the assignment on the line above
		let l:value = l:match[2]
		" trim the value
		if strlen(l:valueTrim)
			let l:value = substitute(l:value, l:valueTrim, '', 'g')
		endif
		execute 'normal! mtyyp`tjmtkS' . l:varDecl . l:match[1] . ' = ' . l:value . ";\<ESC>`t"

		return
	endif " }}}

	" look back 10? 20? lines to see if we can find a variable with an empty
	" assignment
	let l:look = line('.')
	let l:stop = l:look - 20
	let l:matchthis = printf(l:varLineAssignsMatch, '\zs' . l:varMatch . '\ze', '')
	let l:var = ''
	while l:look > l:stop
		let l:look -= 1
		let l:checkthis = getline(l:look)
		if l:checkthis =~? l:matchthis
			" wow! we found an empty variable assignment! let's use it!
			let l:var = matchstr(l:checkthis, l:matchthis)
			break
		endif
	endwhile

	" if we have a variable, swap it in
	if strlen(l:var)
		execute 'normal! gvs' . l:var
		if ! l:selectToEnd
			" this is needed to get the cursor in the right place ...
			normal! l
		endif
	else
		normal! gvx
	endif

	" OK, the user wants to replace the expression in the visual area with
	" a variable name instead: set up an autocmd to automatically add the
	" variable in the previous line once we are finished
	augroup SpecialOpUseVariable
	autocmd!
	" what is our value to insert?
	let l:value = @"
	if strlen(l:valueTrim)
		let l:value = substitute(l:value, l:valueTrim, '', 'g')
	endif
	" we need to escape the value so it makes it into the autocommand safely
	let l:quotedValue = substitute(l:value, "'", "''", 'g')
	execute printf("autocmd InsertLeave * call <SID>SpecialOpUseVariableEnd('%s', '%s')", a:pseudoFT, l:quotedValue)
	augroup end

	" OK, we need to leave the cursor in insert mode, but if we cut all the way
	" to the end of the line, we need to use [A]ppend behaviour instead
	if l:selectToEnd
		" like 'A' in normal mode
		startinsert!
	else
		" like 'i' in normal mode
		startinsert
	endif
endfunction " }}}

" replace a lengthy expression with a variable and set that variable to
" the expression in the previous line
function! <SID>SpecialOpUseVariableEnd(pseudoFT, value) " {{{
	" make sure we destroy the autocommands
	autocmd! SpecialOpUseVariable
	augroup! SpecialOpUseVariable

	let l:value = a:value

	" what is the variable name we now have?
	let l:upto = strpart(getline('.'), 0, col('.'))

	" use a different pattern for VIM filetype:
	if a:pseudoFT == 'vim'
		let l:varname = matchstr(l:upto, '[glsbwta]:\h\w*$')
	elseif a:pseudoFT == 'javascript'
		let l:varname = matchstr(l:upto, '\<\h\w*$')
	elseif a:pseudoFT == 'python'
		let l:varname = matchstr(l:upto, '\h\w*$')
	else
		let l:varname = matchstr(l:upto, '\$\$\=\h\w*$')
	endif

	" remember where we are at ...
	let l:pos = [ line('.') + 1, col('.') ]

	" see if we can find a line with that variable being assigned
  " l:assignPattern = ...
  " also we may want to ensure that we don't insert a new line inside the
  " middle of a () or [] region
  " let l:checkSurround = '()'
	if a:pseudoFT == 'vim'
		let l:assignPattern = '^\s*let\s\+' . escape(l:varname, '$[]') . '\s*=\s*$'
    let l:checkSurround = '('
	elseif a:pseudoFT == 'javascript'
		let l:assignPattern = '^\s*\%(var\s\+\)\=' . l:varname . '\s*=\s*\%(;\s*\)\=$'
    let l:checkSurround = '[({'
	elseif a:pseudoFT == 'python'
		let l:assignPattern = '^\s*' . l:varname . '\s*=\s*\%(#.*\)\=$'
    let l:checkSurround = '[({'
	else
		let l:assignPattern = '^\s*' . escape(l:varname, '$[]') . '\s*=\s*\%(;\s*\)\=$'
    let l:checkSurround = '(['
	endif

	let l:prev = line('.')
	while l:prev > 0
		let l:prev -= 1

    " if the current line is in the middle of 
		let l:prevLineData = getline(l:prev)

		" if the line is an unfinished assignment of the variable, then we want to use it.
		if l:prevLineData =~ l:assignPattern
      " in this circumstance we *don't* want to put the cursor back where it
      " was
      let l:pos[0] -= 1
      break
		endif
	endwhile

	" if we have a line number, then we use this command to insert the new line number
	if l:prev
		let l:cmd = l:prev . 'GS'

	" if we don't have a line number to go to, then we just want to jump back to the
	" next logical place
	else
		let l:curLine = line('.')

    while l:curLine > 0
      " check to see if we are inside parenthesis/brackets ... if so, then we
      " need to be putting our new line of code above the start of this block
      let l:surroundStart = <SID>GetSurroundStart(l:checkSurround)
      if l:surroundStart > 0 && l:surroundStart < l:curLine
        let l:curLine = l:surroundStart
        continue
      endif

      " if the current line looks like it's a contuation of a previous line, we
      " need to use a previous line
      let l:notContinuation = <SID>FindNonContinuation(a:pseudoFT, l:curLine)
      if l:notContinuation > 0 && l:notContinuation < l:curLine
        let l:curLine = l:notContinuation
        continue
      endif

      " no reason not to use the current line
      break
    endwhile

		let l:cmd = l:curLine . 'GO'
	endif

	if a:pseudoFT == 'vim'
		let l:newline = 'let %s = %s'
	elseif a:pseudoFT == 'javascript'
		let l:newline = 'var %s = %s;'
	elseif a:pseudoFT == 'python'
		let l:newline = '%s = %s'
	else
		let l:newline = '%s = %s;'
	endif

	execute 'normal! ' . l:cmd . printf(l:newline, l:varname, l:value) . "\<ESC>"

	" return to where we were
	call cursor(l:pos[0], l:pos[1])
endfunction " }}}

function! <SID>FindNonContinuation(a:pseudoFT, a:startline) " {{{
  return 0
endfunction " }}}

function! <SID>LineIsBlank(pseudoFT, lineData) " {{{
  if a:lineData =~ '^\s*$'
    return 1
  endif

	else
		" PHP
		let l:commentMatch = '^\s*\%(//\|#\)$'
  else
    " python and others
		let l:commentMatch = '^\s*#'
  endif
endfunction " }}}

" vim: sw=2 et ts=2 sts=2 foldmethod=marker

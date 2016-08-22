" This plugin allows you to perform some common code manipulations very easily!

" TODO: when a ternary statement is selected, change it to an 'if'
" statement instead

" TODO: when inserting a line above automatically, insert '{' and '}' if they
" are required

" TODO: when cutting an expression out of a String, we need to go back and
" add quotes again, as necessary

" TODO: provide a new option, 'm' which will save the location of the
" visual area for use later.

" ALSO: provide a new option, 'M', which will swap the current visual
" selection with the previous selection which was marked with 'm'
" TODO: allow having more than 1 previous selection so that 3 different
" selections can be swapped around

function! CleverEdit#go() " {{{
	" if user just wanted a comment shifted up, then we do that
	if <SID>SpecialOpShiftCommentUp()
		return
	endif

	" what's our filetype
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

	" can we figure out what to do automatically?
	if (l:pseudoFT == 'php')
    if <SID>SpecialOpUncompact(@")
      return
    endif
		if @" =~ '^\s*return\s\+.\+;\_s*$'
			call <SID>SpecialOpSplitReturn()
			return
		endif
		if @" =~ '^\$\h\w*$'
			let l:char = char2nr('v')
		endif
	elseif (l:pseudoFT == 'vim') && (@" =~ '^[lgsva]:\h\w*$')
		let l:char = char2nr('v')
	elseif (l:pseudoFT == 'sql') && (@" =~ '^@\h\w*$')
		let l:char = char2nr('v')
	elseif (l:pseudoFT == 'javascript') && (@" =~ '^[$A-Za-z_][$A-Za-z0-9_]*$')
		let l:char = char2nr('v')
	elseif (l:pseudoFT == 'python') && (@" =~ '^\h\w*$')
		let l:char = char2nr('v')
	endif

	let l:commands = [
				\ "'s' to swap arguments",
				\ "'v' to use a variable instead of the selected expression",
				\ "'e' to evaluate python code",
				\ "'p' to execute get the output from php -r",
				\ ]
	if ! has('win32')
		call add(l:commands, "'h' to swap echo with raw html")
	endif

	let l:old_cmdheight = &g:cmdheight
  let &g:cmdheight = len(l:commands) + 1

	if ! exists("l:char")
		" TODO: turn a DESC <table> output into just a list of table names

		echohl WarningMsg
		echon "Press ..."
		for l:command in l:commands
			echon "\n  " . l:command
		endfor
		echohl None

		call inputsave()
		let l:char = getchar()
		call inputrestore()
	endif

	" if 's', swap the arguments
	if l:char == char2nr('s')
		call <SID>SpecialOpSwap()

	" if 'v', use a variable instead
	elseif l:char == char2nr('v')
		call <SID>SpecialOpUseVariableStart(l:pseudoFT)

	" if 'e', evaluate the expression using PYTHON
	elseif l:char == char2nr('e')
		" build a python command and execute our code ...
		let l:exec = printf("import os,sys,time,datetime,hashlib,pprint; print str(%s)", @")
		let l:cmd = "python -c " . shellescape(l:exec)
		let l:result = substitute(system(l:cmd), '^\_s\+\|\_s\+$', '', 'g')

		" delete existing text
		execute printf('normal! %dx', strlen(@"))

		" insert new value
		execute 'normal! i' . l:result

	" if 'p', save to a file and run through PHP ...
	elseif l:char == char2nr('p')
		!echo '<?php' > /tmp/myphp.php
		'<,'>write >> /tmp/myphp.php
		normal! gvd
		read !php /tmp/myphp.php
		!rm -f /tmp/myphp.php

	" if 'h', pipe it through phpechoswap
	elseif l:char == char2nr('h')
		silent '<,'>!phpechoswap

	" otherwise, nothing to do
	endif

	let &g:cmdheight = l:old_cmdheight
endfunction " }}}

function! CleverEdit#replaceSpaces()
	let l:oldPos = getpos(".")
	let l:error = 1

	" temporarily turn off expand-tab
	let l:old_et = &l:et
	setlocal noexpandtab

	try
		" what column are we on now?
		let l:pos = col(".") - 1
		let l:vcol = virtcol(".")
		let l:sw = &l:sw

		" make an array of all the characters in the line
		let l:chars = split(getline("."), '\zs')

		" what character is under the cursor?
		let l:under = l:chars[l:pos]

		" the character under the cursor MUST be a space!
		if l:under != ' '
			echoerr "Cursor not over spaces"
			return
		endif

		" where does the whitespace start?
		let l:start = l:pos
		" if the previous character is a space, move the start position back
		while l:start && (l:chars[l:start - 1] == ' ')
			let l:start -= 1
		endwhile
		
		" same thing for end position
		let l:end = l:pos
		" if the next character is a space, move the end position forward
		while l:end < len(l:chars)  && (l:chars[l:end + 1] == ' ')
			let l:end += 1
		endwhile
		let l:len = 1 + (l:end - l:start)

		" we just need to CEILING() the number of spaces divided by shift-width,
		" that will give us the correct number of spaces to insert
		let l:numTabs = (l:len / l:sw)
		if (l:len % l:sw)
			let l:numTabs += 1
		endif

		" l:back is a motion command to move the cursor back to the first
		" whitespace character in the group
		let l:back = (l:pos > l:start) ? printf("%dh", (l:pos - l:start)) : ''

		" move the cursor to our first space, and insert our tabs!
		let l:exec = printf("normal! %s%dx%di\t", l:back, l:len, l:numTabs)
		execute l:exec
		let g:exec = l:exec
		" no issues, don't move the cursor
		let l:error = 0
	finally
		let &l:et = l:old_et
		if l:error
			call setpos(".", l:oldPos)
		endif
	endtry
endfunction

" shift a comment up?
function! <SID>SpecialOpShiftCommentUp() " {{{
	if @" !~ '^\s*//.\{-}\n\=$'
		return 0
	endif

	" what is the comment?
	let l:comment = substitute(@", '^\s*\|\n$', '', 'g')

	" make sure the comment matches at the end of the line EXACTLY
	let l:start = (col('$') - 1) - strlen(l:comment)

	let l:lineData = getline('.')
	if strpart(l:lineData, l:start) != l:comment
		" what????
		echoerr 'Internal error'
		return 1
	endif

	" if the char before the cursor is whitespace, then we don't want it
	while l:start >= 0
		if strpart(l:lineData, l:start - 1, 1) =~ '\s'
			let l:start -= 1
		else
			break
		endif
	endwhile

	" if we reached the start of the line, nothing to do
	if l:start < 1
		echoerr 'Start of line?'
		return 1
	endif

	" delete from l:start to the end of the line
	call cursor(line('.'), l:start + 1)
	normal! Dmt

	" write the comment in the line above
	execute 'normal! O' . l:comment
	normal! `t
	return 1
endfunction " }}}

" swap two comma-separted items
function! <SID>SpecialOpSwap() " {{{
	" delete the visual area text
	normal! gvx

	" what should we replace around (first pref is a comma)
	if @" =~ '.,.'
		let l:split = '\(,\)'
	else
		" choose between these next ones
		let l:hasColon = (@" =~ '.:.')
		let l:hasArrow = (@" =~ '.=>.')
		let l:hasAssign = (@" =~ '.=[^>]')

		" if more than one to choose from, ask what to replace them on?
		let l:num = (l:hasColon + l:hasArrow + l:hasArrow)
		if l:num < 1
			" let the user decide:
			let l:input = input('What chars to do you want to swap on? ')
			let l:split = '\(' . escape(l:input, '\.$*^()[]') . '\)'
			unlet l:input
		elseif l:num > 1 " {{{
			" need to choose .... eeeks
			echohl WarningMsg
			echon "Swap on? "
						\ (l:hasColon  ? "\n  1: :   colon" : '')
						\ (l:hasArrow  ? "\n  2: =>  dictionary construction" : '')
						\ (l:hasAssign ? "\n  2: =   assignment" : '')
			echohl None

			call inputsave()
			let l:char = getchar()
			call inputrestore()

			if l:char == char2nr('1')
				let l:hasArrow = 0
				let l:hasAssign = 0
			elseif l:char == char2nr('2')
				let l:hasColon = 0
				let l:hasAssign = 0
			else
				let l:hasColon = 0
				let l:hasArrow = 0
			endif
		endif " }}}

		let l:split = l:hasColon ? '\(\s*:\)' : (l:hasArrow ? '\(\s*=>\)' : '\(\s*=\s*\ze[^>]\)')
	endif

	if exists('l:split')
		let l:sub = substitute(@", '^\(.\{-}\)\s\{-}' . l:split . '\s*\(.\{-}\)$', '\3\2 \1', '')
	else
		let l:sub = @"
	endif

	execute 'normal! i' . l:sub 
endfunction " }}}

" split 'return ...;' into '$return = ...; return $return'
function! <SID>SpecialOpSplitReturn() " {{{
	" what is the return value?
	let [ l:indent, l:return ] = matchlist(@", '^\(\s*\)return\s\+\(.*\);\_s*$')[1:2]

	let l:pos = getpos('.')

	" replace the current line with something new ...
	" delete from l:start to the end of the line
	call append(line('.') - 1, [ l:indent . '$return = ' . l:return . ';', l:indent . 'return $return;' ])
	delete
	call setpos('.', l:pos)
endfunction " }}}

" when using 'Sv' on a selection:
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
	if a:pseudoFT == 'vim'
		let l:varMatch = '[glsbwta]:\h\w*'
		let l:lineIsCommentMatch = '^\s*"'
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
	elseif a:pseudoFT == 'python'
		let l:varMatch = '\h\w*'
		let l:lineIsCommentMatch = '^\s*#'
		let l:varLineAssignsMatch = '^\s*%s\s*=\s*%s\s*\%(#.*\)\=$'
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
			if l:prevLineData =~ '^\s*$'
						\ || l:prevLineData =~ l:lineIsCommentMatch
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

	" go up one line and insert the given text
"      execute 'normal! mtyyp`tjmtkS' . l:varname . ' = ' . l:value . ";\<ESC>`t"

	" remember where we are at ...
	let l:pos = [ line('.') + 1, col('.') ]

	" see if we can find a line with that variable being assigned
	if a:pseudoFT == 'vim'
		let l:assignPattern = '^\s*let\s\+' . escape(l:varname, '$[]') . '\s*=\s*$'
	elseif a:pseudoFT == 'javascript'
		let l:assignPattern = '^\s*\%(var\s\+\)\=' . l:varname . '\s*=\s*\%(;\s*\)\=$'
	elseif a:pseudoFT == 'python'
		let l:assignPattern = '^\s*' . l:varname . '\s*=\s*\%(#.*\)\=$'
	else
		let l:assignPattern = '^\s*' . escape(l:varname, '$[]') . '\s*=\s*\%(;\s*\)\=$'
	endif

	let l:prev = line('.')
	while l:prev > 0
		let l:prev -= 1
		let l:prevLineData = getline(l:prev)

		" if the line is an unfinished assignment of the variable, then we want to use it.
		if l:prevLineData =~ l:assignPattern
"			let l:msg = printf("Overwrite this incomplete assignment on line %d?\n%s", l:prev, l:prevLineData)
"			if l:prev >= (line('.') - 3) || 1
"						\ || 1 == confirm(l:msg, "&Yes\n&No", 0, 'Question')
				let l:pos[0] -= 1
				break
"			endif
		endif
	endwhile

	" if we have a line number, then we use this command to insert the new line number
	if l:prev
		let l:cmd = l:prev . 'GS'

	" if we don't have a line number to go to, then we just want to jump back to the
	" next logical place
	else
		let l:curLine = line('.')
		" if the current line looks like part of an expression, then we'll need to find
		" the start of it
		while getline(l:curLine) =~ '^\%(/\*\*/\)\=\s*[\-.&|]'
			let l:curLine -= 1
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

function! <SID>SpecialOpUncompact(string) " {{{
	" can we do this on the string given?
	if a:string !~# '^\s*compact(\s*\%(''\w\+''\|"\w\+"\)\s*\%(,\s*\%(''\w\+''\|"\w\+"\)\s*\)*)\s*$'
		return 0
	endif

	let l:vars = split(substitute(a:string, '^\s*compact\|[^A-Za-z0-9_,]', '', 'g'), ',')

	" what is the indent of the current line?
	let l:indent = matchstr(getline('.'), '^\s*') . "\t\t"

	" we need to grab everything up to the end of the line which may need to be kept
	normal! `<"ty$

	let l:after = strpart(getline('.'), col("'>"))

	" build our new array ...
	normal! Carray(
	for l:var in l:vars
		call append(line('.'), printf("%s'%s'\t=> $%s,", l:indent, l:var, l:var))
		normal! j
	endfor
	call append(line('.'), l:indent . ')' . l:after)
	return 1
endfunction " }}}

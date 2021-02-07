; Function to control str and keyword equality
(defun keyword_control(str)
	(setf s (string-downcase str))
	(cond 
	 ((string-equal "and" s) "KW_AND")
	 ((string-equal "or" s) "KW_OR")
	 ((string-equal "not" s) "KW_NOT")
	 ((string-equal "equal" s) "KW_EQUAL")
	 ((string-equal "less" s) "KW_LESS")
	 ((string-equal "nil" s) "KW_NIL")
	 ((string-equal "list" s) "KW_LIST")
 	 ((string-equal "append" s) "KW_APPEND")
	 ((string-equal "concat" s) "KW_CONCAT")
	 ((string-equal "set" s) "KW_SET")
	 ((string-equal "for" s) "KW_FOR")
	 ((string-equal "deffun" s) "KW_DEFFUN")
	 ((string-equal "if" s) "KW_IF")
	 ((string-equal "load" s) "KW_LOAD")
	 ((string-equal "true" s) "KW_TRUE")
	 ((string-equal "false" s) "KW_FALSE")
	 ((string-equal "disp" s) "KW_DISP")
	 ((string-equal "exit" s) "KW_EXIT")
	 (t nil)
	 )
)

; Function to control str and operator equality
(defun is-op (str)
    (setf s nil)
    (cond
        ((string-equal str "+") (setf s t))
        ((string-equal str "-") (setf s t))
        ((string-equal str "/") (setf s t))
        ((string-equal str "(") (setf s t))
        ((string-equal str ")") (setf s t))
        ((string-equal str "\"") (setf s t))
        ((string-equal str ",") (setf s t))
        ((string-equal str "*") (setf s t))
        ((and (string-equal s "#\*")(string-equal (char content (+ i 1)) "#\*")) t)
    )
)

; Function to check whether str is alpha or not
(defun alpha_control(str)		
	(if (or (and (> (char-int str) 64) (< (char-int str) 91))
			(and (> (char-int str) 96) (< (char-int str) 123))
		)
		t
		nil
	)
)

; Function to check whether str is digit or not
(defun digit_control(str)		
	(if (and (> (char-int str) 47) (< (char-int str) 58))
		t
		nil
	)
)

;Function to check ID ERROR
(defun id_error(str)
	(setf s nil)
	(cond ((equal str #\?) (setf s t))
		  ((equal str #\.) (setf s t))
		  ((equal str #\$) (setf s t))
		  ((equal str #\^) (setf s t))
		  ((equal str #\!) (setf s t))
	)
)

(defun lexer_of_mine(content)		; Lexer Function
	(setq tokens (list))			; Return list.
	(setq size (length content))	; Text length.
	(setq flag nil)
	(setf quoCount 0) ; count for quotes' number
	(setf tempI 0) ; value to keep current i of first quote
	(setf check 0)
	(setf errcheck nil)

	(let ((i 0))

		(loop while (< i size) do 				; Outer loop
			(setq ContentChar (char content i))	; Take each character of readed text.
			(cond ((or (alpha_control ContentChar) (digit_control ContentChar))		; DFA Step 1: digit or alphabetical char control.
				(setq temp (string ""))
				(let ((j i))
					(loop while (and (< (+ i 1) size)(or (alpha_control ContentChar) (digit_control ContentChar))) do 	; Saving the all characters or digits.
						(setq temp (concatenate 'string temp (string ContentChar)))
						(setq j (+ j 1))
						(setq i (+ i 1))
						(setq ContentChar (char content j))
					)
				)
				(cond 																; DFA Step 2: Keyword or terminal or non-terminal control. 
					((and (> (length temp) 1)(and (digit_control (char temp 0)) (not (alpha_control (char temp 1))))) (push "VALUE" tokens) (setq flag t) (setf check 1)) ; Push to list as VALUE, if it's integer.
					((and (<= (length temp) 1) (digit_control (char temp 0)) ) (push "VALUE" tokens) (setq flag t) (setf check 1)) ; Push to list as VALUE, if it's integer.
					
					((keyword_control temp) (push (keyword_control temp) tokens) (setq flag t) (setf check 1))	; Push to list, if it's keyword.
					
					(t (setf llen (length (string ContentChar))) 
					(loop for a from 0 to llen
					do (if (id_error ContentChar)
						(setf errcheck t))
					)
					(if (and (> (length temp) 1) (and (digit_control (char temp 0)) (alpha_control (char temp 1)))) (setf errcheck t) )
					(if (equal errcheck nil)
						(push "IDENTIFIER" tokens))
					(if (equal errcheck t)	(push "ERROR" tokens)) (setf check 1)) ; Push to list, if it's identifier.
				))
			)	


			(if (is-op ContentChar)		; DFA Step 3: Operator control.
				(cond 
					((and (< (+ i 1) size)(and (equal ContentChar #\*)(equal (char content (+ i 1)) #\*)))	; If the operator is "**"
						(push "OP_DBMULT" tokens)
						(setq i (+ i 1))
						(setq flag t)
						(setf check 1)
					)
					((and (string-equal ContentChar "\"") (equal quoCount 0))
						(progn(setf quoCount (+ quoCount 1))
						(push "OP_OC" tokens) 
						(setf tempI i))
						(setf flag t)
						(setf check 1)
					)
					
					((and (and (string-equal ContentChar "\"") (equal quoCount 1)) (not(equal tempI i)))
						(progn(setf quoCount (- quoCount 1))
						(push "OP_CC" tokens) (setf flag t)
						(setf check 1))
					)
					; Push to list, if it's operator.
					((string-equal ContentChar "+") (push "OP_PLUS" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar "-") (push "OP_MINUS" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar "/") (push "OP_DIV" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar "(") (push "OP_OP" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar ")") (push "OP_CP" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar ",") (push "OP_COMMA" tokens) (setq flag t) (setf check 1))
			        ((string-equal ContentChar "*") (push "OP_MULT" tokens) (setq flag t) (setf check 1))

				)
			)

			; Code to prevent syntax error
			(when (or(equal ContentChar #\Space )(equal ContentChar #\Newline)(equal ContentChar #\*)(equal ContentChar #\;)(equal ContentChar #\tab))
				(setf check 1)
			)
			(if (and (< (+ i 1) size)(and (equal ContentChar #\;)(equal (char content (+ i 1)) #\;))) ; DFA Step 4: Comment line control
				(progn(push "COMMENT" tokens)
				(setf check 1)
				(loop while (not (equal (char content (+ i 1)) #\Newline))
										do (setf i (+ i 1))
									)
					)
			)

			; Code to prevent error because of #\' character
			(if (or (equal ContentChar #\') (equal ContentChar #\`)) 
				(setf check 1))
			
			; Error check for leading zero
			(if (and (< (+ i 2) size)(or (and (and (equal ContentChar #\Space)(equal (char content (+ i 1)) #\0)) (or (alpha_control (char content (+ i 2))) (digit_control (char content (+ i 2))) )) 
				(and (and (equal ContentChar #\( ) (equal (char content (+ i 1)) #\0)) (and (or (alpha_control (char content (+ i 2))) (digit_control (char content (+ i 2))) ) (not (equal (char content (+ i 2)) #\) ) )))
				))
				(setf check 0)
			)

			(if (equal check 0)
				(progn
				(push "ERROR" tokens)
				)
			)

			(setf check 0)
			(setq i (+ i 1))
			(setf errcheck nil)
		)
	)
	(reverse tokens)	; Return the list.
)

; gppinterpreter function starts interpreter
; It can have 0 or 1 input.
(defun gppinterpreter( &optional name)
    (if (equal name nil)
        (terminal-fun)
    (file-fun name)
    )
)

; With terminal-fun, user input is taken from terminal
; lexer is used for input and output is printed on terminal
(defun terminal-fun ()
    (setf user-input " ")

    (with-open-file ; Clear output file before writing
		   	(stream "parsed_lisp.txt" ; Output file 
		       :direction :output    
		       :if-exists :supersede ; Supersede if file exists
		       :if-does-not-exist :create) ; Create if file doesn't exist
	)
    
    (loop while (not (or (equal user-input "") (equal (length user-input) 0)))
        do
        (format t "~%>")
        (setf user-input (read-line))
		(setf lst (make-list 0))
		(setf lst (lexer_of_mine user-input))
		(with-open-file 
		   	(stream "parsed_lisp.txt" ; Output file 
		       :direction :output    
		       :if-exists :append ; Supersede if file exists
		       :if-does-not-exist :create) ; Create if file doesn't exist
		    (loop for i in lst
		  		do(format stream "~A~%" i)
		  	)
	  	)        
    )
)


; Function to contents from file
(defun read-contents (filename)
  (with-open-file (stream filename)
    (let ((cnt_fl (make-string (file-length stream))))
      (read-sequence cnt_fl stream) cnt_fl)))

; With file-fun, contents are read from file
; then lexer is used for these contents
; and finally output is written to file
(defun file-fun (filename)
	(setf lst (make-list 0))
	(setf lst (lexer_of_mine(read-contents filename)))
	(with-open-file 
   	(stream "parsed_lisp.txt" ; Output file 
       :direction :output    
       :if-exists :supersede ; Supersede if file exists
       :if-does-not-exist :create) ; Create if file doesn't exist
    (loop for i in lst
  		do(format stream "~A~%" i)
  	)
  )
)

;Main function
(defun main ()
    (if (null *args*)
        (gppinterpreter)
        (gppinterpreter (car *args*))
    )
)

; Calling main function
(main)
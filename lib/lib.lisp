(defun inspect (x) {
    (print x)
    x
})

(defun array-to-str (array) {
    (str-merge
        "["
        (str-join (map
            (fn (i) {
                (str-from-n (bufget-u8 array i) "0x%x")
            })
            (range (buflen array))
        ) " ")
        "]"
    )
})

(defun print-array (array) {
    (print (array-to-str array))
})

(def defmacro (macro (name args body)  
    `(def ,name (macro ,args ,body))
))

(defun map-quote (lst) (map
    (fn (x) (list 'quote x))
    lst
))

(defun apply (f args)
    (eval (cons f (map-quote args)))
)

(defun curry (f) {
    (var curry-args (rest-args)) 
    (fn () (apply f
        (append curry-args (rest-args))
    ))
}) 

(defun last (lst) 
    (ix lst (- (length lst) 1))
)

(defun list-flatten (lst-2d) {
    (var result nil)
    (map
        (fn (value) {
            (setq result (cond
                ((list? value) (append value result))
                (t (cons value result))
            ))
        })
        (reverse lst-2d)
    ) 
    
    result
})

(defun list-populate (value n) {
    (map (fn (-) value) (range n))
})

; Get first element of lst for which fun when applied with said element returns
; true, or nil if no element matched
(defun list-first-by (lst fun) 
    (foldl
        (fn (init element) 
            (cond
                (init init)
                ((fun element) element)
                (t nil)
            )
        )
        nil
        lst
    )
)

(defun enumerate (lst) {
    (var i 0)
    (map
        (fn (value) {
            (var result-hello (cons i value))
            (setq i (+ i 1))
            result-hello
        })
        lst
    )
})

(def u64-max 18446744073709551615u64)
(defun min () {
    (var min-binary (fn (a b) (if (< a b) a b)))
    
    (foldl min-binary u64-max (rest-args))
})
(defun max () {
    (var max-binary (fn (a b) (if (> a b) a b)))
    
    (foldl max-binary 0 (rest-args))
})

(defun any (lst) 
    (apply or lst)
)
(defun all (lst) 
    (apply and lst)
)

(defun str-starts-with (str prefix)
    (= (str-find str prefix) 0)
)
(defun str-ends-with (str suffix)
    (= (str-find str suffix 'left) (- (str-len str) 1))
)

(defun str-remove-prefix (str prefix) {
    (if (= (str-find str prefix) 0) 
        (str-part str (str-len prefix))
        str
    )
})

(defun str-remove-suffix (str suffix) {
    (var index (str-find str suffix 'left))
    (if (= index (- (str-len str) 1))
        (str-part str 0 index)
        str
    )
})

; Get the number of characters from the start of str that matches any character in
; the string set, without any other characters in between.
(defun str-set-length-start (str char-set) {
    (var char-in-set (fn (char characters)
        (any (map (fn (index) {
            (= char (bufget-u8 characters index))
        }) (range (str-len characters))))
    ))
    (var index 0)
    (loopwhile (and
        (< index (str-len str))
        (char-in-set (bufget-u8 str index) char-set)
    )
        (setq index (+ index 1))
    )
    
    index
})

; Get the number of characters from the end of str that matches any character in
; the string set, without any other characters in between.
(defun str-set-length-end (str char-set) {
    (var char-in-set (fn (char characters)
        (any (map (fn (index) {
            (= char (bufget-u8 characters index))
        }) (range (buflen characters))))
    ))
    (var index (- (str-len str) 1))
    (loopwhile (and
        (>= index 0)
        (char-in-set (bufget-u8 str index) char-set)
    )
        (setq index (- index 1))
    )
    
    (- (str-len str) index 1)
})

(defun str-trim (str whitespace) {
    (var start (str-set-length-start str whitespace))
    (var end (-
        (str-len str)
        (str-set-length-end str whitespace)
    ))
    (str-part str start (max 0 (- end start)))
})

(defun str-split-useful (str pattern) { 
    (var segments nil)
    (var occurrence 0)
    (var previous-index 0)
    (var pattern-len (str-len pattern))
    
    (var take-to-index (fn (new-index) {
        (if (= new-index -1)
            (setq new-index (str-len str))
        )
        (var this-segment (if (= previous-index (str-len str))
            ""
            (str-part str previous-index (- new-index previous-index))
        ))
        (setq segments (cons this-segment segments))
        (setq previous-index (+ new-index pattern-len))
    }))
    
    (loopwhile t {
        (var index (str-find str pattern previous-index))
        (take-to-index index)
        (if (= index -1)
            (break)
        )
        (setq occurrence (+ occurrence 1))
    })
    (reverse segments)
})

(defun str-repeat (str amount)
    (str-join (list-populate str amount))
) 

; buf-resize isn't part of lbm's std library :(
(defun buf-add-null-byte (buf) {
    (var result (bufcreate (+ (buflen buf) 1)))
    (bufclear result)
    (bufcpy result 0 buf 0 (buflen buf))
    result
})

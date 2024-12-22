(def esc (str-merge [0x1b 0] "["))

(include "lib/style.lisp")

; Like print but doesn't add a newline.
; Seems to be very slow...
(defun puts (str)
    (exec "printf" "printf" str)
)

; For a shell command which outputs a decimal integer string, return the nth
; byte of said integer. (This is necessary because we can only extract 255 bytes
; at a time)
; The least significant byte has the index 0.
; I love this :)
(defun get-numerical-command-byte (command byte-index) {    
    (var return-value (unsafe-call-system (str-merge
        "exit $((($("
        command
        ") >> "
        (str-from-n (* byte-index 8))
        ") & 255))"
    )))
    ; I have no idea why the return code gets multiplied by 256...
    (/
        (if (eq return-value true)
            0
            return-value
        )
        256
    )
}
)

; Returns the integer output by the command. (Note: the command may be ran
; multiple times.)
(defun get-numerical-command (command) {
    (var result 0)
    (var index 0)
    (loopwhile t {
        (var byte (get-numerical-command-byte command index))
        (if (= byte 0)
            (break result)
        )
        (setq result (bitwise-or result (shl byte (* index 8))))
        (setq index (+ index 1))
    })
})

(defun get-terminal-dimensions () {
    ; This is mega inefficient and I love it
    (var height (get-numerical-command "stty size | cut -d ' ' -f1"))
    (var width (get-numerical-command "stty size | cut -d ' ' -f2"))
    
    (list width height) 
})

(defun enable-alternate-buffer ()
    (str-merge esc "?1049h")
)
(defun disable-alternate-buffer ()
    (str-merge esc "?1049l")
)

(defun show-cursor ()
    (str-merge esc "?25h")
)
(defun hide-cursor ()
    (str-merge esc "?25l")
)

(defun cursor-to-pos (pos)
    (let (
        ((x y) pos)
    )
        (str-merge esc (str-from-n y) ";" (str-from-n x) "H")
    ) 
)

; Move cursor n columns forwards on the line
(defun cursor-move-forwards (n)
    (str-merge esc (str-from-n n) "C")
)
; Move cursor to column x
(defun cursor-to-col (x)
    (str-merge esc (str-from-n x) "G") 
)

(defun text-at-pos (pos text) 
    (str-merge (cursor-to-pos pos) text)
)

(defun clear-screen ()
    (str-merge esc "2J")
)

(defun clear-area (pos dimensions)
    (let (
        ((width height) dimensions)
    )
        (str-join
            (map
                (fn (i) (str-merge
                    (cursor-to-pos (pos+ pos (list 0 i))) 
                    (str-replicate width \# ) 
                ))
                (range height)
            )
        )
    )
)

; The position and dimensions are that of the content which will be framed. So
; the frame pos and dimensions will be offset by -1 and 2 respectively.
(defun create-frame (pos dimensions) {
    (var (x y) pos)
    (var (width height) dimensions) 
    
    ; ╭──╮
    ; │  │
    ; ╰──╯
    
    (str-join (append
        (list
            (str-merge
                (cursor-to-pos (pos+ pos (list -1 -1)))
                "╭" (str-repeat "─" width) "╮"
            )
        )
        (map
            (fn (y)
                (str-merge
                    (cursor-to-pos (list (- x 1) y))
                    "│" (cursor-move-forwards width) "│"
                )
            ) 
            (range y (+ y height))
        )
        (list
            (str-merge
                (cursor-to-pos (pos+ pos (list -1 height)))
                "╰" (str-repeat "─" width) "╯"
            )
        )
    ))
})  

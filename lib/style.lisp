(def colors '(
    ("black" . 30)
    ("red" . 31)
    ("green" . 32)
    ("yellow" . 33)
    ("blue" . 34)
    ("magenta" . 35)
    ("cyan" . 36)
    ("white" . 37)
    ("bright-black" . 90)
    ("bright-red" . 91)
    ("bright-green" . 92)
    ("bright-yellow" . 93)
    ("bright-blue" . 94)
    ("bright-magenta" . 95)
    ("bright-cyan" . 96)
    ("bright-white" . 97)
))

; A "color" is a list of the form (r:int g:int b:int).
; A "style-color" is a list of form (type color)
; where
;   type = 'fg | 'bg

; Returns a list of graphics rendition numbers.
(defun build-style-color (style-color) {
    (var (type color) style-color)
    
    (append
        (list
            (match type
                (fg 38) 
                (bg 48)
            )
            2
        )
        color
    )
})

(defun parse-style-color (color-str) {
    (var type (cond
        ((str-starts-with color-str "fg-") 'fg)
        ((str-starts-with color-str "bg-") 'bg)
        (t (exit-error (str-merge
            "Given color '"
            color-start
            "' is invalid. Colors must start with either 'fg-' or 'bg-'. (Note: Any invalid styles are treated as colors.) ((in parse-style-color))"
        )))
    ))
    (var value (str-replace
        color-str
        (match type
            (fg "fg-#")
            (bg "bg-#")
        )
        ""
    ))
    
    (match (map
        (fn (component-str) {
            (str-to-i component-str 16)
        })
        (str-split value 2)
    )
        (((? r) (? g) (? b)) (list type (list r g b)))
        (_ { 
            (exit-error (str-merge 
            "Given color '"
            color-str
            "' is invalid. Hex colors must consist of 6 digits, example: '#000000'."
        ))})
    )
})

; color-str should be a key of `colors`, or a 6-digit hex color, either way
; prefixed with 'fg-' or 'bg-'. Returns a single or a list of graphics rendition
; numbers.
(defunret parse-color (color-str) {
    (var type (cond
        ((str-starts-with color-str "fg-") 'fg)
        ((str-starts-with color-str "bg-") 'bg)
        (t (exit-error (str-merge
            "Given color '"
            color-str
            "' is invalid. Colors must start with either 'fg-' or 'bg-'. (Note: Any invalid styles are treated as colors.) ((in parse-color))"
        ))); here
    ))
    
    (var value (str-replace
        color-str
        (match type
            (fg "fg-")
            (bg "bg-")
        )
        ""
    ))
    
    (var number (cond
        ((str-starts-with value "#") (return
            (build-style-color (parse-style-color color-str))
        ))
        ((assoc colors value) (assoc colors value))
        (t (exit-error (str-merge
            "Given color '"
            color-str
            "' is invalid. Color '"
            value 
            "' doesn't exist."
        )))
    ))
    
    (match type
        (fg number)
        (bg (+ number 10))
    )
})

; Returns a singlegraphics rendition number.
(defun parse-single-style (single-style) {
    (match single-style
        ("bold" 1)
        ((? color) (parse-color color))
    )
})

; A "style" is a string of "<single-style>" joined by ':',
; where
;   single-style =
;     "bold"
;     | "fg-<color-name>"
;     | "fg-<color-name>"
;   color-name = one of the keys of `colors`.

; Returns a list of graphics rendition numbers.
(defun parse-style (style) { 
    (map parse-single-style (str-split style ":"))
})

(defun build-style (style-numbers)
    (match style-numbers
        (nil "")
        ((? numbers) (str-merge
            esc
            (str-join (map str-from-n (list-flatten numbers)) ";")
            "m"
        ))
    )
)
 
(defun style-reset ()
    (str-merge esc "0m")
)

(defun apply-style-to (style str)
    (str-merge (build-style style) str (style-reset))
)  

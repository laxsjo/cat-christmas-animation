(def frame-delimiter "==========")

(include "lib/paths.lisp")

; A `frame` is a struct of the form
; (
;   ('width . int)
;   ('hold . nil|'end)
;   ('lines . (..str))
; )

; A `frames` is a struct of the from
; (
;   ('pos . (int int))
;   ('start-frame int)
;   ('style str)
;   ('name str)
;   ('frames . (..frame))
; )

(defun pos-apply (operation positions) {
    (var x-coords (map first positions))
    (var y-coords (map second positions))
    (list
        (apply operation x-coords)
        (apply operation y-coords)
    )
})

(defun pos+ () 
    (pos-apply + (rest-args))
)

(defun pos- () 
    (pos-apply - (rest-args))
)

(defun pos/ () 
    (pos-apply / (map promote-to-pos (rest-args)))
)

(defun pos< () 
    (pos-apply < (map promote-to-pos (rest-args)))
)
(defun pos> () 
    (pos-apply > (rest-args))
)
(defun pos<= () 
    (pos-apply <= (rest-args))
)
(defun pos>= () 
    (pos-apply >= (rest-args))
)

(defun pos? (value)
    (match value
        ((_ _) true)
        (_ false)
    )
)

; signature: (promote-to-pos maybe-pos:(int int)|int) -> (int int)
(defun promote-to-pos (maybe-pos) (match maybe-pos
    ((? pos) (pos? pos) pos)
    ((? int) (list int int))
))

; signature: (positions:(..(int int))) -> bounds
; where
;   bounds = (pos:(int int) dimensions:(int int))
(defun point-cloud-get-bounds (positions) {
    (var x-coords (map first positions))
    (var y-coords (map second positions))
    
    (var min-x (apply min x-coords))
    (var min-y (apply min y-coords))
    (var max-x (apply max x-coords))
    (var max-y (apply max y-coords))
    
    (list
        (list min-x min-y)
        (list
            (- max-x min-x)
            (- max-y min-y)
        )
    )
})

; signature: (bounds-join ..bounds) -> bounds
; where
;   bounds = (pos:(int int) dimensions:(int int))
; Return minimal bounding box which contains the given bounding boxes.
(defun bounds-join () {
    (var bounds (rest-args))
    (var min-positions (map first bounds))
    (var max-positions (map (curry apply pos+) bounds))
    (point-cloud-get-bounds
        (append
            min-positions
            max-positions
        )
    )
})


; linearly interpolate between a and b by v.
; v is in range 0-1
(defun lerp (a b v)
    (+ (* (- 1 v) a) (* v b)) 
)

; Linearly interpolate between the two integer colors a and b by v.
; v is in the range 0.0 to 1.0.
(defun lerp-color (a b v) {

    (var r (to-i (lerp (ix a 0) (ix b 0) v)))
    (var g (to-i (lerp (ix a 1) (ix b 1) v)))
    (var b (to-i (lerp (ix a 2) (ix b 2) v)))

    (list r g b)
})

(defun parse-metadata (str) {
    (var result (list
        (cons 'start-frame 'unknown)
        (cons 'pos 'unknown)
        (cons 'parent nil)
        (cons 'style "")
        (cons 'opaque-space-str nil)
    ))
    (var lines (str-split str "\n"))
    (loopforeach line lines {
        (if (!= (str-find line "=") -1) {
            (match (str-split line "=")
                (((? sym-str) (? value-str))
                    (setassoc result (read sym-str) (read value-str))
                )
            )
        })
    })
    
    (loopforeach pair result {
        (var (key . value) pair)
        (if (eq value 'unknown) {
            (exit-error (str-merge
                "Parsed frames metadata did not set "
                (to-str key)
            ))
        })
    })
    
    result
})

(defun parse-frame-commands (str) {
    (var result (list
        (cons 'hold 1)
    ))
    (var components (str-split str " "))
    (looprange i 0 (length components) {
        (match (ix components i)
            ("hold" (setq result
                (setassoc result 'hold (read (ix components (+ i 1))))
            ))
            ("jump-to" (setq result
                (setassoc result 'jump-to (read (ix components (+ i 1))))
            ))
            ("no-keepalive" (setq result
                (setassoc result 'no-keepalive true)
            ))
            ("start-fade" {
                (match (drop components i)
                    (("start-fade" (? start-color-str) "to" (? end-color-str) "for" (? frame-cnt)) {
                        (setq result
                            (setassoc result 'fade-length (str-to-i frame-cnt))
                        )
                        (setq result
                            (setassoc result 'fade-color-start (parse-style-color (read start-color-str)))
                        )
                        (setq result
                            (setassoc result 'fade-color-end (parse-style-color (read end-color-str)))
                        )
                        
                    })
                    ((? lst) (exit-error (str-merge
                        "Invalid start-fade command, must be of the from 'start-fade \"<start-color>\" to \"<end-color>\" for <frame-number>'."
                        "Got "
                        (to-str (take lst 5))
                    )))
                )
            })
        )
    })
    result
})

(defun load-frames (path) {
    (var file (fopen path "r"))
    (if (not file) {
        (exit-error "oh nose, file not found :(")
    })
    (var content (str-remove-comments (buf-add-null-byte (load-file file) 1)))
    (var (metadata-string . frame-strings) (str-split-useful content frame-delimiter))
    
    (var metadata (parse-metadata metadata-string))
    
    (var name (path-basename (path-filename path)))

    (var frame-list (list-flatten
        (map (fn (frame-str) (match (str-split frame-str "\n")
            ; commands is everything after the '=' characters
            (((? commands) (? first-line) . (? remaining-lines)) { 
                (var config (parse-frame-commands commands))
                
                ; +-----------------------+
                ; | Assume that the frame |
                ; | has a frame around it |
                ; +-----------------------+
                (var width (max 0 (- (str-len first-line) 2)))
                
                (setq remaining-lines
                    (take remaining-lines (- (length remaining-lines) 1))
                )
                
                (setq remaining-lines (map
                    (fn (line) 
                        (str-remove-suffix (str-part line 1) "|")
                    )
                    remaining-lines
                ))
                
                (var frame (append
                    config
                    (list                        
                        (cons 'width width)
                        (cons 'lines remaining-lines)
                    )
                ))
                (list frame)
            })
            (_ (exit-error (inspect (str-merge "invalid frame-str: \n" frame-str))))
        )) frame-strings)
    ))
    
    (append
        metadata
        (list
            (cons 'name name)
            (cons 'frames frame-list)
        )
    )
})

(defun frame-get-dimensions (frame) {
    (list
        (assoc frame 'width)
        (length (assoc frame 'lines))
    )
})

(defun frames-get-bounds (frames) {
    (var pos (assoc frames 'pos))
    (var dimensions (frame-get-dimensions (first (assoc frames 'frames))))
    (list pos dimensions) 
})

(defun resolve-frames-list-parent-positions (frames-list) {
    (var get-parent-by-name (fn (name)
        (list-first-by frames-list (fn (frames)
            (eq (assoc frames 'name) name)
        ))
    ))
    (loopwhile (any (map
        (fn (frames) {
            (match (assoc frames 'parent)
                (nil nil)
                ((? parent-name) {
                    (setassoc frames 'pos
                        (pos+
                            (assoc frames 'pos)
                            (assoc (get-parent-by-name parent-name) 'pos)
                        )
                    )
                    (setassoc frames 'parent nil)
                    (assoc frames 'parent)
                })
            ) 
        })
        frames-list
    )) nil)
    frames-list
})

(defun buf-last-bytes (buffer n) {
    (setq n (min n (buflen buffer)))
    
    (var result (bufcreate n))
    (bufcpy result 0 buffer (- (buflen buffer) n) n) 
    result
})

(defun str-remove-comments (str) {
    (str-join
        (filter
            (fn (line) (not (str-starts-with line ";")))
            (str-split-useful str "\n")
        )
        "\n"
    )
})

; Converts all spaces in string to terminal escape commands which move the
; corresponding amount of columns to the left
(defun str-make-spaces-transparent (str) {
    (var components (list))
    (var current-start 0)
    (var last-was-space false)
    (looprange index 0 (str-len str) {
        (var is-space (= (bufget-u8 str index) \# ))
        
        (if is-space
            (if (not last-was-space) {
                (if (!= current-start index) {
                    (setq components (cons
                        (str-part str current-start (- index current-start))
                        components
                    ))
                })
                (setq last-was-space true)
                (setq current-start index)
            })
            (if last-was-space {
                (setq components (cons
                    (- index current-start)
                    components
                ))
                (setq last-was-space false)
                (setq current-start index)
            })
        )
    })
    (if last-was-space {
        (setq components (cons
            (- (str-len str) current-start)
            components
        ))
    } {
        (setq components (cons
            (str-part str current-start (- (str-len str) current-start))
            components
        ))
    })
    
    (str-join (map
        (fn (value) (match value
            ((? space-width) (number? space-width) (cursor-move-forwards space-width))
            ((? str) str)
        ))
        (reverse components)
    ))
})

; Render frame at specified column and row
(defun render-frame (frame pos style-str override-color opaque-space-str transparent) {
    (var content (str-join  
        (list-flatten (map  
            (fn (pair) {
                (var (i . line) pair)
                (setq line (if transparent
                    (str-make-spaces-transparent line)
                    line
                ))
                (if opaque-space-str
                    (setq line (str-replace line opaque-space-str " "))
                )
                (list
                    (cursor-to-pos (pos+ pos (list 0 i)))
                    line
                )
            })
            (enumerate (assoc frame 'lines))
        )) 
    ))
    
    (apply-style-to (parse-style style-str) (if override-color
        (apply-style-to
            (build-style-color override-color) 
            content
        )
        content
    ))
})

; A `canvas` is an assoc-list of the form
; (
;   ('pos . pos)
;   ('bounds . bounds)
;   ('frames-list . (..frames))
; )
; where
;   pos = (int int)
;   dimensions = (int int)
;   bounds = (pos dimensions)
; The frames are stored in the reversed order they are rendered in.
; The bounds are in local space.

(defun create-canvas (pos)
    (list
        (cons 'pos pos)
        (cons 'bounds nil)
        (cons 'frames-list nil)
    )
)

(defun canvas-add-frames (canvas frames) 
    (setassoc canvas 'frames-list
        (cons
            frames
            (assoc canvas 'frames-list)
        )
    )
) 

(defun canvas-get-dimensions (canvas)
    ; The canvas' dimensions if the bounding box started at the origin 
    (pos+
        (first (assoc canvas 'bounds))
        (second (assoc canvas 'bounds))
    )
)

; Resolve the parent positions of all frames in canvas, and calculate the
; bounding box.
(defun canvas-resolve-coordinates (canvas) {
    (resolve-frames-list-parent-positions (assoc canvas 'frames-list))
    (setassoc canvas 'bounds
        (apply bounds-join
            (map frames-get-bounds (assoc canvas 'frames-list))
        )
    )
    canvas
})

; signature: (canvas) -> renderer-fn
; where
;   renderer-fn = () -> bool
; Create function which for each time it is called, renders the next frame, then
; returning if there are any more frames left.
(defun create-canvas-renderer (canvas) {
    (var pos (assoc canvas 'pos))
    (var (bounds-pos bounds-dimensions) (assoc canvas 'bounds))
    (var frames-list (assoc canvas 'frames-list))
    
    ; type: (..(state frames))
    (setq frames-list (map
        (fn (frames) (list
            (list
                (cons 'index 0) ; int|'ended
                (cons 'hold-index nil)
                (cons 'override-color nil)
                
                (cons 'fade-start-relative-index nil)
                (cons 'fade-start-index nil)
                (cons 'fade-length nil)
                (cons 'fade-color-start nil)
                (cons 'fade-color-end nil)
            ) ; state
            frames
        ))
        frames-list
    ))
    
    (var index 0) 
    (var last-start nil)
    (var min-fps 99)
    
    (var stateful-frames-current-frame (fn (stateful-frames) { 
        (var (state frames) stateful-frames)
        (cond
            ((< index (assoc frames 'start-frame)) nil)
            ((eq (assoc state 'index) 'ended) nil)
            (t (ix
                (assoc frames 'frames)
                (assoc state 'index)
            ))
        )
    }))
    
    (fn () 
        (if (all (map
            (fn (stateful-frames)
                (let (
                    ((state _) stateful-frames)
                    (frame (stateful-frames-current-frame stateful-frames))
                ) (or
                    (eq (assoc state 'index) 'ended)
                    (and frame (eq (assoc frame 'hold) 'end)) 
                    (assoc frame 'no-keepalive)
                ))
            )
            frames-list
        )) {
            ; All frames have ended 
            false
        } {
            (var render-placed-frames (fn (stateful-frames) {
                (var (state frames) stateful-frames)
                
                (var frames-pos (assoc frames 'pos))
                (var start-index (assoc frames 'start-frame))
                (var style (assoc frames 'style)) 
                (var opaque-space-str (assoc frames 'opaque-space-str)) 
                (var frame-objects (assoc frames 'frames))
                
                (if (>= index start-index) {
                    (loopwhile t {
                        (match (assoc state 'index)
                            (ended (break ""))
                            ((? current-index) {
                                (var current-frame (ix frame-objects current-index))
                                
                                ; Update general state
                                (cond
                                    ((and
                                        (assoc current-frame 'fade-length)
                                        (not-eq
                                            (assoc state 'fade-start-relative-index)
                                            current-index
                                        )
                                    ) {
                                        (setassoc state 'fade-length (assoc current-frame 'fade-length))
                                        (setassoc state 'fade-start-index index)
                                        (setassoc state 'fade-start-relative-index current-index)
                                        (setassoc state 'fade-color-start (assoc current-frame 'fade-color-start))    
                                        (setassoc state 'fade-color-end (assoc current-frame 'fade-color-end))   
                                        (setassoc state 'override-color (assoc current-frame 'fade-color-start))   
                                    })
                                    ((assoc state 'fade-length) {
                                        (var relative-progress (- index (assoc state 'fade-start-index)))
                                        (if (> relative-progress (assoc state 'fade-length)) {
                                            (setassoc state 'override-color (assoc state 'fade-color-end))
                                            
                                            (setassoc state 'fade-length nil)
                                            (setassoc state 'fade-start-index nil)
                                            (setassoc state 'fade-start-relative-index nil)
                                            (setassoc state 'fade-color-start nil)
                                            (setassoc state 'fade-color-end nil)
                                        } {
                                            (var fraction (/ (to-float relative-progress) (to-float (assoc state 'fade-length))))
                                            (var (type color-start) (assoc state 'fade-color-start))
                                            (var (type color-end) (assoc state 'fade-color-end))
                                            
                                            (setassoc state 'override-color (list type (lerp-color color-start color-end fraction)))
                                        })
                                    })
                                )
                                
                                (var render-current-frame (fn () {
                                    (render-frame
                                        current-frame
                                        (pos+ pos frames-pos)
                                        style
                                        (assoc state 'override-color) 
                                        opaque-space-str 
                                        true
                                    )
                                }))
                                
                                ; Calculate the index for the next iteration
                                (var next-index (cond
                                    ((eq (assoc current-frame 'hold) 'end) {
                                        current-index
                                    })
                                    ((and
                                        (number? (assoc current-frame 'hold))
                                        (!= (assoc current-frame 'hold) 1)
                                        (not (assoc state 'hold-index))
                                    ) {
                                        (setassoc state 'hold-index 0)
                                        current-index
                                    })
                                    ((and
                                        (number? (assoc current-frame 'hold))
                                        (!= (assoc current-frame 'hold) 1)
                                        (assoc state 'hold-index)
                                    ) {
                                        (var next-hold-index (+ (assoc state 'hold-index) 1))
                                        (if (< (+ next-hold-index 1) (assoc current-frame 'hold)) {
                                            (setassoc state 'hold-index next-hold-index)
                                            current-index 
                                        } {
                                            (setassoc state 'hold-index nil)
                                            (+ current-index 1)
                                        }) 
                                    })
                                    ((number? (assoc current-frame 'jump-to)) {
                                        (var jump-to (assoc current-frame 'jump-to))
                                        (if (or (< jump-to 0) (<= (length frame-objects) jump-to))
                                            (exit-error (str-merge
                                                "Invalid index to jump to: "
                                                (str-from-n jump-to)
                                            ))
                                        )
                                        
                                        jump-to
                                    })
                                    (t {
                                        (+ current-index 1)
                                    })
                                ))
                                
                                ; Check if we shouldn't render next index
                                (cond
                                    ((>= next-index (length frame-objects)) {
                                        (setassoc state 'index 'ended) 
                                        
                                        (break (render-current-frame))
                                    })
                                    ((and
                                        (number? (assoc current-frame 'hold))
                                        (= (assoc current-frame 'hold) 0)
                                    ) {
                                        (setassoc state 'hold-index nil)
                                        (setassoc state 'index (+ next-index 1))
                                    })
                                    (t { 
                                        (setassoc state 'index next-index)
                                        (break (render-current-frame))
                                    })
                                )
                            })
                        )
                    })
                }
                    ""
                )
            })) 
            
            (var fps-str (if last-start {
                (var fps (/ 1.0 (secs-since last-start)))
                (if (> min-fps fps) (setq min-fps fps)) 
                ; (setq min-fps (min min-fps fps))
                (str-from-n fps "%.2f fps    ")
            }
                "N/A fps    "
            ))
            (setq last-start (systime))
            
            (puts (str-join (append
                (list
                    (clear-area (pos+ bounds-pos pos) bounds-dimensions)
                    (create-frame (pos+ bounds-pos pos) bounds-dimensions)
                ) 
                (if verbose (list
                    (text-at-pos
                        (pos+ bounds-pos pos (list 0 (+ (second bounds-dimensions) 2)))
                        (str-from-n min-fps "min fps: %.2f")
                    )
                    (text-at-pos
                        (pos+ bounds-pos pos (list 0 (+ (second bounds-dimensions) 1)))
                        fps-str
                    )
                    (text-at-pos
                        (pos+ bounds-pos pos (list 10 (+ (second bounds-dimensions) 1)))
                        (str-merge "frame: " (str-from-n index))
                    ) 
                )
                    (list)
                )
                (map render-placed-frames (reverse frames-list))
            )))
            
            (setq index (+ index 1))
            true
        })  
    )
})

(defun canvas-play (fps canvas) {
    (var render-frame (create-canvas-renderer canvas))
    (every (rate-hz fps)
        (if (not (render-frame)) (break))
    )
})

(defun canvas-from-paths (paths) {
    (var canvas (create-canvas (list 0 0)))
    (loopforeach path paths {
        (canvas-add-frames canvas (load-frames path))
    })
    (canvas-resolve-coordinates canvas)
    canvas
}) 

(defun canvas-center-within-screen (canvas screen-dimensions) {
    (var (bounds-pos bounds-dimensions) (assoc canvas 'bounds))
    (var pos (pos-
        (map to-i (pos-
            (pos/ screen-dimensions 2.0)
            (pos/ bounds-dimensions 2.0)
        ))
        bounds-pos
    ))  
    
    (setassoc canvas 'pos (pos+ pos (list 1 1)))
})

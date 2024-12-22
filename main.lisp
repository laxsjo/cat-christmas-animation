; Print fps and frame information below frame.
(def verbose false)
; (def verbose true)

(defun include (path) {
    (var content nil)
    {
        (var file (fopen path "r"))
        (setq content (load-file file))
    }
    (read-eval-program content)
})

(include "lib/lib.lisp")
(include "lib/tasks.lisp")
(include "lib/terminal.lisp")
(include "lib/animation.lisp")

(defun play-frame (n) {
    (cursor-to-pos)
})

(defunret play () {
    (var screen-dimensions (get-terminal-dimensions))
    (var canvas (canvas-from-paths (list
        "frames/cat.frames"
        "frames/trees.frames"
        "frames/hello.frames"
        "frames/merry-christmas.frames"
    )))
    
    (var rendered-dimensions
        ; Add (2 2) to account for the frame
        (pos+ (canvas-get-dimensions canvas) '(2 2))
    )
    (if (not (all (pos<=
        rendered-dimensions
        screen-dimensions
    ))) {
        (print (str-merge
            "Your screen size ("
            (str-join (map str-from-n screen-dimensions) "x")
            ") is not big enough to fit the animation. :(\n"
            "It must be at least "
            (str-join (map str-from-n rendered-dimensions) "x")
            "."
        ))
        (return nil)
    })
    
    (canvas-center-within-screen canvas screen-dimensions)
    
    (print "Playing animation...")
    (puts (hide-cursor))
    (puts (enable-alternate-buffer))
    (puts (clear-screen))
    
    (canvas-play 12 canvas)
    
    (sleep 1)
    (puts (disable-alternate-buffer))
    (puts (show-cursor))
    t
})

t

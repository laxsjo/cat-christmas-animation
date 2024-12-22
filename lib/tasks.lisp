
(defmacro every (rate expr) `{
    (var start (systime))
    (var rate ,rate)
    (loopwhile t {
        ,expr
        (var processing-time-s (secs-since start))
        (sleep (max 0 (- rate processing-time-s)))
        (setq start (systime))
    })
})

(defun rate-ms (ms) (/ ms 1000.0))
(defun rate-s (s) s)
(defun rate-hz (hz) (/ 1.0 hz))

;; Example usage:
; (every (rate-ms 1000) (print "Hello, World!"))
; (every (rate-s 1) (print "Hello, World!"))
; (every (rate-hz 1) (print "Hello, World!"))

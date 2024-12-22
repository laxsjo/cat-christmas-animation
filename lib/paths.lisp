(defun path-basename (path) {
    (str-part path 0 (str-find path "."))
})

(defun path-filename (path) {
    (var components (str-split path "/"))
    (ix components (- (length components) 1))
})

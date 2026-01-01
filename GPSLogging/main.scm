(define gui #f)

(define gpslabel #f)
(define status-label #f)
(define toggling? #f)
(define elapsed 0.0)
(define logging-active #f)

;; validation
(define (valid-gps-coords? lat lon)
  (and lat lon
       (number? lat)
       (number? lon)))


;; CSV logging
(define (append-gps-row lat lon ts)
  (let* ((path (string-append (system-directory) (system-pathseparator) "gps_log.csv"))
         (rows (if (file-exists? path)
                   (csv-read path)
                   '(("Latitude" "Longitude" "Timestamp")))))
    (csv-write path
      (append rows
              (list (list (number->string lat)
                          (number->string lon)
                          (number->string ts)))))))

;; toggle
(define (logging-loop t)
  (when toggling?
    (set! elapsed (+ elapsed t))
    (when (> elapsed 5000.0)
      (let ((lat (gps-latitude))
            (lon (gps-longitude))
            (ts  (current-seconds)))
        (when (valid-gps-coords? lat lon)
        (glgui-widget-set! gui status-label 'label
        (string-append (float->choppedstring lat 8) ":"
          (float->choppedstring lon 8)))
          (append-gps-row lat lon ts)))
      (set! elapsed 0.0))))

;; button actions
(define (start-action g w t x y)
  (set! toggling? #t)
  (set! elapsed 0.0)
  (set! logging-active #t)
  (glgui-widget-set! gui status-label 'label "Waiting for GPS..."))

(define (stop-action g w t x y)
  (set! toggling? #f)
  (when logging-active
    (set! logging-active #f)
    (glgui-widget-set! gui status-label 'label "Logging stopped")))

;; MAIN
(main
;; INIT
  (lambda (w0 h0)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))

    ;; Title
    (glgui-label gui 120 (- (glgui-height-get) 30) 200 30 "GPS Logging" ascii_18.fnt White)

    ;; Start/Stop buttons
    (glgui-button-string gui 60 220 100 36 "Start" ascii_18.fnt start-action)
    (glgui-button-string gui 180 220 100 36 "Stop" ascii_18.fnt stop-action)

    ;; Exit button
    (glgui-button-string gui 85 180 150 30 "Exit" ascii_18.fnt
         (lambda (g w t x y) (force-terminate)))


   ;; status label
   (let ((lat (gps-latitude))
         (lon (gps-longitude)))
     (if (valid-gps-coords? lat lon)
         (set! status-label (glgui-label gui 80 270 300 30 "Ready" ascii_18.fnt White))
         (set! status-label (glgui-label gui 80 270 300 30 "Starting GPS..." ascii_18.fnt White))))
  )

 ;; EVENT / FRAME
 (lambda (t x y)
   (glgui-event gui t x y)
   ;; background check
   (if logging-active
       (logging-loop t)
       (let ((lat (gps-latitude))
             (lon (gps-longitude)))
         (when (valid-gps-coords? lat lon)
           (glgui-widget-set! gui status-label 'label "Ready")))))

;; TERMINATE
  (lambda () #t)
;; SUSPEND
  (lambda () (glgui-suspend))
;; RESUME
  (lambda () (glgui-resume)))

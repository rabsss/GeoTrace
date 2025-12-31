#|
MIT License

Copyright (c) 2025 Rabina Poudyal

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#


(define gui #f)
(define status-label #f)
(define toggling? #f)
(define elapsed 0.0)
(define logging-active #f)

;; validation
(define (valid-gps-coords? lat lng)
  (and lat lng
       (number? lat)
       (number? lng)
       (not (eq? lat #f))
       (not (eq? lng #f))))


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
          (append-gps-row lat lon ts)))
      (set! elapsed 0.0))))

;; button actions
(define (start-action g w t x y)
  (set! toggling? #t)
  (set! elapsed 0.0)
  (set! logging-active #t)
  (glgui-widget-set! gui status-label 'label "Logging Coordinates..."))

(define (stop-action g w t x y)
  (set! toggling? #f)
  (when logging-active
    (set! logging-active #f)
    (glgui-widget-set! gui status-label 'label "Logging stopped")))

;; MAIN
(main
 ;; INIT
 (lambda (w h)
   (make-window 320 480)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))

   ;; Title
   (glgui-label gui 120 (- (glgui-height-get) 30) 200 30 "GPS Logger" ascii_18.fnt White)

   ;; Exit button
   (glgui-button-string gui 85 180 150 30 "Exit" ascii_18.fnt
     (lambda (g w t x y) (force-terminate)))

   ;; Start/Stop buttons
   (glgui-button-string gui 60 220 100 36 "Start" ascii_18.fnt start-action)
   (glgui-button-string gui 180 220 100 36 "Stop" ascii_18.fnt stop-action)

   ;; status label
   (let ((lat (gps-latitude))
         (lon (gps-longitude)))
     (if (valid-gps-coords? lat lon)
         (set! status-label (glgui-label gui 80 270 300 30 "Ready" ascii_18.fnt White))
         (set! status-label (glgui-label gui 80 270 300 30 "Waiting for GPS..." ascii_18.fnt White))))

   gui)

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

;; MIT License
;;
;; Copyright (c) 2025 Rabina Poudyal
;; Copyright (c) 2025 Benson Muite
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(use-modules (haunt asset)
             (haunt page)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
	         (haunt builder flat-pages)
             (haunt builder redirects)
             (haunt builder rss)
	         (haunt html)
	         (haunt reader)
             (haunt reader skribe)
             (haunt reader texinfo)
             (haunt reader commonmark)
             (haunt site)
	         (haunt utils))

(define test-theme
  (theme #:name "test-theme"
         #:layout
         (lambda (site title body)
           `((doctype "html")
             (head
              (meta (@ (charset "utf-8")))
              (title ,title))
             (body
              (div (@ (class "container"))
                    ,body))))))

(define (static-page title file-name body)
  (lambda (site posts)
    (make-page file-name
               (with-layout test-theme site title body)
               sxml->html)))

(define home-page
  (static-page "GPS Logger" "index.html"
	       `(
		 (h1 "GPS Logger")
		 (p "GPS Logger is an open-source Android application that records
		     your device's GPS location in real-time.  It’s lightweight,
                     accurate, and built using LambdaNative — a cross-platform
                     Scheme-based framework for mobile and embedded applications.")
		 (h2 "Features")
		 (ul
                 (li "Logs GPS coordinates (latitude, longitude)")
                 (li "Timestamped entries for each location fix")
                 (li "Saves data locally in a structured format")
                 (li "Easy to export and visualize with external tools
                       (e.g., " (a (@ (href "https://www.osgeo.org/projects/qgis/"))
				   "qGIS")
                       ", " (a (@ (href "https://earth.google.com/"))
			       "Google Earth)"))
                 (li "Android support via " (a (@ (href "https://www.lambdanative.org/"))
					       "LambdaNative")))
		 (h2 "Download")
		 (p "An Android apk is available at " (a (@ (href "GPSLogging-1.0-android-api28.apk"))
							 "here") ".")
		 (p "Source code is available at " (a (@ (href "https://github.com/rabsss/GPS_Logger"))
						      "https://github.com/rabsss/GPS_Logger")))))

(site #:title "GPS Logger"
      #:domain "rabsss.github.io"
      #:build-directory "build"
      #:default-metadata
      '((author . "Rabina Poudyal"))
      #:readers (list commonmark-reader texinfo-reader skribe-reader sxml-reader html-reader)
      #:builders (list (blog)
		       home-page
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (rss-feed)
		       (static-directory "images")))

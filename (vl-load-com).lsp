(vl-load-com)

(defun vl-string-split (str sep)
  "Custom string split function since VL-STRING-SPLIT is missing."
  (setq result (list) pos 0)
  (while (setq next (vl-string-search sep str pos))
    (setq result (append result (list (substr str (+ pos 1) (- next pos)))))
    (setq pos (+ next (strlen sep))))
  (setq result (append result (list (substr str (+ pos 1)))))
  result)

(defun get-csv-files ()
  "Prompt user to select multiple CSV files."
  (setq fileList (list) filePath "")
  (while filePath
    (setq filePath (getfiled "Select CSV Files (Cancel to stop)" "" "csv" 8))
    (if filePath (setq fileList (cons filePath fileList))))
  fileList)

(defun import-survey-data (filePath)
  "Read survey data from CSV file and create points, labels, and elevation texts."
  (setq file (open filePath "r"))
  (if file
      (progn
        (princ (strcat "\nProcessing file: " filePath "...\n"))
        (while (setq line (read-line file))
          (setq data (vl-string-split line ","))
          (if (and data (>= (length data) 5))
              (progn
                (setq easting (atof (nth 1 data)))  ;; Swapped Easting & Northing
                (setq northing (atof (nth 2 data)))
                (setq elevation (atof (nth 3 data)))
                (setq label (nth 4 data))
                (setq pointXY (list easting northing elevation))

                ;; Create point
                (entmake (list (cons 0 "POINT") (cons 10 pointXY)))
                
                ;; Add text label at the exact point location with -25 rotation
                (entmake (list (cons 0 "TEXT")
                               (cons 8 "Survey_Labels")  ;; Assign to Survey_Labels layer
                               (cons 10 pointXY)  ;; Exact coordinates
                               (cons 40 2.5)  ;; Text height set to 2.5 meters
                               (cons 50 -25)  ;; Rotation angle for labels
                               (cons 1 label)))

                ;; Add elevation text at the same coordinates with 0 rotation
                (entmake (list (cons 0 "TEXT")
                               (cons 8 "Survey_Elevations")  ;; Assign to Survey_Elevations layer
                               (cons 10 pointXY)  ;; Exact coordinates
                               (cons 40 2.5)  ;; Text height set to 2.5 meters
                               (cons 50 0)  ;; Different rotation angle for elevation
                               (cons 1 (rtos elevation 2 2))))) ;; Convert elevation to string with 2 decimals
          )
        )
        (close file)
      )
      (princ (strcat "\nError: Could not open " filePath "!\n"))
  )
)

(defun c:IMPORT_SURVEY ()
  "Main function to import multiple survey CSV files."
  (princ "\nRunning IMPORT_SURVEY command...\n")
  (setq fileList (get-csv-files))
  (if (not fileList)
      (princ "\nNo files selected!\n")
      (foreach filePath fileList
        (import-survey-data filePath)))
  (princ "\nAll survey points imported successfully!\n")
  (princ)
)
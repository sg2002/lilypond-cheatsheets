(use-modules (srfi srfi-1))

(define (is-sharp? note)
  (let ((note-list (string->list note)))
    (if (and (> (length note-list) 1)
             (char=? (cadr note-list) #\i))
        #t #f)))

(define (tone-name note)
  (let ((note-list (string->list note)))
    (if (is-sharp? note)
        (string (car note-list) #\i #\s)
        (string (car note-list)))))

(define (higher-notes note)
  (member (tone-name note)
          (list "c" "cis" "d" "dis" "e" "f" "fis"
                "g" "gis" "a" "ais" "b" "c")))

(define (next-note note)
  (cadr (higher-notes note)))

(define (octave note)
  (let ((note-list (string->list note)))
    (car (string-split (list->string (list-tail note-list
                                                (if (is-sharp? note)
                                                    3 1))) #\\))))

(define (add-octave octave)
  (let ((octave-list (string->list octave)))
    (if (not (null? octave-list))
        (if (char=? (car octave-list) #\,)
            (list->string (list-tail octave-list 1))
            (list->string (append octave-list (list #\'))))
        "'")))

(define (next-octave note)
  (let ((note-list (string->list note)))
    (let ((octave (octave note)))
      (if (char=? (car note-list) #\b)
          (add-octave octave)
          octave))))

(define (add-step note)
  (string-append (next-note note) (next-octave note)))

(define (calculate-octave-value octave)
  (let ((octave-list (string->list octave)))
    (fold (lambda (v result)
            (cond ((char=? v #\')(+ result 12))
                  ((char=? v #\,)(- result 12))))
          0 octave-list)))

(define (calculate-note-value note)
  (+ (- 14 (length (higher-notes note)))
     (calculate-octave-value (octave note))))

(define (note-position note string-root-note frets)
  (let ((pos (-(calculate-note-value note)
               (calculate-note-value string-root-note))))
    (cond ((> pos frets) #f)
	  ((>= pos 0) pos)
	  (else #f))))

(define (note-position-to-staff note pos)
  (string-append note "\\" (number->string pos)))

(define (last-note last-string frets)
  (if (= frets 0)
      last-string
      (last-note (add-step last-string) (- frets 1))))


(define (count-notes first-note last-note counter)
  (if (string=? last-note first-note)
      (+ 1 counter)
      (count-notes (add-step first-note) last-note (+ 1 counter))))

(define (fill-notes staff note note-index tuning frets)
  (do ((s 0 (+ 1 s)))
      ((>= s (length tuning)))
    (let ((pos (note-position note (list-ref tuning s) frets)))
      (if pos
	  (array-set! staff (note-position-to-staff
                             note (- (length tuning) s)) note-index s))))
  (if (string=? note (last-note (last tuning) frets))
      staff
      (fill-notes staff (add-step note) (+ 1 note-index) tuning frets)))

(define (get-empty-staff tuning frets)
  (make-array "r"
              (count-notes
               (first tuning)
               (last-note (last tuning) frets) 0) (length tuning)))

(define (get-notes tuning frets)
  (array->list (fill-notes (get-empty-staff tuning frets)
                           (car tuning) 0 tuning frets)))


(define (generate-octave-number note)
  (- (string-count note #\') (string-count note #\,) 1))

(define (generate-rest)
  (make-music
   'RestEvent
   'duration
   (ly:make-duration 2 0 1)))

(define (generate-pitch note)
  (ly:make-pitch
   (generate-octave-number (octave note))
   (- 8
      (length (filter
               (lambda (e)
                 (if (= (string-length e) 1) #t #f))
               (higher-notes
                (string (car (string->list note)))))))
   (if (is-sharp? note) 1/2 0)))

(define (generate-note note)
  (if (string=? note "r")
      (generate-rest)
      (make-music
       'NoteEvent
       'articulations
       (let ((strnum (string-split note #\\)))
         (if (= (length strnum) 2)
             (list (make-music
                    'StringNumberEvent
                    'string-number
                    (string->number (cadr strnum))))))
       'duration
       (ly:make-duration 2 0 1)
       'pitch
       (generate-pitch note))))


(define (generate-chord notes)
  (make-music
   'EventChord
   'elements
   (map generate-note notes)))

(define (generate-single-note notes)
  (generate-note (car (string-split (find (lambda (x) (not (string=? x "r"))) notes) #\\))))

(define (generate-staff-music chords)
  (make-music
   'SequentialMusic
   'elements
   (map generate-single-note chords)))

(define (generate-tab-staff chords tuning)
  (make-music
   'ContextSpeccedMusic
   'create-new
   #t
   'property-operations
   '()
   'context-type
   'TabStaff
   'element
   (make-music
    'SequentialMusic
    'elements
    (list (make-music
           'ContextSpeccedMusic
           'context-type
           'TabStaff
           'element
           (make-music
            'PropertySet
            'value
            (map generate-pitch (reverse tuning))
            'symbol
            'stringTunings))
          (make-music
           'SequentialMusic
           'elements
           (map generate-chord chords))))))

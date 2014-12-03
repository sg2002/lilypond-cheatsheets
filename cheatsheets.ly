\version "2.18.2"

% The instrument should be defined here:
tuning = #'("e," "a," "d" "g" "b" "e'")
frets = 24

#(load "cheatsheets-fns.scm")

notes = #(get-notes tuning frets)
staff-music = #(generate-staff-music notes)
tab-staff = #(generate-tab-staff notes tuning)

offset = 3
#(define (extract-note-name str)
   (let* ((lst (string->list str))
          (hd (take-while char-alphabetic? lst)))
     (list->string hd)))

#(define (note-name-stencil grob) 
   (let* ((text (ly:grob-property grob 'text))
          (name (string-upcase (extract-note-name text)))
          (up-count (string-count text #\'))
          (down-count (string-count text #\,))
          (num
           (cond
            ((> up-count 0) (+ up-count offset))
            ((> down-count 0) (- offset down-count))
            (else offset)))
          (num-text (number->string num))
          (name (string-append name num-text)))  
     (grob-interpret-markup grob name)))

%  Landscape is better for our needs.
#(set-default-paper-size "a4" 'landscape)
% Makes the whole score larger.
#(set-global-staff-size 29)
\layout { indent = 0.0\cm } % Removes indentation for the first bar.
\header { tagline = "" } % Removes lilypond tagline.

<<
  \new Staff {
    <<
      % Clef should be selected here
      % Guitars normally use treble_8
      \clef "treble_8"
      \staff-music
      \context NoteNames {
        \set printOctaveNames = ##t
        % If you want to use lilypond notation for note names
        % Comment out the next line
        \override NoteName.stencil = \note-name-stencil
        \staff-music
      }
      % Removes bar numbers
      \override Score.BarNumber.break-visibility = ##(#f #f #f)
    >>
  }
  \tab-staff
>>
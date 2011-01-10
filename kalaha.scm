(use srfi-1 srfi-18 mojo ncurses ncurses-more coops miscmacros tcp matchable)

(define (w x) (inexact->exact (round (* 7 x))))
(define (h x) (inexact->exact (round (* 4 x))))

(define-class <pit> (<win>)
  ((name: accessor: pit-name)
   (width: (w 1))
   (height: (h 1))
   (highlight: accessor: pit-highlight initform: #f)
   (owner: accessor: pit-owner)
   (count: accessor: pit-count initform: 3)
   (next: accessor: pit-next)
   (prev: accessor: pit-prev)
   (opposite: accessor: pit-opposite initform: #f)))

(define-class <kalaha> (<pit>)
  ((width: (w 2))
   (height: (h 2))
   (left: 1)
   (top: 1)
   (count: 0)))

(define (pit? pit)
  (eq? <pit> (class-of pit)))

(define (kalaha? pit)
  (eq? <kalaha> (class-of pit)))

(define (make-pits count init-prev owner #!key top relation left right)
  (cdr
   (reverse
    (fold (lambda (i pits)
            (let* ((prev (car pits))
                   (pit (make <pit>
                          owner: owner
                          name: i
                          relation prev
                          prev: prev
                          left: left
                          right: right
                          top: top)))

              
              (set! (pit-next prev) pit)
              (cons pit pits)))
          (list init-prev)
          (iota count)))))

(define kalaha-opponent
  (make <kalaha> 
    owner: 'opponent
    name: 'opponent))

(define pits-player (make-pits 6 kalaha-opponent 'player
                               relation: #:right-of
                               top: (+ 1 (h 1))
                               left: 1))

(define kalaha-player
  (make <kalaha>
    owner: 'player
    name: 'player
    right-of: (last pits-player)
    prev: (last pits-player)
    left: 1
    top: 1))

(set! (pit-next (last pits-player)) kalaha-player)

(define pits-opponent (make-pits 6 kalaha-player 'opponent
                                 relation: #:left-of
                                 top: 1
                                 right: 1))

(for-each (lambda (p o)
            (set! (pit-opposite p) o)
            (set! (pit-opposite o) p))
          pits-player
          (reverse pits-opponent))

(set! (pit-next (last pits-opponent)) kalaha-opponent)
(set! (pit-prev kalaha-opponent) (last pits-opponent))

(define-method (draw-win after: (pit <pit>) height width top left)
  (let ((text (number->string (pit-count pit))))
    (mvwaddstr (win-pointer pit)
               (inexact->exact (round (- (/ height 2) 1)))
               (inexact->exact (round (- (/ width 2) (/ (string-length text) 2))))
               text)))

(define-method (print-object (pit <pit>) #!optional (out (current-output-port)))
  (fprintf out "#<~a " (select (class-of pit)
                         ((<pit>) 'pit)
                         ((<kalaha>) 'kalaha)))

  (display (pit-owner pit) out)

  (select (class-of pit)
    ((<kalaha>) (display ">" out))
    ((<pit>) (fprintf out " ~a>" (slot-value pit #:name)))))

(define focus #f)

(define-method (draw-win before: (pit <pit>) . args)
  (let ((win (win-pointer pit)))
    (when (eq? focus pit)
      (wattron win (COLOR_PAIR 1)))))

(define-method (draw-win around: (pit <pit>) . args)
  (if (pit-highlight pit)
      (let ((win (win-pointer pit)))
        (wattron win (COLOR_PAIR 2))
        (call-next-method)
        (wattroff win (COLOR_PAIR 2))
        (set! (pit-highlight pit) #f))
      (call-next-method)))

(define-method (update-win after: (pit <pit>) . args)
  (wstandend (win-pointer pit)))

(define (set-focus! win)
  (when win
    (set! focus win)))

(define (find-pit player from dir)
  (let loop ((pit (dir from)))
    (and (eq? (pit-owner pit) player)
         (not (kalaha? pit))
         (if (zero? (pit-count pit))
             (loop (dir pit))
             pit))))

(define (make-turn player kalaha pit)
  (let loop ((count (pit-count pit)) 
             (npit (pit-next pit)))

    (let ((next (lambda (drop)
                  (when drop
                    (dec! (pit-count pit))
                    (inc! (pit-count npit)))

                  (set! (pit-highlight npit) #t)
                  (redraw)
                  (thread-sleep! .5)                  

                  (loop (if drop (sub1 count) count)
                        (pit-next npit)))))

      (cond ((zero? count)
             (let* ((prev (pit-prev npit))
                    (player-owns? (eq? player (pit-owner prev))))
               
               (when player-owns?
                 (let ((opposite-pit (pit-opposite prev)))
                   (when (and opposite-pit
                              (= 1 (pit-count prev))
                              (> (pit-count opposite-pit) 0))
                     (set! (pit-highlight opposite-pit) #t)
                     (inc! (pit-count kalaha) (add1 (pit-count opposite-pit)))
                     (set! (pit-count opposite-pit) 0)
                     (set! (pit-count prev) 0)
                     (redraw)
                     (thread-sleep! .5))))
               
               (and player-owns? (kalaha? prev))))
            ((eq? (pit-owner npit) player)
             (next #t))
            ((kalaha? npit)
             (next #f))
            (else (next #t))))))

(define (make-random-opponent-turn)
  (let ((opp (filter (lambda (p) (> (pit-count p) 0)) pits-opponent)))
    (unless (null? opp)
      (let ((pit (car (shuffle opp random))))
        (set-focus! pit)
        (make-turn 'opponent kalaha-opponent pit)))))

(define game-over #f)

(define (game-over? #!optional pits)
  (if pits
      (every (o zero? pit-count) pits)
      (or game-over
          (begin
            (when (or (game-over? pits-player)
                      (game-over? pits-opponent))
              (finish-game!)
              (set! game-over #t))
            game-over))))

(define (finish-game!)
  (cond ((= (pit-count kalaha-player) 
            (pit-count kalaha-opponent))
         (set-status! "It's a draw!"))
        ((> (pit-count kalaha-player)
            (pit-count kalaha-opponent))
         (set-status! "You win!"))
        (else (set-status! "You lose!"))))

(define (play-player)
  (when remote-game?
    (fprintf remote-out "~A~%" (pit-name focus)))

  (if (make-turn 'player kalaha-player focus)
      (refocus-player!)
      (switch-current-turn!)))

(define (refocus-player!)
  (set-focus! (find-pit 'player kalaha-opponent pit-next)))

(define (wait-for-remote-opponent-turn)
  (let* ((name (string->number (read-line remote-in)))
         (pit (find (lambda (pit) (eq? (pit-name pit) name)) 
                    pits-opponent)))
    (set-focus! pit)
    (make-turn 'opponent kalaha-opponent pit)))

(define (play-opponent)
  (if (if remote-game?
          (wait-for-remote-opponent-turn)
          (make-random-opponent-turn))
      (set! focus #f)
      (switch-current-turn!)))

(define (switch-current-turn!)
  (case current-turn
    ((opponent)
     (set-current-turn! 'player)
     (refocus-player!))
    (else
     (set-current-turn! 'opponent)
     (set! focus #f))))

(define-class <status-win> (<win>)
  ((text "")))

(define status
  (make <status-win> 
    below: kalaha-opponent
    right-of: kalaha-opponent
    left-of: kalaha-player
    left: 1
    right: 1
    height: 3))

(define-method (draw-win after: (win <status-win>) height width . args)
  (let* ((text (slot-value win 'text))
         (x (inexact->exact (round (- (/ width 2) (/ (string-length text) 2))))))
    (mvwaddstr (win-pointer win) 1 x text)))

(define-method (set-status! text)
  (set! (slot-value status 'text) text))

(define current-turn #f)
(define player-name #f)
(define opponent-name "Opponent")
(define remote-game? #f)
(define remote-socket #f)
(define remote-in #f)
(define remote-out #f)

(define (set-current-turn! turn)
  (set! current-turn turn)
  (set-status!
   (if (eq? 'player current-turn)
       "Your turn"
       (format "~A's turn" opponent-name))))

(match (command-line-arguments)
  (("host" port . name) 
   (set! player-name (string-intersperse name " "))
   (set! remote-socket (tcp-listen (string->number port)))
   (print (format "waiting for connection on port ~a" port))
   (set! remote-game? #t)
   (receive (in out)
       (tcp-accept remote-socket)
     (set! remote-in in)
     (set! remote-out out)
     (set! opponent-name (read-line in))
     (fprintf out "~A~%" player-name)
     (set-current-turn! 'player)))
  (("join" host port . name)
   (set! player-name (string-intersperse name " "))
   (set! remote-game? #t)
   (receive (in out)
       (tcp-connect host (string->number port))
     (set! remote-in in)
     (set! remote-out out)
     (fprintf out "~A~%" player-name)
     (set! opponent-name (read-line in))
     (set-current-turn! 'opponent)))
  (else (set-current-turn! 'player)))

(initialize)
(start_color)
(use_default_colors)
(init_pair 1 COLOR_RED COLOR_BLACK)
(init_pair 2 COLOR_YELLOW COLOR_BLACK)

(when (eq? 'player current-turn)
  (set-focus! (car pits-player)))

(let loop ()
  (redraw)
  (flushinp)

  (cond ((game-over?)
         (redraw)
         (select (get-char)
           ((#\q) (exit))))
        ((eq? 'opponent current-turn)
         (play-opponent))
        (else 
         (select (get-char)
           ((key-left)
            (set-focus! (find-pit 'player focus pit-prev)))
           ((key-right)
            (set-focus! (find-pit 'player focus pit-next)))
           ((#\return)
            (play-player))
           ((#\q)
            (exit)))))

  (loop))
(defparameter *wizard-allowed-commands* '(look walk pickup inventory reset))

(defparameter *wizard-nodes*
  '((living-room (you are in the living-room.
                      a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden.
                 there is a well in front of you.))
    (attic (you are in the attic.
                there is a giant welding torch in the corner.))))

(defparameter *wizard-edges*
  '((living-room (garden west door)
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defparameter *wizard-objects* '(whiskey bucket frog chain))

(defparameter *wizard-object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *wizard-location* 'living-room)

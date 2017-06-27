(keymaster-data

 :instruments
 (('inputs
   ("MIDISPORT 4x4 Anniv Port A" 'kk "Kronos")
   ("MIDISPORT 4x4 Anniv Port B" 'ws "Wavestation"))
  ('outputs
   ("MIDISPORT 4x4 Anniv Port A" 'kk "Kronos")
   ("MIDISPORT 4x4 Anniv Port B" 'ws "Wavestation")))

 :messages
  (('tune-request (16rf6))
   ('multiple-note-offs ((16r80 64 0)
                         (16r81 64 0)
                         (16r82 42 0))))
 :triggers ()

 :songs
 (("To Each His Own" "optional song notes"
   ("Vanilla Through"
    ('kk 'all 'kk 'all)
    ('ws 'all 'ws 'all)))

  ("Kronos -> Wavestation"
   ("patch K to W"
    ('kk 'all 'ws 'all)
    ('ws 'all 'kk 'all))))

 :set-lists
 (("Tonight's Set List"
   ("To Each His Own"
    "Kronos -> Wavestation"))
  ("Tomorrow's"
   ("To Each His Own"
    "Kronos -> Wavestation"))))

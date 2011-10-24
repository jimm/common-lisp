(w/ffi "osx_midi"
  (cdef osxm-init "init_osx_midi" cvoid ())
  (cdef osxm-num-sources "num_sources" cint ())
;;  (cdef osxm-num-destinations "num_destinations" culong ())
)

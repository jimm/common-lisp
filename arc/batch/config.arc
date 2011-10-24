;; configuration
(= db* "backoffice_dev_jimm"
   db-host* "localhost" db-user* "osc_dev" db-pwd* "Hello*Kittie"
   source-dir* "/tmp/bulk-clip-creation/"
   dest-dir* "/media/audio/backoffice-media/"
   script-dir* "/usr/local/jboss/server/default/scripts/")

(def source-dir (amp) (string source-dir* amp "/"))
(def completed-dir (amp) (string source-dir* amp "-done/"))
(def master-dir (amp) (string dest-dir* "master/" amp "/"))
(def module-dir (amp) (string dest-dir* "modules/" amp "/"))
(def program-dir (amp) (string dest-dir* "programs/" amp "/"))
(def script-dir () script-dir*)

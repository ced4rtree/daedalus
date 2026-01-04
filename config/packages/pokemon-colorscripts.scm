(define-module (config packages pokemon-colorscripts)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix licenses))

(define-public pokemon-colorscripts
  (let ((url "https://github.com/nuke-dash/pokemon-colorscripts-mac"))
    (package
     (name "pokemon-colorscripts")
     (version "1.0")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url url)
             (commit "6aa0cd93b255bee35c5716652b8b7dfecb5fcfa2")))
       (sha256
        (base32
         "06b86qy2fpzdd81n2mscc2njkrxx0dyzxpgnm1xk6ldn17c853lc"))))
     (build-system copy-build-system)
     (arguments (list #:install-plan #~'(("colorscripts" "local/opt/pokemon-colorscripts/")
                                         ("pokemon-colorscripts.sh" "local/opt/pokemon-colorscripts/")
                                         ("nameslist.txt" "local/opt/pokemon-colorscripts/"))
                      #:phases #~(modify-phases %standard-phases
                                                (add-after 'install 'symlink-bin-file
                                                           (lambda _
                                                             (mkdir (string-append #$output "/bin"))
                                                             (symlink
                                                              (string-append (ungexp output) "/local/opt/pokemon-colorscripts/pokemon-colorscripts.sh")
                                                              (string-append (ungexp output) "/bin/pokemon-colorscripts"))
                                                             #t))
                                                (add-before 'install 'fix-wrong-commands
                                                            (lambda _
                                                              (substitute* "pokemon-colorscripts.sh"
                                                                           (("greadlink") "readlink")
                                                                           (("gshuf") "shuf"))
                                                              #t)))))
     (home-page url)
     (synopsis "Some scripts to print out images of pokemons to terminal.")
     (description "Prints out colored unicode sprites of pokemon onto
your terminal. Contains almost 900 pokemon from gen 1 to gen 8. Has
all the pokemons you could ever want (okay it doesn't have shiny
pokemon or different forms, but cut me some slack)")
     (license expat))))

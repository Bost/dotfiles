(define-module (cfg fish)
  #:export (fish-functions fish-plugins-functions))

(define fish-functions
  ;; (scandir (string-append (getenv "HOME") "/dev/dotfiles/fish/functions/")
  ;;      (lambda (f) (string-match "\\S{1,}\\.fish$" f)))
  (list
   ".....fish"
   "....fish"
   "...fish"
   "armv.fish"
   "b.fish"
   "bin.fish"
   "cd-.fish"
   "cdd.fish"
   "cheat.fish"
   "cl.fish"
   "cls.fish"
   "coc.fish"
   "corona.fish"
   "cpr.fish"
   "ctd.fish"
   "cvs-reset.fish"
   "d2u.fish"
   "dec.fish"
   "der.fish"
   "desk.fish"
   "desktop.fish"
   "dev.fish"
   "dir.fish"
   "dos2unix.fish"
   "dotf.fish"
   "down.fish"
   "eac.fish"
   "ec.fish"
   "edit-grub.fish"
   "el.fish"
   "ema.fish"
   "emag.fish"
   "envp.fish"
   "ext.fish"
   "extract.fish"
   "fc.fish"
   "fclj.fish"
   "fdk.fish"
   "ff.fish"
   "filename.fish"
   "fjar.fish"
   "fjava.fish"
   "foo.fish"
   "fxml.fish"
   "g.fish"
   "ga.fish"
   "gaa.fish"
   "gad.fish"
   "gap.fish"
   "gau.fish"
   "gb.fish"
   "gbb.fish"
   "gbg.fish"
   "gbir.fish"
   "gbis.fish"
   "gbr.fish"
   "gbra.fish"
   "gbrd.fish"
   "gbrm.fish"
   "gc.fish"
   "gci.fish"
   "gcia.fish"
   "gcianoe.fish"
   "gcim.fish"
   "gcl.fish"
   "gco-.fish"
   "gco.fish"
   "gcob.fish"
   "gcod.fish"
   "gcom.fish"
   "gcoo.fish"
   "gcov.fish"
   "gcp.fish"
   "gdd.fish"
   "gdf.fish"
   "genpasswd.fish"
   "getopts.fish"
   "gfe.fish"
   "gfeo.fish"
   "gfeu.fish"
   "gh.fish"
   "ghe.fish"
   "gl.fish"
   "gla.fish"
   "glf.fish"
   "glg.fish"
   "glh.fish"
   "gmv.fish"
   "goodies.fish"
   "gr.fish"
   "gra.fish"
   "grb.fish"
   "grba.fish"
   "grbc.fish"
   "grbi.fish"
   "grbs.fish"
   "grc.fish"
   "grepc.fish"
   "grepo.fish"
   "grepp.fish"
   "grh.fish"
   "gri.fish"
   "grm.fish"
   "grs.fish"
   "gs.fish"
   "gsh.fish"
   "gshp.fish"
   "gss.fish"
   "gst.fish"
   "gstr.fish"
   "gtg.fish"
   "h.fish"
   "h1.fish"
   "h2.fish"
   "h3.fish"
   "h4.fish"
   "h10.fish"
   "h20.fish"
   "h30.fish"
   "h40.fish"
   "he.fish"
   "hib.fish"
   "hibernate.fish"
   "hoch.fish"
   "hrep.fish"
   "ifconfig.fish"
   "inst.fish"
   "k9.fish"
   "kill-buffers.fish"
   "lat.fish"
   "latest.fish"
   "latr.fish"
   "latte.fish"
   "launch.fish"
   "lc.fish"
   "lca.fish"
   "lcad.fish"
   "lcc.fish"
   "lco.fish"
   "lct.fish"
   "lcxa.fish"
   "ldir.fish"
   "lf.fish"
   "lff.fish"
   "lg.fish"
   "lga.fish"
   "lh.fish"
   "li.fish"
   "ll.fish"
   "lock.fish"
   "loff.fish"
   "lr.fish"
   "lT.fish"
   "lt.fish"
   "ltr.fish"
   "lx.fish"
   "m.fish"
   "make-emacs.fish"
   "make-git.fish"
   "math.fish"
   "mkcd.fish"
   "mount-usb.fish"
   "music.fish"
   "netstat.fish"
   "notes.fish"
   "nslookup.fish"
   "nt.fish"
   "owid.fish"
   "png.fish"
   "prep.fish"
   "purge.fish"
   "reboot.fish"
   "reinst.fish"
   "rmrf.fish"
   "rmv.fish"
   "rr.fish"
   "script.clj"
   "server.clj"
   "shut.fish"
   "shutdown.fish"
   "spa.fish"
   "susp.fish"
   "synaptic.fish"
   "t.fish"
   "take.fish"
   "telnet.fish"
   "tf.fish"
   "time.fish"
   "timestamp.fish"
   "tmp.fish"
   "trackle.fish"
   "u.fish"
   "u2d.fish"
   "ufo.fish"
   "umount-usb.fish"
   "unexport.fish"
   "unix2dos.fish"
   "unset.fish"
   "utils.fish"
   "v.fish"
   "vdir.fish"
   "vesmir.fish"
   "wp.fish"
   "y.fish"
   "yas.fish"
   "ys.fish"
   "zark.fish"
   ))

(define fish-plugins-functions
  (list
   "_tide_detect_os.fish"
   "_tide_find_and_remove.fish"
   "_tide_item_character.fish"
   "_tide_item_chruby.fish"
   "_tide_item_cmd_duration.fish"
   "_tide_item_context.fish"
   "_tide_item_git.fish"
   "_tide_item_go.fish"
   "_tide_item_jobs.fish"
   "_tide_item_kubectl.fish"
   "_tide_item_newline.fish"
   "_tide_item_node.fish"
   "_tide_item_os.fish"
   "_tide_item_php.fish"
   "_tide_item_rustc.fish"
   "_tide_item_shlvl.fish"
   "_tide_item_status.fish"
   "_tide_item_time.fish"
   "_tide_item_vi_mode.fish"
   "_tide_item_virtual_env.fish"
   "_tide_print_item.fish"
   "_tide_prompt.fish"
   "_tide_pwd.fish"
   "_tide_remove_unusable_items.fish"
   "_tide_sub_bug-report.fish"
   "_tide_sub_configure.fish"
   "fish_mode_prompt.fish"
   "fish_prompt.fish"
   "fisher.fish"
   "tide.fish"
   ))

## sound-series

An exercise in musical notation, used for teaching a six-year-old the basics.
Two example generated audio files are provided.

Depends on [Common Music
2.6.0](https://sourceforge.net/projects/commonmusic/files/cm/2.6.0/),
[Common Lisp Music 4](https://ccrma.stanford.edu/software/clm/)
and [Series
2.2.10](http://sourceforge.net/projects/series/files/series/series-2.2.10/).

To run, set up the dependences in ASDF and then `(load "scratch")`
(tested with SBCL).

Emacs add-ons:

```
(defun mplayer-test ()
  (interactive)
  (shell-command "mplayer test.snd > /dev/null &" nil nil))`
(global-set-key [f6] 'mplayer-test)
```

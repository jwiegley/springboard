How many times have you wanted to fire off a quick command, such as M-!, but
the directory you want to run the command in isn't the same as the directory
of the current buffer?  In those situations, you want a quick way to change
the default-directory *for only the next command*.  That is what Springboard
aims to solve.

There are three packages in this repo, any which of you may require and use:
1. springboard package, springboard.el

   This package works by integrating with Helm.
   
   Bind it to a convenient key, like `Control-.`, and after you press it you'll
   see a handy Helm buffer showing the directories of all the files you've
   recently visited, plus the permanent directory list from
   `springboard-directories` -- a good place to list your active project
   directories.

2. ido-springboard package, ido-springboard.el

   This package works by advising ido-switch-buffer

3. ivy-springboard package, ivy-springboard.el

   This package works by advising ivy-switch-buffer

In any of the three packages above, type a few chars to narrow down to the
directory of interest, then just type your command, like `M-!`, `C-x C-f`, or
whatever custom bindings you may have to run PCVS, Magit, etc. The moment you
type your command, Springboard disappears, and if your command needs minibuffer
input, you'll now be in the minibuffer for that new command.

Here is an example use-package form for using ivy-springboard package:

```
(use-package ivy-springboard
   :demand t
   :load-path "/path/to/springboard directory")
```

WARNING:  I am just learning git, so the following could have mistakes.
      For now, I am making backups of the repository regularly
      in case anyone, including myself, screws it up. Jon

NOTE: To get the files you need to be working on a machine that has a
    current version of git installed. To build wrf you need to have the
    requisite libraries installed and environment set up. 
    See the user guide linked from http://openwfm.org/software.html
    for further information.

***************************************
*The big picture
***************************************

First you clone to create a local repository. The first time you create
your own branch as a copy of master. If you already have a
branch in the archive you create a local branch as a copy of that. 
To get new files pull from master in the archive. You work all the time in your
local branch. Then push your branch to the archive.

***************************************
*Things to remember
***************************************

Everybody:
1. make sure you are in your branch by git branch before you modify anything
2. merge between repositories is by pull only, never by push
3. before and after git pull or merge use git log to make sure where you are
4. the first line from git log identifies the state of all files uniquely
5. before pull you must commit all changes (=to your local repository)
6. accumulated changes cause edit conflicts - merge with master often!!

Developers with write access:
7. only after you push your committed changes will be visible to others
8. your changes will get merged into master only when you ask Jon to do that


***************************************
*Initial command list (see explanations below)*
***************************************

1.  Initial setup (substitute your information)

  git config --global user.name "FirstName LastName"
  git config --global user.email "user@example.com"

  (You need to do this only once)

2.  Local repository setup

  a) Local developers with write access: make sure ssh math.ucdenver.edu works
  and you are in group mandel there. Then:

    git clone ssh://math.cudenver.edu/home/grads/jbeezley/wrf-fire.git

  b) Everyone else, read access only:

    git clone git://github.com/jbeezley/wrf-fire.git

  In either case, this will create a directory wrf-fire with the files. The cloned 
  repository will be created in a hidden directory wrf-fire/.git

3.  Create your branch:
  (Substitute <branch> with what you want to name your branch.)
  (If you have write access, please use branch names of the form <xy>/<branchname>,
  where xy are your initials, so as not to clutter the repository.)

  cd ./wrf
  git branch <branch>
  git checkout <branch>

4.  Updating <branch> from the shared repository, similar to "cvs update":

  git pull origin master:master 
  git branch  (to make sure you are on your own branch)
  git checkout <branch> (if necessary)
  git merge master

5. Commit your changes:
  git add <filename>   (to add any new files)
  git commit -a  (do not forget the flag -a)

6. If you have write acces, copy your changes from the local repository
  to the share repository:

  git push origin <branch>:<branch>

  (The first time, next time you can try the short form, just git push)

7. To share your changes:

  a) If you have write access: ask Jon to merge your branch into master
   and tell him which commit exactly (first line from git log)

  b) If you have read-only access: email Jon for instructions how to 
   create and email a patch file with your changes

***********************************
*Explanation of the commands above*
*and additional useful information*
***********************************

1.  Initial setup

  git requires that each user be identified with a name and email.
  Or edit ~/.gitconfig

2.  Local repository setup

  Only changes committed in the main repository by git push can be seen
  by others. git commit works with your local repository. This
  allows you work offline and prevents your changes from being seen by
  others until you are ready.

3.  Branches

  git branch <branch>

  This will create your branch starting from the commit where you currently are
  (the latest commit on the master branch by default).

  git checkout <branch>

  This will make sure you are on your own branch.

  More about branches
  -------------------

  Our shared repository contains several branches, by default you will
  checkout the "master" branch.  This is meant to be the stable branch,
  and only Jon may git push to it. Branches with "origin" are in the
  shared repository.

  To get a list of local branches and see what branch you are on

    git branch

  To see remote branches available

    git branch -r

  or to see all branches

    git branch -a

  Among others, you should see 'origin/jbeezley'.  To use this branch
  you need to create your private clone in your local repository by

    git checkout -b jbeezley origin/jbeezley

  This will create a local branch, jbeezley, that will automatically be
  updated by the remote branch, jbeezley, on the shared repository.
  "origin" is the default name of the shared repository.

  The first command creates a local branch starting from "master".  The
  second command commits (pushes) this branch to the shared repository.  

  To update your local copy of the jbeezley branch, you can do

  git checkout jbeezley
  git pull   (git will know you want to pull from origin/jbeezley)

  and you are on the jbeezley branch at this point. Or, you can

  git pull origin jbeezley:jbeezley

  (Of course you can do all that with any other branch, not just jbeezley.)

  To switch to a different local branch

  git checkout <branch>

4.  Updating the repository

  To synchronize the local repository with the shared one do

    git fetch

  This will not change your files, only the local repository.
  Note that git pull is equivalent to git fetch; git merge

  To get rid of all local changes (like cvs update -C)

    git checkout -f

5.  Committing changes

  If you create any new files, you must tell git about it

    git add <new file>

  All new files in the directory tree can be added with

    git add .

  (see also 'git rm' and 'git mv') Finally, the changes can be
  committed with

    git commit -a

  You have to be somewhere in the wrf directory, but it does no matter
  where, the wrf directory and subdirectories are always committed.

  This will only commit to your local repository.  To add your commits to
  *your branch* in the shared repository

    git push origin <branch>:<branch>

  Ask Jon to add your commits to the master branch.

  Advanced:

    git push origin <local branch>:<destination branch>

  Because this will modify the shared repository, only use a destination
  branch which you control.  There is a way to automate this so that the
  branches don't have to be specified (the --track flag may actually take
  care of this), but I'm not sure how this works.  For now, I suggest
  specifying the branches to be safe. 

6.  Deleting a remote branch

  A remote branch can be deleted by pushing "nothing" to the branch.  For
  example, if you want to delete the remote branch "oldbranch" on origin, 
  you would do:

  git push origin :oldbranch

7.  Reverting local changes

  If you make a change to some files, and you wish to revert those files to
  a state as in the repository, you can do this with git checkout.  To revert
  all files and restore the state to the last commit:

  git checkout -f HEAD

  To revert only a single file (<filename>) and to keep all other changes intact:

  git checkout-index -f <filename>

  Remember, git by default won't let you do anything that will lose any
  changes that you've made.  Generally, git commands support a '-f' flag that
  will allow it over-write any changes that you have made; however, once you
  do this, you will not be able to get the changes back.  If you really don't
  want to lose these changes, then you can just create a temporary local
  branch to keep them in.

  git branch <tempbranch>
  git checkout <tempbranch>
  git commit -a

  This way you can get back to the original branch with the usual git checkout
  command, and you won't lose the changes.  If, later, you wish to put these
  changes into your main branch (<mainbranch>):

  git checkout <mainbranch>
  git merge <tempbranch>

  You can delete the temporary branch at any time with:

  git branch -d <tempbranch>

8.  Other commands

  For a log of everything you have done in your repository, see the command
  reflog.
  You can undo changes with reset.  The cvs style commands status, log, and
  diff also work in git.

9. diff with other branch:

  git fetch                       (to make sure you have current repository)
  git diff <branch>               (for example git diff origin/jb/fire_da)

10.  Getting help

  man git : Overview of git commands
  man git-commit : man page for "git commit" (replace commit with other commands as well)

  Much of this document was based on the web page:

    http://wiki.sourcemage.org/Git_Guide

  Crash course:

    http://git.or.cz/course/svn.html

  There is also a simplified interface to git known as cogito, it is similar to cvs,
  but I haven't looked at it.

    http://git.or.cz/course/cvs.html

  Documentation of common commands

    http://www.kernel.org/pub/software/scm/git/docs/everyday.html

  Full documentation

    http://www.kernel.org/pub/software/scm/git/docs/user-manual.html

11. Magic sequences for specific tasks

  Magic sequence to see files from somebody else's branch

    git remote update
    git branch -r     (to see the list of branches, then pick the <other_branch> name)
    git branch
    if the <other_branch> is not there then (first time only)
        git checkout --track -b <other_branch> origin/<other_branch>
    else
        git checkout <other_branch>
    end
    [now look at the files]
    git checkout <your_own_branch>
    git branch        (look for * to make sure you are on your own branch)

    Note: it may be better to set up a separate clone (see the top) for that.
    Each clone has its own local repository.

  Magic sequence to get all updates from master brach from the remote
  repository and merge them into your branch:

    git checkout <branch>
    git pull origin master:master
    git merge master
    git push origin <branch>:<branch>

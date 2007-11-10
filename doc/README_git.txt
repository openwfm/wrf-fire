WARNING:  I am just learning git, so the following could have mistakes.
      For now, I am making backups of the repository regularly
      in case anyone, including myself, screws it up. Jon

NOTE: To get the files you need to be working on a machine that has a
    current version of git installed. To build wrf you need to have the
    requisite libraries installed and environment set up. Both
    conditions are satisfied on wf.cudenver.edu and opt4.cudenver.edu
    it is strongly recommended to work on these two machines only, at
    least from the start.

Using the repository:

***************************************
*Initial command list (see explanations below)*
***************************************

This will create a directory ./wrf under your current directory.

1.  Initial setup (substitute your information)

  git config --global user.name "FirstName LastName"
  git config --global user.email "user@example.com"

2.  Local repository setup

  Make sure that you have an account on math.cudenver.edu and you are in
  group mandel there. Then:

    git clone ssh://math.cudenver.edu/home/grads/jbeezley/wrf.git

  This will create a directory ./wrf with the files. The cloned repository
  will be created in a hidden directory in ./wrf

3.  Branch setup (substitute <branch> with what you
                  want to name your branch)

  cd ./wrf
  git checkout -b <branch> origin/master
  git push origin <branch>:<branch>

4.  "cvs update" (updating <branch> from the shared repository)

  git checkout <branch>
  git pull

5.  "cvs commit" (committing <branch> to the shared repository)

  git commit -a
  git push origin <branch>:<branch>


***********************************
*Explanation of the commands above*
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

    git checkout --track -b jbeezley origin/jbeezley

  This will create a local branch, jbeezley, that will automatically be
  updated by the remote branch, jbeezley, on the shared repository.
  "origin" is the default name of the shared repository. (You can omit
  the --track flag if your version of git objects, some do.)

  You should create your own branch at this point, (substitute <branch>
  with what you wish to name your branch).

    git checkout -b <branch> origin/master
    git push origin <branch>:refs/heads/<branch>

  The first command creates a local branch starting from "master".  The
  second command commits (pushes) this branch to the shared repository.  
  The "refs/heads/" part in the remote branch is now necessary in order
  to create a new branch in a remote repository.  This is known as a 
  long branch reference, as opposed to the short branch reference "<branch>".
  While long branch references will work in any git operation, they are
  only necessary in creating a remote branch.

  To switch to a different local branch

    git checkout <branch>

4.  Updating the repository

  You can check for new branches in the shared repository by

    git remote update
    git branch -r

  To update a branch, change to that branch and update with

    git pull

  This is like 'cvs update', but this will not restore deleted files,
  for that you need

    git checkout-index -a

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

7.  Other commands

  For a log of everything you have done in your repository, see the command
  reflog.
  You can undo changes with reset.  The cvs style commands status, log, and
  diff also work in git.

8. diff with other branch:

  git fetch                       (to make sure you have current repository)
  git diff <branch>               (for example git diff origin/jb/fire_da)

9.  Getting help

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

10. Magic sequences for specific tasks

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

WARNING:  I am just learning git, so the following could have mistakes.
	  For now, I am making backups of the repository regularly
	  in case anyone, including myself, screws it up.

Using the repository:

***************************************
*Command list (see explanations below)*
***************************************

1.  Initial setup (substitute your information)

  git config --global user.name "FirstName LastName"
  git config --global user.email "user@example.com"

2.  Local repository setup

  git clone /home/wrf.git

3.  Branch setup (substitute <branch> with what you 
                  want to name your branch)

  git checkout -b <branch> origin/master
  git push origin <branch>:<branch>

4.  "cvs update" (updating the branch called <branch>)

  git checkout <branch>
  git pull 

5.  "cvs commit" (committing <branch> to the shared repository)

  git commit -a
  git push origin <branch>:<branch>

***********************************
*Explanation of the commands above*
***********************************
  
1.  Initial setup
  
  git requires that each user be identified with a name and email, set
  these with:

    git config --global user.name "FirstName LastName"
    git config --global user.email "user@example.com"

  or edit ~/.gitconfig

2.  Local repository setup

  The main development repository is located on wf.cudenver.edu at

    /home/wrf.git

  It can be accessed from other computers by instead refering to 

    ssh://wf.cudenver.edu/home/wrf.git

  To "checkout" this repository, use the clone command

    git clone /home/wrf.git

  This will create a directory ./wrf with the files. The cloned repository
  will be created in the hidden directory ./wrf/.git 

3.  Branches

  Our shared repository contains several branches, by default you will
  checkout the "master" branch.  This is meant to be the stable branch.
  To see other branches available

    git branch -r

  Among others, you should see 'origin/jbeezley'.  To use this branch,

    git checkout --track -b jbeezley origin/jbeezley

  This will create a local branch, jbeezley, that will automatically be
  updated by the remote branch, jbeezley, on the shared repository.

  You should create your own branch at this point, (substitute <branch name>
  with what you wish to name your branch).

    git checkout -b <branch name> origin/master
    git push origin <branch name>:<branch name>

  The first command creats a local branch starting from "master".  The 
  second command commits (pushes) this branch to the shared repository.  To
  get a list of local branches

    git branch

  To switch to a different local branch

    git checkout <branch name>

4.  Updating the repository

  You can check for new branches in the shared repository by
  
    git remote update
    git branch -r
    
  To update a branch, change to that branch and update with

    git pull

  This is like 'cvs update'.

5.  Committing changes

  If you create any new files, you must tell git about it

    git add <new file>

  All new files in the directory tree can be added with

    git add .

  (see also 'git rm' and 'git mv') Finally, the changes can be 
  committed with 

    git commit -a

  This will only commit to your local repository.  To add your commits to
  the shared repository

    git push origin <local branch>:<destination branch>

  Because this will modify the shared repository, only use a destination
  branch which you control.  There is a way to automate this so that the
  branches don't have to be specified (the --track flag may actually take 
  care of this), but I'm not sure how this works.  For now, I suggest 
  specifying the branches to be safe.

6.  Other commands

  For a log of everything you have done in your repository, see the command
  reflog. 
  You can undo changes with reset.  The cvs style commands status, log, and
  diff also work in git.

7.  Getting help

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

    


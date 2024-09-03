This is now a meta-repo for all DevMountain daily projects, as well as notes and assorted whatevers, and more
substantial projects that didn't quite make it. I've kept individual repos for everything, which has made a bit of a
mess of my synced directories (not to mention my Github).

If anyone's considering doing the same (consolidating repos, whether that's devmtn/school related or otherwise) but is
concerned about losing commit history/logs/whatever, here's how I did it:

Started a new (empty) repo with an initial commit (this readme). I should've written a script to do the next bit for me,
but didn't think of it until I was halfway done, and I'm stubborn.

First I added a repo. In my case, `git remote add -f 11a https://github.com/zacanger/11a/git`. Obviously adjust for your
own repos. Then, `git pull 11a master`, moved those files into a new directory (at week1/11a). `git add -A`, `git commit
-am 'week one, day one, very first project, yay'`.  Repos with multiple branches were pulled twice and put into their
own subdirectories (fortunately most were a single branch). After that, `git remote add -f
https://github.com/zacanger/11b.git`, and so on.

From now, all these tiny exercises will be done straight in here.

The 'big' projects are in their own repos:
  - [markvi](https://github.com/zacanger/markvi.git)
  - [ayuba](https://github.com/zacanger/ayuba.git)
  - [pharaoh.js](https://github.com/pharaoh-js)

And assorted repos full of samples, boilerplates, and practices, are broken out into their own repos,
by technology or language.


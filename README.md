<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Introduction</a></li>
<li><a href="#sec-2">2. How To</a></li>
<li><a href="#sec-3">3. Installation</a></li>
</ul>
</div>
</div>

# Introduction<a id="sec-1" name="sec-1"></a>

Emacs is a great text editor and you can read up on it on the Internet. 
But "With great power comes great responsibility" =) 
So, if you want to feel a full power of Emacs, you'll need to customise it yourself. 
And I just want to help by sharing my own configuration. Ok, let's start!

# How To<a id="sec-2" name="sec-2"></a>

So, my configuration has next features(you can add/remove/modify as you want):
-   a handy way to find something in Emacs enviroment([helm](https://github.com/emacs-helm/helm));
-   working with projects([projectile](https://github.com/bbatsov/projectile), [helm-projectile](https://github.com/bbatsov/helm-projectile));
-   a fast and awesome search in buffers([helm-swoop](https://github.com/ShingoFukuyama/helm-swoop));
-   a source code navigation([gtags](https://www.gnu.org/software/global/), [helm-gtags](https://github.com/syohex/emacs-helm-gtags), [sr-speedbar](https://github.com/emacsmirror/sr-speedbar));
-   on-the-fly syntax checking([flycheck](https://github.com/flycheck/flycheck), [helm-flycheck](https://github.com/yasuyk/helm-flycheck));
-   dealing with parentheses([smartparens](https://github.com/Fuco1/smartparens));
-   incredible packages for working with VCS(mostly git):
    -   git porcelain([magit!!!](https://github.com/magit/magit) - it's really a cool stuff);
    -   git-svn extension([magit-svn!!!](https://github.com/magit/magit-svn));
    -   highlighting uncommitted changes([diff-hl](https://github.com/dgutov/diff-hl));
-   text completion([company](https://github.com/company-mode/company-mode)):
    -   also, smart completion for C++, C([irony](https://github.com/Sarcasm/irony-mode));
-   a code snippets([yasnippet](https://github.com/joaotavora/yasnippet));
-   keeping notes, maintaining TODO lists, planning projects([org](http://orgmode.org/));
-   a Common Lisp IDE([slime](https://github.com/slime/slime));
-   and finally [cool color theme](https://github.com/cpaulik/emacs-material-theme), working with whitespaces and other 
    small useful features.

A little bit scary, yeah? =)
Ok, read this to find out how it works:
-   [A Package in a league of its own: Helm](http://tuhdo.github.io/helm-intro.html);
-   [Exploring large projects with Projectile and Helm Projectile](http://tuhdo.github.io/helm-projectile.html);
-   [C/C++ Development Environment for Emacs](http://tuhdo.github.io/c-ide.html);
-   [Emacs as C++ IDE](http://syamajala.github.io/c-ide.html).

Of course, don't forget to visit github pages of packages(links above).
At last, read about [use-package](https://github.com/jwiegley/use-package) package. I'm using it in my configuration files. 
It's nice :)

# Installation<a id="sec-3" name="sec-3"></a>

1.  [Install Emacs](https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Emacs.html)(I use versions 24.4-24.5);
2.  Install [GNU Global](https://www.gnu.org/software/global/):
    1.  Install [python](https://www.python.org/);
    2.  Install [ctags](http://ctags.sourceforge.net/);
    3.  Install [pygments](http://pygments.org/) package;
    4.  Enable pygments plugin: 
        Add enviroment variables to '.profile'(or whatever you use):
        export GTAGSCONF=/usr/local/share/gtags/gtags.conf
        export GTAGSLABEL=pygments
        You can find a more detailed guide at 'global-x.x.x/plugin-factory/PLUGIN<sub>HOWTO</sub>.pygments'.
3.  Generate gtags for external libraries(you can do it later):
    1.  Add enviroment variable to '.profile'(or whatever you use) =):
        export GTAGSLIBPATH=$HOME/.gtags/
    2.  Create database:
        mkdir ~/.gtags
        ln -s <LIBRARY PATH1> <NAME1>
        ln -s <LIBRARY PATH2> <NAME2>
        gtags -c
4.  Get my configuration:
    Clone to your Emacs directory 'git clone <https://github.com/ANDRON94/emacs-setup.git> .'
5.  Run Emacs :)
    [use-package](https://github.com/jwiegley/use-package) download and setup all packages automatically.

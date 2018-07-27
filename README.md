# Introduction
semantic-php is an experimental package for GNU Emacs aiming to improve the level of support for PHP in the [Semantic](http://cedet.sourceforge.net/semantic.shtml) framework. It contains a from-scratch LALR parser based on the PHP7 grammar, an implementation of the semantic API and a test suite.

The ultimate objective of semantic-php is to make the support for PHP in semantic on par with the support for C/C++.

# State of affairs
This package is currently only useful for (elisp) developers. It's very much a work in progress.

The parser has not seen a lot of real-world testing but is an improvement on the existing parser in several ways. The real challenge though is not extracting information from source files but implementing the semantic API to do something useful with that information.

Semantic does a lot of smart things out-of-the-box and allows language implementors to override specific parts of the API to accomodate new or different behaviour. But although it offers this generic (language-neutral) API, semantic is biased towards the target languages it supports - most notably the first-class citizens: C/C++. And not all differences between PHP and the target languages can be overcome by simply implementing the provided overrides. This repository currently contains no major advances in that area. Yet.

Some specific examples of the difficulties described above are:

* namespaces without brace blocks are ubiquitous in PHP, but a completely new concept in Semantic. I've tried several approaches but all have their own disadvantage. I think we can find a satisfactory solution short-term, and ideally propose changes to semantic to allow implementing this cleanly.

* use statements have very different semantics (pun intended) compared to "using" and "namespace X = Y" in C++, using the c.el overrides for this is a pitfall

* namespaces in PHP are not hierarchical, simply splitting each identifier by namespace separator and then let semantic do its tricks will not work

* ...

# Plan de campagne

Milestone #1: implement namespaces without braces, use statements and name resolution rules

* document and discuss problems and possible solutions for each of these features
* implement the best override or workaround for each feature
* work with CEDET to discuss new overrides or find better implementations

When this is implemented, context parsing works without custom semanticdb backend for all single-file PHP sources or those that use require/include directives. For other sources (those that you're actually working on) ede-php-autoload must be used (or a backend ctags/phptags-like backend). I'd like to note that there are quicker routes to get some features working (like skipping context analysis, and using custom routines to resolve class names, etc) but I'm still very determined to leverage the semantic API to the fullest.

Milestone #2+:

* implement traits and trait precedence
* extract type information from annotations
* decide on what parser we'd like to ship in semantic-php (I'm open for any discussion; maybe my rewrite isn't the best and it's better to simply go for the java-based/wisent-php parser)
* get lots of ideas in the issue tracker!

# Requirements
* Emacs 24.4 or higher
* [php-mode](https://github.com/ejmr/php-mode) (any version)

# Suggested packages
* [ede-php-autoload](https://github.com/stevenremot/ede-php-autoload)
* [auto-complete](http://github.com/auto-complete/auto-complete)
* ...

# Installation
This package is not yet available in a package archive. To install it from source:

* clone this repository
* run `make dist` to generate the parser and the autoload definitions
* run `make test` to ensure everything works as intended

Then add semantic-php to your load path and load the autoload definitions:

```
(add-to-list 'load-path "~/.emacs.d/custom/semantic-php")
(load "~/.emacs.d/custom/semantic-php/loaddefs.el")
```

Now when you enable semantic-mode in a php-mode buffer, semantic-php is automatically loaded.

# Related projects
Several people have been working on similar projects, I'll list them in a nutshell.

* [wisent-php](http://sourceforge.net/p/cedet/git/ci/master/tree/contrib/wisent-php.el) is included in CEDET as a contributed package. It is not shipped wit Emacs. You'd have to install CEDET from source and enable the contrib packages. This version of wisent-php provides a PHP5-ish parser and does not implement the semantic API (so, limited context analysis). The parser is a modified copy of the wisent java parser. Don't use this.

* Steven Rémot [forked CEDET](https://bitbucket.org/stevenremot/cedet/) to improve on wisent-php and make better use of use-statements. See ede-php-autoload below.

* Steven Rémot also created [ede-php-autoload](https://github.com/stevenremot/ede-php-autoload) which is basically a composer.json parser that can find a file defining a given type name. It can be used in an unlimited number of ways, for example:

* Andrea Tursos [xref implementation](https://github.com/ejmr/php-mode/issues/256#issuecomment-114227890): Emacs 25 drops `find-tag` in favor of `xref`. This xref implementation uses ede-php-autoload to find the files defining a given tag. Not based on semantic.

* There's a version of [EDEP](https://github.com/jorissteyn/edep/tree/multiple-db-backends) that works with ede-php-autoload: It works. But don't use it.

* [EDEP](https://github.com/jorissteyn/edep) is a precursor to this repository. It does not implement the semantic API very well, but instead uses some tricks to allow source code navigation using [PHPTAGS](https://github.com/jorissteyn/phptags). As a result, navigation works well but context sensitive completion does not. The current master branches of the two repositories work well together. There will be no further development on EDEP.

# Configuration snippets
You might also be interested in configuring auto-complete mode with semantic completions:

```
(require 'auto-complete-config)
(setq-default ac-sources '(ac-source-semantic))
```

And to automatically enable semantic-mode in all php-mode buffers:

```
(add-hook 'php-mode-hook #'semantic-mode)
```

To use ede-php-autoload, check the projects readme. For example:

```
(require 'ede)
(global-ede-mode t)

(add-to-list 'load-path "~/.emacs.d/custom/ede-php-autoload")
(require 'ede-php-autoload-mode)
(add-hook 'php-mode-hook #'ede-php-autoload-mode)
(ede-php-autoload-project "Symfony" :file "/path/to/project/using/composer/composer.json")
```

For projects using ZF1-style require/include statements, use ede-cpp-root-project until ede-php-autoload can handle that as well:

```
(ede-cpp-root-project "ZF1"
                      :file "/path/to/old/school/project/composer.json"
                      :include-path '("/library")
                      :system-include-path '("/usr/share/php"))
```

Note that the :file attribute in both ede projects need to point to an existing file, the file is not used in any way except to deduce the project root directory.

# Tests and utilities
* Run `make test` in the edep source directory to invoke the test suite.
* Run `make autoload` to generate the autoload definitions.
* Run `make parser-create` to regenerate the parser from the wisent grammar.
* Run `make parser-test PARSE_PATH=/path/to/lots/of/source/files` to feed arbitrary source files to the wisent parser.
* Run `make compile` byte-compile the lisp source files.

# License
    Copyright 2014-2015 Joris Steyn

    semantic-php is not part of GNU Emacs.

    This file is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 3
    of the License, or (at your option) any later version.

    This file is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this file; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
    02110-1301, USA.


Clacman
=======

[![License MIT][badge-license]][LICENSE]
[![GitHub version](https://badge.fury.io/gh/nlamirault%2Fclacman.svg)](https://badge.fury.io/gh/nlamirault%2Fclacman)


Master :
* [![Circle CI](https://circleci.com/gh/nlamirault/clacman/tree/master.svg?style=svg)](https://circleci.com/gh/nlamirault/clacman/tree/master)

Develop:
* [![Circle CI](https://circleci.com/gh/nlamirault/clacman/tree/develop.svg?style=svg)](https://circleci.com/gh/nlamirault/clacman/tree/develop)

This is the classic [Pacman](http://en.wikipedia.org/wiki/Pacman) game in Common Lisp.

## Required softwares

You will need :

* [SBCL][]
* [Quicklisp][]

and dependencies :

    $ sudo apt-get install libsdl1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev

## Installation

* Install tools and dependencies :

        $ make init
        $ make deps

* Make binary :

        $ make binary

* Launch game :

        $ roswell/clacman


## Commands

Available commands :

* s : Start a new game
* Left : Move the current block to the left
* Right : Move the current block to the right
* Up : Rotate the current block
* Down : Move the current block down
* Space : Move the current block all the way down.
* p : Pause game
* v : View score (when user not playing a game).
* q : Quit clacman


## Development

* Install [roswell][] to setup the Common Lisp environment and install your Common Lisp implementation using it.:

        $ make init
        $ ros install sbcl

* Install dependencies :

        $ make deps

* Launch unit tests :

        $ make test

## Support / Contribute

See [here](CONTRIBUTING.md)


## Changelog

A changelog is available [here](ChangeLog.md).


## License

See [LICENSE](LICENSE).


## Contact

Nicolas Lamirault <nicolas.lamirault@gmail.com>



[sbcl]: http://www.sbcl.org
[quicklisp]: http://www.quicklisp.org
[roswell]: https://github.com/snmsts/roswell

[clacman]: https://github.com/nlamirault/clacman
[badge-license]: https://img.shields.io/badge/license-MIT-green.svg?style=flat
[LICENSE]: https://github.com/nlamirault/clacman/blob/master/LICENSE

[Issue tracker]: https://github.com/nlamirault/clacman/issues

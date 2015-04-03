# eopengrok.el [![Build Status](https://secure.travis-ci.org/magnars/eopengrok.el.png)](http://travis-ci.org/magnars/eopengrok.el)

opengrok interface for emacs

## Installation

I highly recommend installing eopengrok through elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.milkbox.net/):

    M-x package-install eopengrok

You can also install the dependencies on your own, and just dump
eopengrok in your path somewhere:

 - <a href="https://github.com/magnars/s.el">s.el</a>
 - <a href="https://github.com/magnars/dash.el">dash.el</a>

## Contribute

Yes, please do. :-)

All changes must be accompanied by feature tests, or I might break it later.
They are written in [Ecukes](http://ecukes.info), a Cucumber for Emacs.

You'll find the repo at:

    https://github.com/magnars/eopengrok.el

To fetch the test dependencies, install
[carton](https://github.com/rejeep/carton) if you haven't already,
then:

    $ cd /path/to/eopengrok
    $ carton

Run the tests with:

    $ ./run-tests.sh

## License

Copyright (C) 2015 Youngjoo Lee

Author: Youngjoo Lee <youngker@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

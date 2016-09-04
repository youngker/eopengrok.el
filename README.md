# eopengrok.el [![MELPA](http://melpa.org/packages/eopengrok-badge.svg)](http://melpa.org/#/eopengrok)

opengrok interface for emacs

## ScreenShot

* fulltext
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/img/fulltext.png">

* history
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/img/history.png">

* custom 1
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/img/custom1.png">

* custom 2
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/img/custom2.png">

## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install eopengrok

## Requirements

* [clj-opengrok](https://github.com/youngker/clj-opengrok) : download [here](https://github.com/youngker/clj-opengrok/releases)

You can add these lines to your init file.

```elisp
(require 'eopengrok)

(define-key global-map (kbd "C-c s i") 'eopengrok-create-index)
(define-key global-map (kbd "C-c s I") 'eopengrok-create-index-with-enable-projects)
(define-key global-map (kbd "C-c s d") 'eopengrok-find-definition)
(define-key global-map (kbd "C-c s f") 'eopengrok-find-file)
(define-key global-map (kbd "C-c s s") 'eopengrok-find-reference)
(define-key global-map (kbd "C-c s t") 'eopengrok-find-text)
(define-key global-map (kbd "C-c s h") 'eopengrok-find-history)
(define-key global-map (kbd "C-c s c") 'eopengrok-find-custom)
(define-key global-map (kbd "C-c s b") 'eopengrok-resume)
```

## Key bindings

Key | Function
--- | --------
<kbd>n</kbd> | eopengrok-next-line
<kbd>p</kbd> | eopengrok-previous-line
<kbd>q</kbd> | eopengrok-quit

## License

Copyright (C) 2016 Youngjoo Lee

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

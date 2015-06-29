# eopengrok.el

opengrok interface for emacs

## ScreenShot

- **reference**
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/eopengrok.png">

- **git history**
<img align="center" src="https://raw.github.com/youngker/eopengrok.el/master/eopengrok-git.png">

## Installation

Requirements

- **Java 1.7**

- **Exuberant ctags**
  [http://ctags.sourceforge.net](http://ctags.sourceforge.net)

- **Opengrok**
  Latest release from [https://github.com/OpenGrok/OpenGrok/releases](https://github.com/OpenGrok/OpenGrok/releases)

Edit eopengrok.el

```elisp
(defcustom eopengrok-jar
  "/Users/youngker/Projects/opengrok-0.12.1.5/lib/opengrok.jar"
  :group 'eopengrok)

(defcustom eopengrok-ctags
  "/usr/local/bin/ctags"
  :group 'eopengrok)
```

Add your .el
```elisp
(define-key global-map (kbd "C-c s d") 'eopengrok-find-definition)
(define-key global-map (kbd "C-c s f") 'eopengrok-find-file)
(define-key global-map (kbd "C-c s s") 'eopengrok-find-reference)
(define-key global-map (kbd "C-c s t") 'eopengrok-find-text)
(define-key global-map (kbd "C-c s h") 'eopengrok-find-history)
(define-key global-map (kbd "C-c s I") 'eopengrok-index-files)
```

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

Clando
======

## Introduction

Clando is a command-line todo list manager for minimalists. 

Its main idea is based on [t](http://stevelosh.com/projects/t/), which aims to maximize efficiency by making things simple.


## Basic Usage

To add a task, use `clando add <description>`

    $ clando add Finish coursework of AI class
    $ clando add Implement a LISP interpreter

To list your pending tasks, simply type in `clando`

    $ clando
    6 - Finish coursework of AI class
    8 - Implement a LISP interpreter

(The numbers at the front are the IDs of each task. )

To mark a pending task as finished, use `clando finish <id>`

    $ clando finish 6

To edit the description of a task, use `clando edit <id> <description>`

    $ clando edit 8 Implement a LISP interpreter in Javascript

To remove a task, use `clando remove <id>`

    $ clando remove 2


## Advanced Usage

Feeling tedious to input the long `clando` in the terminal each time? Just choose any letter in A-Z as the alias for `clando` and add it to your `.bashrc` file.

    alias c='clando'

Remember to execute `. ~/.bashrc` or restart your terminal for it to take effect.

Clando conforms to the UNIX philosophy. There are no redundant outputs, and therefore you may use tools such as `grep` and `wc` to filter or count tasks.

    $ clando | grep -i "book" | wc -l  # count the number of all tasks having "book" in their descriptions


## Installation

Clando is written in Common Lisp and depends on `sbcl`. You may install `sbcl` from your Linux distribution's sofware repository.

For Ubuntu users:

    sudo apt-get install sbcl
    
For ArchLinux users:

    sudo pacman -S sbcl

Then,

    git clone git@github.com:qiao/clando.git
    cd clando
    make
    sudo make install

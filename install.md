soft intall on ubuntu linux
===========================

1. install nodejs on ubuntu linux

    ```bash
    $ curl -sL https://deb.nodesource.com/setup | sudo bash -
    $ sudo apt-get update
    $ sudo apt-get install nodejs
    ```

2. vim markdown preview tool
    
    ```bash
    $ npm -g install instant-markdown-d
    $ mkdir -p ~/.vim/after/ftplugin/markdown/
    $ cd ~/.vim/after/ftplugin/markdown
    $ wget https://raw.githubusercontent.com/suan/vim-instant-markdown/master/after/ftplugin/markdown/instant-markdown.vim
    ```

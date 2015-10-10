ubuntu mac theme
================

1. theme 

    ```bash
    $ sudo add-apt-repository ppa:noobslab/themes
    $ sudo apt-get update
    $ sudo apt-get install mbuntu-y-ithemes-v4
    $ sudo apt-get install mbuntu-y-icons-v4
    ```


2. Indicator Synapse and Mutate (Alternative to Spotlight)

    ```bash
    $ sudo add-apt-repository ppa:noobslab/apps
    $ sudo apt-get update
    $ sudo apt-get install indicator-synapse
    ```


3. Set themes and icons:
    
    ```bash
    $ cd && wget -O config.sh http://drive.noobslab.com/data/Mac-14.10/config.sh
    $ chmod +x config.sh;./config.sh
    ```


4. Apply MBuntu Splash:

    ```bash
    $ sudo add-apt-repository ppa:noobslab/themes
    $ sudo apt-get update
    $ sudo apt-get install mbuntu-y-bscreen-v4
    ```


5. Replace 'Ubuntu Desktop' text with 'Mac' on the Panel

    ```bash
    $ cd && wget -O Mac.po http://drive.noobslab.com/data/Mac-14.10/change-name-on-panel/mac.po
    $ cd /usr/share/locale/en/LC_MESSAGES; sudo msgfmt -o unity.mo ~/Mac.po;rm ~/Mac.po;cd
    ```



6. Remove White Dots and Ubuntu Logo from Lock Screen:

    ```bash
    $ sudo xhost +SI:localuser:lightdm
    $ sudo su lightdm -s /bin/bash
    $ gsettings set com.canonical.unity-greeter draw-grid false;exit
    $ sudo mv /usr/share/unity-greeter/logo.png
    $ /usr/share/unity-greeter/logo.png.backup
    $ cd;wget -O logo.png
    $ http://drive.noobslab.com/data/Mac-14.10/ubuntu_logo.png
    $ sudo mv logo.png /usr/share/unity-greeter/;gsettings set
    $ com.canonical.unity-greeter draw-grid false
    ```



7. Apple Logo in Launcher

    ```bash
    $ wget -O launcher_bfb.png http://drive.noobslab.com/data/Mac-14.10/launcher-logo/apple/launcher_bfb.png
    $ sudo mv launcher_bfb.png /usr/share/unity/icons/
    ```


8. Unity Tweak Tool to change Themes & Icons:

    ```bash
    $ sudo apt-get install unity-tweak-tool
    ```



9. (Optional) Mac fonts:

    ```bash
    $ wget -O mac-fonts.zip http://drive.noobslab.com/data/Mac-14.10/macfonts.zip
    $ sudo unzip mac-fonts.zip -d /usr/share/fonts; rm mac-fonts.zip
    $ sudo fc-cache -f -v
    ```


10. (Optional) Install MacBuntu theme for LightDM Webkit:

    ```bash
    $ sudo add-apt-repository ppa:noobslab/themes
    $ sudo apt-get update
    $ sudo apt-get install mbuntu-y-lightdm-v4
    ```


11. mac monaco font:

    ```bash
    $ curl -kL https://raw.github.com/cstrap/monaco-font/master/install-font-ubuntu.sh | bash
    ```

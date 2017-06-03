The lab
=======

Most of the demos listed below are written in `CHICKEN Scheme`_.

brause.cc_
----------

`Animated CSS3 pixel art`_.  The effect is created by abusing the
``box-shadow`` CSS attribute to attach a multitude of blocky shadows
to the ``<div>`` container and animating its value.

brause.cc/*
-----------

Ersatz-Dropbox_.  I really like the permalink feature of Dropbox_, so I
decided to recreate it with a `rsync script`_.  To get the links, I
use a `Ruby script`_.

braune.brause.cc_
-----------------

`Random quotes`_ by `Axel Stoll`_.

fette.brause.cc_
----------------

Dynamically generated atom feed for `a certain burger restaurant`_.
Consists of `a binary scraping contents`_ and `a binary serving them`_.

brot.brause.cc_
---------------

Atom feed for breadfaceblog_.  Surprisingly enough, dissecting
Instagram_ websites only involves a JSON parser after finding the blob
they're generated from.

elpa.brause.cc_
---------------

`Statically generated atom feeds`_ for new `ELPA packages`_.  Highly
useful to keep track of new Emacs packages to brag about!

c4.brause.cc_
-------------

Status page for the CCCC_ hackerspace, based on `their official API`_.
If you find this information useful, use the API instead of scraping
my page or you'll get blocked.

.. _CHICKEN Scheme: http://call-cc.org/
.. _brause.cc: http://brause.cc/
.. _Animated CSS3 pixel art: https://github.com/wasamasa/brause.cc/blob/master/index.html
.. _Ersatz-Dropbox: http://brause.cc/dealwithit.jpg
.. _Dropbox: https://www.dropbox.com/
.. _rsync script: https://github.com/wasamasa/dotfiles/blob/master/home/wasa/bin/fallkiste
.. _Ruby script: https://github.com/wasamasa/dotfiles/blob/master/home/wasa/bin/permalink
.. _braune.brause.cc: http://braune.brause.cc/
.. _Random quotes: https://github.com/wasamasa/brause.cc/blob/master/stoll/stoll.scm
.. _Axel Stoll: https://en.wikipedia.org/wiki/Axel_Stoll
.. _fette.brause.cc: http://fette.brause.cc/
.. _a certain burger restaurant: http://fettekuh.de/
.. _a binary scraping contents: https://github.com/wasamasa/brause.cc/blob/master/kuh/kuh.scm
.. _a binary serving them: https://github.com/wasamasa/brause.cc/blob/master/kuh/fette.scm
.. _brot.brause.cc: http://brot.brause.cc/
.. _breadfaceblog: https://www.instagram.com/breadfaceblog/
.. _Instagram: https://www.instagram.com/
.. _elpa.brause.cc: http://elpa.brause.cc/
.. _Statically generated atom feeds: https://github.com/wasamasa/brause.cc/blob/master/elpa/elpa.scm
.. _ELPA packages: https://github.com/wasamasa/brause.cc/blob/master/elpa/elpa.scm
.. _c4.brause.cc: http://c4.brause.cc/
.. _CCCC: https://koeln.ccc.de/
.. _their official API: https://api.koeln.ccc.de/

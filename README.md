Based on:

- https://ocharles.org.uk/blog/pages/2012-12-01-24-days-of-hackage.html
- https://ocharles.org.uk/blog/pages/2013-12-01-24-days-of-hackage.html
- https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html
- http://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/

Examples:

- `ghci`:
    - `stack ghci doh2013:exe:linear`
- `ghcid`:
    - `ghcid -c 'stack ghci --ghci-options="-fno-code -ferror-spans" doh2013:exe:linear'`
- run:
    - `stack exec exe-20 2012/data/users.txt Bob`

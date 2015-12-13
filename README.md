autoenc: Clean implementation of sparse autoencoders in R
===========

Implementation based off [this article](http://web.stanford.edu/class/archive/cs/cs294a/cs294a.1104/sparseAutoencoder.pdf).

The goal is to have a very readable and well-tested implementation, so that autoencoders can be used in production and trusted.

The existing CRAN [implementation](https://github.com/cran/autoencoder) of autoencoders has no test coverage and is hard to follow, so auditing this package for any serious use case would involve spending quite a bit of time. Autoenc package is designed with code readability in mind, following best functional programming practices, in order to make it comfortable for others to use and enhance this package.

# Quadpack

## Information
This repository contains the original `quadpack` from netlib.org, available as a _fpm_ package.

## About
Quadpack is a Fortran 77 library for numerical integration of one-dimensional functions.

## Origin
[http://www.netlib.org/quadpack/](http://www.netlib.org/quadpack/)

## fpm
To use quadpack within your fpm project, add the following to `fpm.toml` file:

```toml
[dependencies]
quadpack = {git = "https://github.com/brocolis/quadpack" }
```

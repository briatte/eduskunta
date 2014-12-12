This repository contains code to build cosponsorship networks from bills (and motions) passed in the [Finnish Parliament](http://www.eduskunta.fi/).

- [interactive demo](http://briatte.org/eduskunta)
- [static plots](http://briatte.org/eduskunta/plots.html)

For related work, see the [`finpar`](https://github.com/rOpenGov/finpar) package.

# HOWTO

Replicate by running `make.r` in R.

The `data.r` script downloads information on bills and sponsors. All photos should download fine.

The `build.r` script then assembles the edge lists and plots the networks, with the help of a few routines coded into `functions.r`. Adjust the `plot`, `gexf` and `mode` parameters to skip the plots or to change the node placement algorithm.

# DATA

## Bills

- `url` -- bill URL
- `year` -- year of introduction
- `authors` -- semicolon-separated sponsor names
- `n_au` -- total number of sponsors
- `legislature` -- legislature number (35 or 36)

## Sponsors

- `profile_url` -- URL to access the sponsor profile
- `url` -- alternative profile URL (see below)
- `year` -- year of entry in parliament
- `photo_url` -- photo URL
- `name` -- sponsor name (same as in bills data)
- `born` -- year of birth
- `party` -- political party, abbreviated
- `partyname` -- political party, full name
- `party_length` -- number of political parties the sponsor has belonged to
- `mandate` -- semicolon-separated years of mandate, used to compute the `nyears` seniority variable
- `sex` -- gender (F/M), imputed from first names
- `photo` -- photo URL, simplified to its filename

The Eduskunta website uses a _very_ strange URL system: two URLs are necessary to scrape the sponsors, which explains the four different URL variables (two for the sponsor profile, one for the original photo URL and one for the shortened version).

# CREDITS

Thanks to Leo Lahti, Joona Lehtomäki and Juha Yrjölä for pointers on how to impute gender from Finnish names: see [this discussion](https://github.com/rOpenGov/finpar/issues/2) and [this discussion](https://github.com/kansanmuisti/kamu/issues/134).

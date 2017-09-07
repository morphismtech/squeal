# squeal

![squeal-icon](http://www.emoticonswallpapers.com/emotion/cute-big-pig/cute-pig-smiley-046.gif)

[![CircleCI](https://circleci.com/gh/echatav/squeal.svg?style=svg&circle-token=a699a654ef50db2c3744fb039cf2087c484d1226)](https://circleci.com/gh/echatav/squeal)

Main repository for the squeal database library.

## installation

The easiest way to install `squeal-postgresql` is to use the haskell [stack](https://docs.haskellstack.org/en/stable/README/) tool.

Use `resolver: nightly-2017-08-25`. We're stuck with nightlies until an lts supports GHC-8.2. Do a `stack upgrade && stack update` to make sure you get GHC-8.2 and the nightly. Then add `squeal-postgresql-0.1.1.2` as an extra dependency in your `stack.yaml` and you should be good to go.

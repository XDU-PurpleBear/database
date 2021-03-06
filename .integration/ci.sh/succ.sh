#!/bin/bash
# stop when error
set -e

echo skip pull request
if [ x"$TRAVIS_PULL_REQUEST" == "xfalse" ]; then
  echo set up tag
  if [ -n "$TRAVIS_TAG" ]; then
    export GIT_TAG=$TRAVIS_TAG
  else
    export GIT_TAG=$TRAVIS_BRANCH-${TRAVIS_COMMIT:0:7}
  fi

  echo clean stack build
  stack clean

  if [ -n "$IS_DOCKER" ]; then
    echo build docker image
    echo
    echo create folder
    cd $TRAVIS_BUILD_DIR
    mkdir -p docker.tmp/bin
    echo build pb-auth and pb-logger  image
    cd $TRAVIS_BUILD_DIR
    stack install pb-auth pb-logger pb-isbn --ghc-options -O2 --ghc-options -threaded
    export GIT_TAG=`echo $GIT_TAG | sed 's/\//-/g'`
    export LATEST=latest
    export PB_AUTH_IMAGE_TAG=pb-auth
    export PB_LOGGER_IMAGE_TAG=pb-logger
    export PB_ISBN_IMAGE_TAG=pb-isbn
    export PGSQL_IMAGE_TAG=pgsql
    echo copy files
    sudo cp $HOME/.local/bin/pb-auth   docker.tmp/bin
    sudo cp $HOME/.local/bin/pb-logger docker.tmp/bin
    sudo cp $HOME/.local/bin/pb-isbn   docker.tmp/bin
    sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pb-auth.dockerfile   docker.tmp
    sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pb-logger.dockerfile docker.tmp
    sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pb-isbn.dockerfile   docker.tmp
    cd docker.tmp
    docker build -t qinka/pb-database:$PB_AUTH_IMAGE_TAG-$GIT_TAG   -f pb-auth.dockerfile   . || true
    docker build -t qinka/pb-database:$PB_LOGGER_IMAGE_TAG-$GIT_TAG -f pb-logger.dockerfile . || true
    docker build -t qinka/pb-database:$PB_ISBN_IMAGE_TAG-$GIT_TAG   -f pb-isbn.dockerfile   . || true
    docker tag qinka/pb-database:$PB_AUTH_IMAGE_TAG-$GIT_TAG   qinka/pb-database:$PB_AUTH_IMAGE_TAG-$LATEST    || true
    docker tag qinka/pb-database:$PB_LOGGER_IMAGE_TAG-$GIT_TAG qinka/pb-database:$PB_LOGGER_IMAGE_TAG-$LATEST  || true
    docker tag qinka/pb-database:$PB_ISBN_IMAGE_TAG-$GIT_TAG   qinka/pb-database:$PB_ISBN_IMAGE_TAG-$LATEST    || true
    echo build PostgreSQL image
    cd $TRAVIS_BUILD_DIR
    echo copy files
    sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pgsql.dockerfile docker.tmp
    sudo cp $TRAVIS_BUILD_DIR/sql/initialization.sql docker.tmp
    cd docker.tmp
    docker build -t qinka/pb-database:$PGSQL_IMAGE_TAG -f pgsql.dockerfile . || true
    echo push docker images
    docker push qinka/pb-database || true
  else
    echo skip building docker image
  fi

fi

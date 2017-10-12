#!/bin/bash
# stop when error
set -e

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
  stack install pb-auth pb-logger --ghc-options -O2 --ghc-options -threaded
  export PB_AUTH_IMAGE_TAG=pb-auth-$GIT_TAG
  export PB_AUTH_IMAGE_TAG=pb-logger-$GIT_TAG
  export PGSQL_IMAGE_TAG=pgsql-$GIT_TAG
  echo copy files
  sudo cp $HOME/.local/bin/pb-auth   docker.tmp/bin
  sudo cp $HOME/.local/bin/pb-logger docker.tmp/bin
  sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pb-auth.dockerfile   docker.tmp
  sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pb-logger.dockerfile docker.tmp
  cd docker.tmp
  docker build -t ??/??:$PB_AUTH_IMAGE_TAG   -f pb-auth.dockerfile   .
  docker build -t ??/??:$PB_LOGGER_IMAGE_TAG -f pb-logger.dockerfile .
  echo build PostgreSQL image
  cd $TRAVIS_BUILD_DIR
  echo copy files
  sudo cp $TRAVIS_BUILD_DIR/.integration/dockerfiles/pgsql.dockerfile docker.tmp
  sudo cp $TRAVIS_BUILD_DIR/sql/initialzation.sql                     docker.tmp
  docker build -t ??/??:$PGSQL_IMAGE_TAG -f pgsql.dockerfile .
  echo push docker images
  docker push ??/??
else
  echo skip building docker image
fi


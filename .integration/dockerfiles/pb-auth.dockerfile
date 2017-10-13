# pb auth docker file
FROM debian:latest
MAINTAINER Johann Lee <me@qinka.pro>
COPY bin/pb-auth /bin
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000

# pb logger docker file
FROM debian:latest
MAINTAINER Johann Lee <me@qinka.pro>
COPY bin/pb-logger /bin
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000

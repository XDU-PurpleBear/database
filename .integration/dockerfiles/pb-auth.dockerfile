# pb auth docker file
FROM debian:latest
MAINTAINER Johann Lee <me@qinka.pro>
RUN apt update && apt install -y libgmp10
COPY bin/pb-auth /bin
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000

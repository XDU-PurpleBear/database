# pb pgsql docker file
FROM postgres:latest
MAINTAINER Johann Lee <me@qinka.pro>
COPY initialization.sql /
RUN psql -f /initialization.sql
ENTRYPOINT ["/bin/bash"]
EXPOSE 3000

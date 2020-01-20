FROM debian

RUN mkdir /opt/skypeernet /opt/skypeernet/config
RUN apt-get update && apt-get install -y libssl-dev libev-dev libjson-c-dev libreadline-dev libcunit1-dev

WORKDIR /opt/skypeernet/
COPY spntracker spnpeer spnvalidate libskypeernettcp.so ./
COPY config/settings.cfg ./config/

CMD ["./spnpeer"]

FROM ubuntu:18.04
ENTRYPOINT ["/bin/bash", "-c"]
SHELL ["/bin/bash", "-c"]
RUN git clone https://github.com/explorable-viz/fluid.git
RUN yarn install
RUN yarn run bundle-tests
RUN yarn run tests
CMD ["/bin/bash"]
